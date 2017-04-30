-- |
-- Module: BigE.TextRenderer.Font
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.TextRenderer.Font
    ( Font (..)
    , fromFile
    ) where

import qualified BigE.TextRenderer.Parser   as Parser
import           BigE.TextRenderer.Types    (Character (charId), Common, Info)
import           Control.Exception          (SomeException, try)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Text.Megaparsec            (parse)

-- | A loaded font.
data Font = Font
    { info       :: !Info
    , common     :: !Common
    , characters :: !(HashMap Int Character)
    } deriving Show

-- | Read 'Font' data from file.
fromFile :: MonadIO m => FilePath -> m (Either String Font)
fromFile file = do
    eFnt <- liftIO $ readFontFromFile file
    case eFnt of
        Right fnt -> return $
            Right Font
                { info = Parser.info fnt
                , common = Parser.common fnt
                , characters =
                    HashMap.fromList $ keyValueList (Parser.characters fnt)
                }
        Left err  -> return $ Left err
    where
        keyValueList = map (\char -> (charId char, char))

-- | Get a 'FontFile' from external file.
readFontFromFile :: FilePath -> IO (Either String Parser.FontFile)
readFontFromFile file = do
    eBs <- tryRead file
    case eBs of
        Right bs ->
            case parse Parser.parseFontFile file bs of
                Right fnt -> return $ Right fnt
                Left err  -> return $ Left (show err)
        Left e -> return $ Left (show e)
    where
        tryRead :: FilePath -> IO (Either SomeException ByteString)
        tryRead = try . BS.readFile
