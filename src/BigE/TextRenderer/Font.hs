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

import           BigE.TextRenderer.Parser   (parseFontFile)
import           BigE.TextRenderer.Types    (FontFile)
import           Control.Exception          (SomeException, try)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Text.Megaparsec            (parse)

data Font = Font
    { foo :: !Int
    } deriving Show

-- | Read 'Font' data from file.
fromFile :: MonadIO m => FilePath -> m (Either String FontFile)
fromFile file = do
    eFnt <- liftIO $ readFontFromFile file
    case eFnt of
        Right fnt -> return $ Right fnt
        Left err  -> return $ Left err

-- | Get a 'FontFile' from external file.
readFontFromFile :: FilePath -> IO (Either String FontFile)
readFontFromFile file = do
    eBs <- tryRead file
    case eBs of
        Right bs ->
            case parse parseFontFile file bs of
                Right fnt -> return $ Right fnt
                Left err  -> return $ Left (show err)
        Left e -> return $ Left (show e)
    where
        tryRead :: FilePath -> IO (Either SomeException ByteString)
        tryRead = try . BS.readFile
