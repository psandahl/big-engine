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
import           BigE.TextRenderer.Types    (Character (charId), Common, Info,
                                             Page (..))
import           BigE.Texture               (TextureParameters (..),
                                             defaultParams2D)
import qualified BigE.Texture               as Texture
import           BigE.Types                 (Texture, TextureFormat (..),
                                             TextureWrap (..))
import           Control.Exception          (SomeException, try)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           System.FilePath
import           Text.Megaparsec            (parse)

-- | A loaded font.
data Font = Font
    { info       :: !Info
    , common     :: !Common
    , characters :: !(HashMap Int Character)
    , fontAtlas  :: !Texture
    } deriving Show

-- | Read 'Font' data from file.
fromFile :: MonadIO m => FilePath -> m (Either String Font)
fromFile filePath = do
    eFnt <- liftIO $ readFontFromFile filePath
    case eFnt of

        Right fnt -> do

            eFontAtlas <- readTextureFromFile filePath fnt
            case eFontAtlas of

                Right fontAtlas' ->
                    return $
                        Right Font
                            { info = Parser.info fnt
                            , common = Parser.common fnt
                            , characters =
                                HashMap.fromList $ keyValueList (Parser.characters fnt)
                            , fontAtlas = fontAtlas'
                            }

                Left err -> return $ Left err

        Left err  -> return $ Left err
    where
        keyValueList = map (\char -> (charId char, char))

-- | Get a 'FontFile' from external file.
readFontFromFile :: FilePath -> IO (Either String Parser.FontFile)
readFontFromFile filePath = do
    eBs <- tryRead filePath
    case eBs of
        Right bs ->
            case parse Parser.parseFontFile filePath bs of
                Right fnt -> return $ Right fnt
                Left err  -> return $ Left (show err)
        Left e -> return $ Left (show e)
    where
        tryRead :: FilePath -> IO (Either SomeException ByteString)
        tryRead = try . BS.readFile

readTextureFromFile :: MonadIO m => FilePath -> Parser.FontFile
                    -> m (Either String Texture)
readTextureFromFile filePath fntFile = do
    let fntDir = takeDirectory filePath
        texFile = fntDir </> file (Parser.page fntFile)
    Texture.fromFile2D texFile
        defaultParams2D { format = RGBA8
                        , wrapS = WrapClampToEdge
                        , wrapT = WrapClampToEdge
                        }
