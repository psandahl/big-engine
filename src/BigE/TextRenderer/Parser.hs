-- |
-- Module: BigE.TextRenderer.Parser
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.TextRenderer.Parser
    ( parseInfo
    , parseSpacing
    , parsePadding
    , keyValue
    ) where

import           BigE.TextRenderer.Font          (Info (..), Padding (..),
                                                  Spacing (..))
import           Control.Applicative             (empty)
import           Control.Monad                   (void)
import           Text.Megaparsec
import           Text.Megaparsec.ByteString.Lazy
import qualified Text.Megaparsec.Lexer           as Lexer

-- | Parse an 'Info' record from the stream.
parseInfo :: Parser Info
parseInfo = do
    kw "info"
    Info <$> keyValue "face" quotedString

-- | Parse a 'Spacing' record from the stream.
parseSpacing :: Parser Spacing
parseSpacing = Spacing <$> signedInt <* comma <*> signedInt

-- | Parse a 'Padding' record from the stream.
parsePadding :: Parser Padding
parsePadding =
    Padding <$> unsignedInt <* comma
            <*> unsignedInt <* comma
            <*> unsignedInt <* comma
            <*> unsignedInt

-- | Parse a keyname and a value from the stream.
keyValue :: String -> Parser a -> Parser a
keyValue key parser = do
    kw key
    assign
    parser

-- Helper parsers.

kw :: String -> Parser ()
kw = void . Lexer.symbol sc

assign :: Parser ()
assign = void $ lexeme (char '=')

comma :: Parser ()
comma = void $ lexeme (char ',')

signedInt :: Parser Int
signedInt = fromIntegral <$> lexeme (Lexer.signed sc Lexer.integer)

unsignedInt :: Parser Int
unsignedInt = fromIntegral <$> lexeme Lexer.integer

--boolean :: Parser Bool
--boolean = (/= 0) <$> unsignedInt

quotedString :: Parser String
quotedString = between (char '\"') (char '\"') (many $ noneOf ['\"'])

-- | Space consumer; either whitespaces or line comments starting with '#'.
sc :: Parser ()
sc = Lexer.space (void spaceChar) (Lexer.skipLineComment "#") empty

-- | Lexeme; parser prepended with the space consumer.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc
