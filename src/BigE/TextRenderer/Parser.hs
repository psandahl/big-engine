-- |
-- Module: BigE.TextRenderer.Parser
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module BigE.TextRenderer.Parser
    ( FontFile (..)
    , parseFontFile
    ) where

import           BigE.TextRenderer.Types         (Character (..), Common (..),
                                                  Info (..), Kerning (..),
                                                  Padding (..), Page (..),
                                                  Spacing (..))
import           Control.Applicative             (empty)
import           Control.Monad                   (void)
import           Text.Megaparsec
import           Text.Megaparsec.ByteString.Lazy
import qualified Text.Megaparsec.Lexer           as Lexer

-- | Representation of a file with font data. Some restrictions though:
-- Only one page fonts are supported.
-- No channel stuff.
data FontFile = FontFile
    { info         :: !Info
    , common       :: !Common
    , page         :: !Page
    , characters   :: ![Character]
    , kerningPairs :: ![Kerning]
    } deriving (Eq, Show)

-- | Parse a 'FontFile' from the stream.
parseFontFile :: Parser FontFile
parseFontFile =
    FontFile <$> parseInfo
             <*> parseCommon
             <*> parsePage
             <*> (skipCharCount *> many parseCharacter)
             <*> (optional skipKerningsCount *> many parseKerning)

-- | Parse an 'Info' record from the stream.
parseInfo :: Parser Info
parseInfo = do
    kw "info"
    Info <$> keyValue "face" quotedString
         <*> keyValue "size" unsignedInt
         <*> keyValue "bold" boolean
         <*> keyValue "italic" boolean
         <*> keyValue "charset" quotedString
         <*> keyValue "unicode" boolean
         <*> keyValue "stretchH" unsignedInt
         <*> keyValue "smooth" boolean
         <*> keyValue "aa" unsignedInt
         <*> keyValue "padding" parsePadding
         <*> keyValue "spacing" parseSpacing

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

-- | Parse a 'Common' record from the stream.
parseCommon :: Parser Common
parseCommon = do
    kw "common"
    Common <$> keyValue "lineHeight" unsignedInt
           <*> keyValue "base" unsignedInt
           <*> keyValue "scaleW" unsignedInt
           <*> keyValue "scaleH" unsignedInt
           <*> keyValue "pages" unsignedInt
           <*> keyValue "packed" boolean

-- | Parse a 'Page' record from the stream.
parsePage :: Parser Page
parsePage = do
    kw "page"
    Page <$> keyValue "id" unsignedInt
         <*> keyValue "file" quotedString

-- | Parse a 'Character' record from the stream.
parseCharacter :: Parser Character
parseCharacter = do
    kw "char"
    Character <$> keyValue "id" unsignedInt
              <*> keyValue "x" unsignedInt
              <*> keyValue "y" unsignedInt
              <*> keyValue "width" unsignedInt
              <*> keyValue "height" unsignedInt
              <*> keyValue "xoffset" signedInt
              <*> keyValue "yoffset" signedInt
              <*> keyValue "xadvance" unsignedInt
              <*> keyValue "page" unsignedInt
              <*> keyValue "chnl" unsignedInt

-- | Parse a 'Kerning' record from the stream.
parseKerning :: Parser Kerning
parseKerning = do
    kw "kerning"
    Kerning <$> keyValue "first" unsignedInt
            <*> keyValue "second" unsignedInt
            <*> keyValue "amount" signedInt

skipCharCount :: Parser ()
skipCharCount = do
    kw "chars"
    void $ keyValue "count" unsignedInt

skipKerningsCount :: Parser ()
skipKerningsCount = do
    kw "kernings"
    void $ keyValue "count" unsignedInt

-- | Parse a keyname and a value from the stream.
keyValue :: String -> Parser a -> Parser a
keyValue key parser = do
    lexeme (kw key)
    lexeme assign
    lexeme parser

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

boolean :: Parser Bool
boolean = (/= 0) <$> unsignedInt

quotedString :: Parser String
quotedString = between (char '\"') (char '\"') (many $ noneOf ['\"'])

-- | Space consumer; either whitespaces or line comments starting with '#'.
sc :: Parser ()
sc = Lexer.space (void spaceChar) (Lexer.skipLineComment "#") empty

-- | Lexeme; parser prepended with the space consumer.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc
