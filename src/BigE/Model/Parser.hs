-- |
-- Module: BigE.Model.Parser
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- Parse a Wavefront object file. Only a subset of the file format is supported.
module BigE.Model.Parser
    ( FilePart (..)
    , parser
    ) where

import           Control.Applicative             (empty)
import           Control.Monad                   (void)
import           Data.Scientific                 (toRealFloat)
import           Graphics.GL                     (GLfloat)
import           Linear                          (V3 (..))
import           Text.Megaparsec
import           Text.Megaparsec.ByteString.Lazy (Parser)
import qualified Text.Megaparsec.Lexer           as Lexer

-- | A part of the parsed model file.
data FilePart
    = Vertex !(V3 GLfloat)
    | Normal !(V3 GLfloat)
    deriving (Eq, Show)

-- | Parse content from the 'Parser's content stream.
parser :: Parser [FilePart]
parser = manyTill (lexeme filePart) eof

filePart :: Parser FilePart
filePart = try vertex
       <|> try normal

-- | Parse one vertex specification.
vertex :: Parser FilePart
vertex = kwV *> (Vertex <$> v3)

-- | Parse one vertex normal specification.
normal :: Parser FilePart
normal = kwVN *> (Normal <$> v3)

-- | Parse one V3.
v3 :: Parser (V3 GLfloat)
v3 = V3 <$> lexeme glFloat <*> lexeme glFloat <*> lexeme glFloat

-- | Parse one GLfloat.
glFloat :: Parser GLfloat
glFloat = toRealFloat <$> Lexer.signed sc Lexer.scientific

-- | Keyword "v". Initiates a vertex specification.
kwV :: Parser ()
kwV = void $ Lexer.symbol sc "v"

-- | Keyword "vn". Initiates a vertex normal specification.
kwVN :: Parser ()
kwVN = void $ Lexer.symbol sc "vn"
-- | Space consumer; white space or comments starting with #.
sc :: Parser ()
sc = Lexer.space (void spaceChar) (Lexer.skipLineComment "#") empty

-- | Lexeme; a parser prepended by the space comsumer.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc
