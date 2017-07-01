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
    , Point (..)
    , parser
    ) where

import           Control.Applicative             (empty)
import           Control.Monad                   (void)
import           Data.Scientific                 (toRealFloat)
import           Graphics.GL                     (GLfloat)
import           Linear                          (V2 (..), V3 (..))
import           Text.Megaparsec
import           Text.Megaparsec.ByteString.Lazy (Parser)
import qualified Text.Megaparsec.Lexer           as Lexer

-- | A part of the parsed model file.
data FilePart
    = Vertex !(V3 GLfloat)
    | Normal !(V3 GLfloat)
    | TexCoord !(V2 GLfloat)
    | Triangle !Point !Point !Point
    deriving (Eq, Show)

-- | A point in a triangle face, where all the components are describing an
-- index:
-- First component is vertex index. It is mandatory.
-- Second component is texture coordinate index.
-- Third component is normal index.
data Point = Point !Int !(Maybe Int) !(Maybe Int)
    deriving (Eq, Show)

-- | Parse content from the 'Parser's content stream.
parser :: Parser [FilePart]
parser = manyTill (lexeme filePart) eof

filePart :: Parser FilePart
filePart = try vertex
       <|> try normal
       <|> try texCoord
       <|> try triangle

-- | Parse one vertex specification.
vertex :: Parser FilePart
vertex = kwV *> (Vertex <$> v3)

-- | Parse one vertex normal specification.
normal :: Parser FilePart
normal = kwVN *> (Normal <$> v3)

-- | Parse one texture coordinate specification.
texCoord :: Parser FilePart
texCoord = kwVT *> (TexCoord <$> v2)

-- | Parse one triangle face.
triangle :: Parser FilePart
triangle = kwF *> (Triangle <$> lexeme point <*> lexeme point <*> lexeme point)

-- | Parse one point.
point :: Parser Point
point = Point <$> int
              <*> (char '/' *> optional int)
              <*> (char '/' *> optional int)

-- | Parse one V2.
v2 :: Parser (V2 GLfloat)
v2 = V2 <$> lexeme glFloat <*> lexeme glFloat

-- | Parse one V3.
v3 :: Parser (V3 GLfloat)
v3 = V3 <$> lexeme glFloat <*> lexeme glFloat <*> lexeme glFloat

-- | Parse one GLfloat.
glFloat :: Parser GLfloat
glFloat = toRealFloat <$> Lexer.signed sc Lexer.scientific

-- | Parse one Int.
int :: Parser Int
int = fromIntegral <$> Lexer.signed sc Lexer.integer

-- | Keyword "v". Initiates a vertex specification.
kwV :: Parser ()
kwV = void $ Lexer.symbol sc "v"

-- | Keyword "vn". Initiates a vertex normal specification.
kwVN :: Parser ()
kwVN = void $ Lexer.symbol sc "vn"

-- | Keyword "vt". Initiates a texture coordinate specification.
kwVT :: Parser ()
kwVT = void $ Lexer.symbol sc "vt"

-- | Keyword "f". Initiates a face (triangle) specification.
kwF :: Parser ()
kwF = void $ Lexer.symbol sc "f"

-- | Space consumer; white space or comments starting with #.
sc :: Parser ()
sc = Lexer.space (void spaceChar) (Lexer.skipLineComment "#") empty

-- | Lexeme; a parser prepended by the space comsumer.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc
