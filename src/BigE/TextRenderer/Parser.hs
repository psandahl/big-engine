-- |
-- Module: BigE.TextRenderer.Parser
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.TextRenderer.Parser
    ( parseSpacing
    ) where

import           BigE.TextRenderer.Font          (Spacing (..))
import           Control.Applicative             (empty)
import           Control.Monad                   (void)
import           Text.Megaparsec
import           Text.Megaparsec.ByteString.Lazy
import qualified Text.Megaparsec.Lexer           as Lexer

-- | Parse a 'Spacing' record from the stream.
parseSpacing :: Parser Spacing
parseSpacing = do
    kwSpacing
    void $ lexeme (char '=')
    h <- signedInt
    void $ lexeme (char ',')
    v <- signedInt
    return $ Spacing { horizontal = h, vertical = v }

kwSpacing :: Parser ()
kwSpacing = void $ Lexer.symbol sc "spacing"

signedInt :: Parser Int
signedInt = fromIntegral <$> Lexer.signed sc Lexer.integer <* sc

-- | Space consumer; either whitespaces or line comments starting with '#'.
sc :: Parser ()
sc = Lexer.space (void spaceChar) (Lexer.skipLineComment "#") empty

-- | Lexeme; parser prepended with the space consumer.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc
