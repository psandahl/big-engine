-- |
-- Module: BigE.TextRenderer.Text
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.TextRenderer.Text
    ( Text
    , init
    , mkCharacterBox
    , normLen
    ) where

import           BigE.Attribute.Vert_P_Tx (Vertex (..))
import           BigE.TextRenderer.Font   (Font (..))
import           BigE.TextRenderer.Types  (Character (..))
import           Control.Monad.IO.Class   (MonadIO)
import           Graphics.GL              (GLfloat)
import           Linear                   (V2 (..), V3 (..))
import           Prelude                  hiding (init)

data Text = Text
    deriving Show

type Cursor = GLfloat
type NormLen = Int -> GLfloat

init :: MonadIO m => Font -> String -> m Text
init = undefined

mkCharacterBox :: Cursor -> NormLen -> Character -> [Vertex]
mkCharacterBox cursor nl char =
    let xStart = cursor + nl (xOffset char)
        xStop = xStart + nl (width char)
        yTop = 0 - nl (yOffset char)
        yBottom = yTop - nl (height char)
    in
        [ Vertex { position = V3 xStop yTop 0, texCoord = V2 0 0 }
        , Vertex { position = V3 xStart yTop 0, texCoord = V2 0 0 }
        , Vertex { position = V3 xStart yBottom 0, texCoord = V2 0 0 }
        , Vertex { position = V3 xStop yBottom 0, texCoord = V2 0 0 }
        ]

normLen :: Int -> Int -> GLfloat
normLen unit len = fromIntegral len / fromIntegral unit
