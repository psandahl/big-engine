-- |
-- Module: BigE.TextRenderer.Text
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.TextRenderer.Text
    ( Text (mesh)
    , init
    ) where

import           BigE.Attribute.Vert_P_Tx (Vertex (..))
import           BigE.Mesh                (Mesh, fromVector)
import           BigE.TextRenderer.Font   (Font (..))
import           BigE.TextRenderer.Types  (Character (..), Common (..))
import           BigE.Types               (BufferUsage (..))
import           Control.Monad.IO.Class   (MonadIO)
import           Data.Char                (ord)
import qualified Data.HashMap.Strict      as HashMap
import           Data.List                (foldl')
import           Data.Vector.Storable     (Vector)
import qualified Data.Vector.Storable     as Vector
import           Graphics.GL              (GLfloat, GLuint)
import           Linear                   (V2 (..), V3 (..))
import           Prelude                  hiding (init)

-- | Representation of a drawable piece of text.
data Text = Text
    { font   :: !Font
    , mesh   :: !Mesh
    , string :: !String
    }
    deriving Show

type Cursor = GLfloat
type PixToCoord = Int -> GLfloat

-- | Initialize the text using a 'Font' and a string.
init :: MonadIO m => Font -> String -> m Text
init fnt str = do
    mesh' <- fromVector DynamicDraw
                        (mkCharacterBoxVertices fnt str)
                        (mkIndices $ length str)
    return Text { font = fnt, mesh = mesh', string = str }

mkCharacterBoxVertices :: Font -> String -> Vector Vertex
mkCharacterBoxVertices fnt str =
    let vert = pixToCoord (lineHeight $ common fnt)
        tex = pixToCoord (scaleW $ common fnt) -- Assume square texture.
        chars = characters fnt
    in snd $ foldl' (\(cursor, vec) ascii ->
                        case HashMap.lookup (ord ascii) chars of

                            -- Character found. Make a box and advance the
                            -- the cursor.
                            Just char ->
                                let box = mkCharacterBox cursor vert tex char
                                    cursor' = cursor + vert (xAdvance char)
                                    vec' = Vector.concat [vec, box]
                                in (cursor', vec')

                            -- No valid character found. Just skip.
                            Nothing -> (cursor, vec)
                    ) (0, Vector.empty) str

-- | Construct one single character box.
mkCharacterBox :: Cursor -> PixToCoord -> PixToCoord -> Character -> Vector Vertex
mkCharacterBox cursor vert tex char =
    -- Start calculate the coordinates for the square surrounding the
    -- character. Vertice coords are normalized to the line height which
    -- is interpreted as the length one.
    let xStart = cursor + vert (xOffset char)
        xStop = xStart + vert (width char)
        yTop = negate (vert (yOffset char))
        yBottom = yTop - vert (height char)

        -- Then calculate the texture coordinates. Texture coords are normalized
        -- to the dimensions of the texture atlas for the font.
        xStartTex = tex (x char)
        xStopTex = xStartTex + tex (width char)
        yTopTex = tex (y char)
        yBottomTex = yTopTex + tex (height char)
    in Vector.fromList
           [ -- Upper right corner.
             Vertex { position = V3 xStop yTop 0
                    , texCoord = V2 xStopTex yTopTex
                    }

             -- Upper left corner.
           , Vertex { position = V3 xStart yTop 0
                    , texCoord = V2 xStartTex yTopTex
                    }

             -- Lower left corner.
           , Vertex { position = V3 xStart yBottom 0
                    , texCoord = V2 xStartTex yBottomTex
                    }

             -- Lower right corner.
           , Vertex { position = V3 xStop yBottom 0
                    , texCoord = V2 xStopTex yBottomTex
                    }
           ]

-- | Make vertex indices for the specified number of boxes.
mkIndices :: Int -> Vector GLuint
mkIndices 0 = Vector.empty
mkIndices num =
    Vector.fromList $ concatMap indicesFor [b * 4 | b <- [0 .. fromIntegral num - 1]]
    where
        indicesFor b = [ b, b + 1, b + 2, b, b + 2, b + 3 ]

-- | Transform a pixel length to a coordinate. To help a unit value, which
-- represents the coordinate length of 1, is provided.
pixToCoord :: Int -> Int -> GLfloat
pixToCoord unit len = fromIntegral len / fromIntegral unit