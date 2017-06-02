{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: BigE.TextRenderer
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Low level functionality for rendering text on the graphics device. The
-- application is expected to build higher level abstractions on top of this
-- functionality.
module BigE.TextRenderer
    ( TextRenderer
    , Position (..)
    , RenderParams (..)
    , defaultRenderParams
    , init
    , render
    , delete
    ) where

import           BigE.Math              (mkTranslate)
import qualified BigE.Program           as Program
import           BigE.Runtime           (Render, displayDimensions)
import qualified BigE.TextRenderer.Font as Font
import           BigE.TextRenderer.Text (Text (..))
import qualified BigE.TextRenderer.Text as Text
import           BigE.Types             (Location, Program, ShaderType (..),
                                         setUniform)
import           BigE.Util              (clamp)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8  (ByteString)
import           Graphics.GL            (GLfloat, GLint)
import qualified Graphics.GL            as GL
import           Linear                 (M44, V3 (..), V4 (..), (!*!))
import           Prelude                hiding (init)

-- | TextRenderer record. Opaque to the user.
data TextRenderer = TextRenderer
    { program      :: !Program
    , transformLoc :: !Location
    , fontColorLoc :: !Location
    , fontAtlasLoc :: !Location
    } deriving Show

-- | Rendering position. When using 'LeftAt' the upper left corner of the
-- text will be placed at the coordinates x y given. From the user perspective
-- the screen coordinates are (-1, -1) at the upper left corner, (0, 0) at the
-- middle and (1, 1) at the lower right corner.
data Position
    = LeftAt !GLfloat !GLfloat
    deriving Show

-- | Parameters for the rendering.
data RenderParams = RenderParams
    { size     :: !Int
      -- ^ The size of the rendered text. Smallest value is 1, and biggest
      -- value is 30. Will be clamped if outside range.

    , position :: !Position
      -- ^ Text position.
    } deriving Show

-- | Default set of 'RenderParams'.
defaultRenderParams :: RenderParams
defaultRenderParams =
    RenderParams
        { size = 10
        , position = LeftAt (-1) (-1)
        }

-- | Initialize the 'TextRenderer'.
init :: MonadIO m => m (Either String TextRenderer)
init = do
    eProgram <-
        Program.fromByteString
            [ (VertexShader, "builtin/TextRenderer/vertex.glsl", vertexShader)
            , (FragmentShader, "builtin/TextRenderer/fragment.glsl", fragmentShader)
            ]
    case eProgram of
        Right program' -> do
            transformLoc' <- Program.getUniformLocation program' "transform"
            fontColorLoc' <- Program.getUniformLocation program' "fontColor"
            fontAtlasLoc' <- Program.getUniformLocation program' "fontAtlas"

            return $ Right TextRenderer
                { program = program'
                , transformLoc = transformLoc'
                , fontColorLoc = fontColorLoc'
                , fontAtlasLoc = fontAtlasLoc'
                }

        Left err -> return $ Left err

-- | Delete the 'TextRenderer'.
delete :: MonadIO m => TextRenderer -> m ()
delete = Program.delete . program

-- | Render the 'Text' using the 'TextRenderer'.
render :: Text -> RenderParams -> TextRenderer -> Render state ()
render text params textRenderer = do
    dim@(width, height) <- displayDimensions
    let aspectRatio = fromIntegral width / fromIntegral height
        size' = fromIntegral $ clamp 1 30 $ size params
        scale = 1 / (31 - size')
        transform = textTranslate (position params) text dim !*!
                    fontScaling scale aspectRatio

    GL.glEnable GL.GL_BLEND
    GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA

    Program.enable (program textRenderer)
    Text.enable text
    Font.enable 0 (font text)

    setUniform (transformLoc textRenderer) transform
    setUniform (fontColorLoc textRenderer) (V3 1 0 0 :: V3 GLfloat)
    setUniform (fontAtlasLoc textRenderer) (0 :: GLint)

    Text.render text

    Font.disable 0
    Text.disable
    Program.disable

    GL.glDisable GL.GL_BLEND

fontScaling :: GLfloat -> GLfloat -> M44 GLfloat
fontScaling scale aspectRatio =
    V4 (V4 scale 0 0 0)
       (V4 0 (scale * aspectRatio) 0 0)
       (V4 0 0 scale 0)
       (V4 0 0 0 1)

-- | Make a translation matrix from the given position.
textTranslate :: Position -> Text -> (Int, Int) -> M44 GLfloat
textTranslate (LeftAt x y) _text _dimensions =
    let xTrans = clamp (-1) 1 x
        yTrans = clamp (-1) 1 (negate y)
    in mkTranslate $ V3 xTrans yTrans 0

vertexShader :: ByteString
vertexShader =
    "#version 330 core\n\
    \\n\
    \layout (location = 0) in vec3 position;\n\
    \layout (location = 1) in vec2 texCoord;\n\
    \\n\
    \uniform mat4 transform;\n\
    \\n\
    \out vec2 vTexCoord;\n\
    \\n\
    \void main()\n\
    \{\n\
    \  vTexCoord = texCoord;\n\
    \  gl_Position = transform * vec4(position, 1.0);\n\
    \}\n\
    \"

fragmentShader :: ByteString
fragmentShader =
    "#version 330 core\n\
    \\n\
    \in vec2 vTexCoord;\n\
    \uniform vec3 fontColor;\n\
    \uniform sampler2D fontAtlas;\n\
    \\n\
    \out vec4 color;\n\
    \\n\
    \void main()\n\
    \{\n\
    \  float alpha = texture(fontAtlas, vTexCoord).a;\n\
    \  color = vec4(fontColor, alpha);\n\
    \}\n\
    \"
