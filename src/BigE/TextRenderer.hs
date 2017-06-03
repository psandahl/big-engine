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

import           BigE.Math              (mkScale, mkTranslate)
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
import           Linear                 (M44, V3 (..), (!*!))
import           Prelude                hiding (init)

-- | TextRenderer record. Opaque to the user.
data TextRenderer = TextRenderer
    { program      :: !Program
    , transformLoc :: !Location
    , fontColorLoc :: !Location
    , userAlphaLoc :: !Location
    , fontAtlasLoc :: !Location
    } deriving Show

-- | Rendering position. From the user perspective the screen coordinates
-- are (-1, -1) at the upper left corner, (0, 0) at the
-- middle and (1, 1) at the lower right corner.
data Position
    = LeftAt !GLfloat !GLfloat
      -- ^ Position the text with its upper left corner at x, y.

    | CenterAt !GLfloat !GLfloat
      -- ^ Position the text with its center at x, y.
    deriving Show

-- | Parameters for the rendering.
data RenderParams = RenderParams
    { size     :: !Int
      -- ^ The size of the rendered text. Smallest value is 1, and biggest
      -- value is 30. Will be clamped if outside range. The size is specific
      -- to each font and its basic size.

    , position :: !Position
      -- ^ Text position of the text to be rendered. See 'Position' for
      -- explanation.

    , color    :: !(V3 GLfloat)
      -- ^ The color r,g,b of the text to be rendered.

    , alpha    :: !GLfloat
      -- ^ User specified alpha value for the text to be rendered. Iff the
      -- user specified alpha is lower than the 'Font's bitmap, the user
      -- specified alpha is selected. Otherwise not. The user specified alpha
      -- can be used for fading out text.
    } deriving Show

-- | Default set of 'RenderParams'.
defaultRenderParams :: RenderParams
defaultRenderParams =
    RenderParams
        { size = 10
        , position = LeftAt (-1) (-1)
        , color = V3 1 0 0
        , alpha = 1
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
            userAlphaLoc' <- Program.getUniformLocation program' "userAlpha"
            fontAtlasLoc' <- Program.getUniformLocation program' "fontAtlas"

            return $ Right TextRenderer
                { program = program'
                , transformLoc = transformLoc'
                , fontColorLoc = fontColorLoc'
                , userAlphaLoc = userAlphaLoc'
                , fontAtlasLoc = fontAtlasLoc'
                }

        Left err -> return $ Left err

-- | Delete the 'TextRenderer'.
delete :: MonadIO m => TextRenderer -> m ()
delete = Program.delete . program

-- | Render the 'Text' using the 'TextRenderer'.
render :: Text -> RenderParams -> TextRenderer -> Render state ()
render text params textRenderer = do
    (width, height) <- displayDimensions
    let aspectRatio = fromIntegral width / fromIntegral height
        size' = fromIntegral $ clamp 1 30 $ size params
        scale = 1 / (31 - size')
        transform = textTranslate (position params) text scale !*!
                    fontScaling scale aspectRatio

    GL.glEnable GL.GL_BLEND
    GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA

    Program.enable (program textRenderer)
    Text.enable text
    Font.enable 0 (font text)

    setUniform (transformLoc textRenderer) transform
    setUniform (fontColorLoc textRenderer) (color params)
    setUniform (userAlphaLoc textRenderer) (clamp 0 1 $ alpha params)
    setUniform (fontAtlasLoc textRenderer) (0 :: GLint)

    Text.render text

    Font.disable 0
    Text.disable
    Program.disable

    GL.glDisable GL.GL_BLEND

-- | Make a scaling matrix.
fontScaling :: GLfloat -> GLfloat -> M44 GLfloat
fontScaling scale aspectRatio =
    mkScale $ V3 scale (scale * aspectRatio) scale

-- | Make a translation matrix from the given position.
textTranslate :: Position -> Text -> GLfloat -> M44 GLfloat
textTranslate (LeftAt x y) _text _scale =
    let xTrans = clamp (-1) 1 x
        yTrans = clamp (-1) 1 (negate y)
    in mkTranslate $ V3 xTrans yTrans 0

textTranslate (CenterAt x y) text scale =
    let xOffset = (scale * gridWidth text / 2)
        xTrans = clamp (-1) 1 (x - xOffset)
        yTrans = clamp (-1) 1 (negate y)
    in mkTranslate $ V3 xTrans yTrans 0

-- | The 'Text's vertex shader.
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

-- | The 'Text's fragment shader.
fragmentShader :: ByteString
fragmentShader =
    "#version 330 core\n\
    \\n\
    \in vec2 vTexCoord;\n\
    \uniform vec3 fontColor;\n\
    \uniform float userAlpha;\n\
    \uniform sampler2D fontAtlas;\n\
    \\n\
    \out vec4 color;\n\
    \\n\
    \void main()\n\
    \{\n\
    \  float bitmapAlpha = texture(fontAtlas, vTexCoord).a;\n\
    \  color = vec4(fontColor, min(bitmapAlpha, userAlpha));\n\
    \}\n\
    \"
