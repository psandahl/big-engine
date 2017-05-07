{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: BigE.TextRenderer
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.TextRenderer
    ( TextRenderer (..)
    , RenderParams (..)
    , init
    , render
    , delete
    ) where

import qualified BigE.Program           as Program
import           BigE.Runtime           (Render, displayDimensions)
import qualified BigE.TextRenderer.Font as Font
import           BigE.TextRenderer.Text (Text (..))
import qualified BigE.TextRenderer.Text as Text
import           BigE.Types             (Location, Program, ShaderType (..),
                                         setUniform)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8  (ByteString)
import           Graphics.GL            (GLfloat, GLint)
import qualified Graphics.GL            as GL
import           Linear                 (M44, V3 (..), V4 (..), (!*!))
import           Prelude                hiding (init)

data TextRenderer = TextRenderer
    { program      :: !Program
    , transformLoc :: !Location
    , fontColorLoc :: !Location
    , fontAtlasLoc :: !Location
    } deriving Show

data RenderParams = RenderParams
    { size :: !Int
    } deriving Show

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
    (width, height) <- displayDimensions
    let aspectRatio = fromIntegral width / fromIntegral height
        size' = fromIntegral $ clamp 1 30 $ size params
        scale = 1 / (31 - size')
        transform = textTranslate !*! fontScaling scale aspectRatio

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

fontScaling :: GLfloat -> GLfloat -> M44 GLfloat
fontScaling scale aspectRatio =
    V4 (V4 scale 0 0 0)
       (V4 0 (scale * aspectRatio) 0 0)
       (V4 0 0 scale 0)
       (V4 0 0 0 1)

textTranslate :: M44 GLfloat
textTranslate =
    V4 (V4 1 0 0 (-1))
       (V4 0 1 0 1)
       (V4 0 0 1 0)
       (V4 0 0 0 1)

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = min mx . max mn

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
