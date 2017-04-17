{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
-- |
-- Module: BigE.MousePicker
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.MousePicker
    ( MousePicker (colorTexture)
    , ObjectId
    , PickObject (..)
    , Pickable (..)
    , mkObjectId
    , init
    , enable
    , disable
    , delete
    , render
    , getObjectId
    ) where

import           BigE.Internal.GLResources (genFramebuffer, genTexture)
import qualified BigE.Program              as Program
import           BigE.Runtime.Render       (Render)
import           BigE.Types                (Framebuffer (..), Location (..),
                                            Program, ShaderType (..),
                                            Texture (..), TextureMagFilter (..),
                                            TextureMinFilter (..),
                                            TextureWrap (..), ToGLint (..),
                                            Uniform (..))
import           Control.Monad             (forM_)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Bits                 ((.|.))
import           Data.ByteString.Char8     (ByteString)
import           Foreign                   (nullPtr)
import           Graphics.GL               (GLfloat, GLuint)
import qualified Graphics.GL               as GL
import           Linear                    (M44, (!*!))
import           Prelude                   hiding (init)

data MousePicker = MousePicker
    { frameBuffer      :: !Framebuffer
    , colorTexture     :: !Texture
    , depthTexture     :: !Texture
    , program          :: !Program
    , mvpLocation      :: !Location
    , objectIdLocation :: !Location
    } deriving Show

data ObjectId = ObjectId !GLuint
    deriving (Eq, Show)

instance Uniform ObjectId where
    setUniform (Location loc) (ObjectId objId) =
        GL.glUniform1ui loc objId

class PickObject a where
    objectId :: a -> ObjectId
    modelMatrix :: a -> M44 GLfloat
    renderForPicking :: a -> Render app ()

data Pickable = forall a. PickObject a => Pickable a

mkObjectId :: GLuint -> ObjectId
mkObjectId = ObjectId

init :: MonadIO m => Int -> Int -> m (Either String MousePicker)
init width height = do
    eProgram <- initProgram
    case eProgram of

        Right program' -> do
            (frameBuffer', colorTexture', depthTexture') <- initResources width height
            mvpLocation' <- Program.getUniformLocation program' "mvp"
            objectIdLocation' <- Program.getUniformLocation program' "objectId"

            return $ Right MousePicker
                { frameBuffer = frameBuffer'
                , colorTexture = colorTexture'
                , depthTexture = depthTexture'
                , program = program'
                , mvpLocation = mvpLocation'
                , objectIdLocation = objectIdLocation'
                }

        Left err      -> return $ Left err

enable :: MonadIO m => MousePicker -> m ()
enable mousePicker = do
    let Framebuffer fbo = frameBuffer mousePicker
    GL.glBindFramebuffer GL.GL_DRAW_FRAMEBUFFER fbo

disable :: MonadIO m => m ()
disable = GL.glBindFramebuffer GL.GL_DRAW_FRAMEBUFFER 0

delete :: MonadIO m => MousePicker -> m ()
delete = undefined

render :: M44 GLfloat -> [Pickable] -> MousePicker -> Render app ()
render vp xs mousePicker = do
    enable mousePicker
    Program.enable $ program mousePicker

    GL.glEnable GL.GL_DEPTH_FUNC
    GL.glClearColor 1 1 1 1
    GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)

    forM_ xs $ \(Pickable pickObj) -> do
        let mvp = vp !*! modelMatrix pickObj
        setUniform (mvpLocation mousePicker) mvp
        setUniform (objectIdLocation mousePicker) (objectId pickObj)
        renderForPicking pickObj

    Program.disable
    disable

getObjectId :: MonadIO m => (Int, Int) -> MousePicker -> m ObjectId
getObjectId = undefined

initProgram :: MonadIO m => m (Either String Program)
initProgram =
    Program.fromByteString
        [ (VertexShader, "builtin/MousePicker/vertex.glsl", vertexShader)
        , (FragmentShader, "builtin/MousePicker/fragment.glsl", fragmentShader)
        ]

initResources :: MonadIO m => Int -> Int -> m (Framebuffer, Texture, Texture)
initResources width height = do
    -- Create the frame buffer object.
    frameBuffer'@(Framebuffer fbo) <- genFramebuffer
    GL.glBindFramebuffer GL.GL_FRAMEBUFFER fbo

    -- Create the color texture and attach it to the frame buffer.
    colorTexture'@(Texture cTex) <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_2D cTex
    GL.glTexImage2D GL.GL_TEXTURE_2D 0 (fromIntegral GL.GL_RGB)
                    (fromIntegral width) (fromIntegral height)
                    0 GL.GL_RGB GL.GL_UNSIGNED_BYTE nullPtr
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S (toGLint WrapClampToEdge)
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T (toGLint WrapClampToEdge)
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (toGLint MinNearest)
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (toGLint MagNearest)
    GL.glFramebufferTexture2D GL.GL_FRAMEBUFFER GL.GL_COLOR_ATTACHMENT0
                              GL.GL_TEXTURE_2D cTex 0

    -- Create the depth texture and attach it to the frame buffer.
    depthTexture'@(Texture dTex) <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_2D dTex
    GL.glTexImage2D GL.GL_TEXTURE_2D 0 (fromIntegral GL.GL_DEPTH_COMPONENT)
                    (fromIntegral width) (fromIntegral height)
                    0 GL.GL_DEPTH_COMPONENT GL.GL_FLOAT nullPtr
    GL.glFramebufferTexture2D GL.GL_FRAMEBUFFER GL.GL_DEPTH_ATTACHMENT
                              GL.GL_TEXTURE_2D dTex 0

    -- Disable buffer reading.
    GL.glReadBuffer GL.GL_NONE

    -- Set the attached color buffer as draw buffer.
    GL.glDrawBuffer GL.GL_COLOR_ATTACHMENT0

    bufStat <- GL.glCheckFramebufferStatus GL.GL_FRAMEBUFFER
    liftIO $ print (bufStat == GL.GL_FRAMEBUFFER_COMPLETE)
    liftIO $ print bufStat

    -- Restore default frame buffer.
    GL.glBindTexture GL.GL_TEXTURE_2D 0
    GL.glBindFramebuffer GL.GL_FRAMEBUFFER 0

    return (frameBuffer', colorTexture', depthTexture')


vertexShader :: ByteString
vertexShader =
    "#version 330 core\n\
    \\n\
    \layout (location = 0) in vec3 position;\n\
    \uniform mat4 mvp;\n\
    \\n\
    \void main()\n\
    \{\n\
    \  gl_Position = mvp * vec4(position, 1.0);\n\
    \}"

fragmentShader :: ByteString
fragmentShader =
    "#version 330 core\n\
    \\n\
    \uniform uint objectId;\n\
    \out vec4 color;\n\
    \\n\
    \void main()\n\
    \{\n\
    \  uint r = (objectId >> 16) & 0xFFu;\n\
    \  uint g = (objectId >> 8) & 0xFFu;\n\
    \  uint b = objectId & 0xFFu;\n\
    \  color = vec4(float(r) / 255.0, float(g) / 255.0, float(b) / 255.0, 1.0);\n\
    \}"
