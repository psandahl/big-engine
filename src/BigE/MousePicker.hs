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
    , firstObjectId
    , nextObjectId
    , noObjectId
    , mkObjectId
    , init
    , resize
    , enable
    , disable
    , delete
    , render
    , getPickedObjectId
    ) where

import           BigE.Internal.GLResources (deleteFramebuffer, deleteProgram,
                                            deleteTexture, genFramebuffer,
                                            genTexture)
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
import           Data.Bits                 (shift, (.|.))
import           Data.ByteString.Char8     (ByteString)
import           Foreign                   (nullPtr, peekArray, withArray)
import           Graphics.GL               (GLfloat, GLsizei, GLubyte, GLuint)
import qualified Graphics.GL               as GL
import           Linear                    (M44, (!*!))
import           Prelude                   hiding (init)

-- | The mouse picker record. Besides the color texture map the content of
-- the record is opaque to the user. If using the color texture directly beware
-- that the texture will be replaces when the display is resizing.
data MousePicker = MousePicker
    { frameBuffer      :: !Framebuffer
    , colorTexture     :: !Texture
    , depthTexture     :: !Texture
    , program          :: !Program
    , mvpLocation      :: !Location
    , objectIdLocation :: !Location
    , textureWidth     :: !GLsizei
    , textureHeight    :: !GLsizei
    } deriving Show

-- | An object id of something that is pickable.
data ObjectId = ObjectId !GLuint
    deriving (Eq, Show)

-- | Uniform instance for 'ObjectId'.
instance Uniform ObjectId where
    setUniform (Location loc) (ObjectId objId) =
        GL.glUniform1ui loc objId

-- | All 3D types that need to be picked must derive the PickObject typeclass.
class PickObject a where
    -- | Get the type's 'ObjectId'.
    objectId :: a -> ObjectId

    -- | Get the type's model/world matrix.
    modelMatrix :: a -> M44 GLfloat

    -- | The type's simplified pick rendering function. Enabling VAO, render
    -- primitives and disabling VAO is what should be done. Nothing more,
    -- nothing less.
    renderForPicking :: a -> Render app ()

-- | Type to allow lists of PickObject typeclass implementations of different
-- types.
data Pickable = forall a. PickObject a => Pickable a

-- | Give the first possible - zero valued - 'ObjectId'.
firstObjectId :: ObjectId
firstObjectId = ObjectId 0

-- | Give the next - enumerated - 'ObjectId'.
nextObjectId :: ObjectId -> ObjectId
nextObjectId (ObjectId objId) = ObjectId (objId + 1)

-- | Give the 'no object id'. The id of things that's not pickable. Like
-- the background or explicitely not pickable object (which need to be
-- rendered for depth checking).
noObjectId :: ObjectId
noObjectId = mkObjectId maxBound maxBound maxBound

-- | Make an object id with an explicit value given from r, g and b values.
mkObjectId :: GLubyte -> GLubyte -> GLubyte -> ObjectId
mkObjectId r g b =
    ObjectId $ shift (fromIntegral r) 16 .|. shift (fromIntegral g) 8 .|. fromIntegral b

-- | Initialize the mouse picker with the display dimensions of width and height.
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
                , textureWidth = fromIntegral width
                , textureHeight = fromIntegral height
                }

        Left err      -> return $ Left err

-- | Resize the mouse picker.
resize :: MonadIO m => Int -> Int -> MousePicker -> m (Either String MousePicker)
resize width height mousePicker = do
    deleteTexture $ depthTexture mousePicker
    deleteTexture $ colorTexture mousePicker
    deleteFramebuffer $ frameBuffer mousePicker

    (frameBuffer', colorTexture', depthTexture') <- initResources width height

    return $ Right mousePicker
        { frameBuffer = frameBuffer'
        , colorTexture = colorTexture'
        , depthTexture = depthTexture'
        , textureWidth = fromIntegral width
        , textureHeight = fromIntegral height
        }

-- | Enable the framebuffer.
enable :: MonadIO m => MousePicker -> m ()
enable mousePicker = do
    let Framebuffer fbo = frameBuffer mousePicker
    GL.glBindFramebuffer GL.GL_DRAW_FRAMEBUFFER fbo

-- | Disable the framebuffer.
disable :: MonadIO m => m ()
disable = GL.glBindFramebuffer GL.GL_DRAW_FRAMEBUFFER 0

-- | Delete all mouse picker resources.
delete :: MonadIO m => MousePicker -> m ()
delete mousePicker = do
    deleteTexture $ depthTexture mousePicker
    deleteTexture $ colorTexture mousePicker
    deleteFramebuffer $ frameBuffer mousePicker
    deleteProgram $ program mousePicker

-- | Render the 'Pickable' objects, i.e. types implementing the 'PickObject'
-- typeclass from the list. The given matrix shall be a valid perspective *
-- view matrix for the scene. When done the OpenGL state is set with
-- 1 1 1 0 as clear color, and depth checking is activated.
render :: M44 GLfloat -> [Pickable] -> MousePicker -> Render app ()
render vp xs mousePicker = do
    enable mousePicker
    Program.enable $ program mousePicker

    GL.glViewport 0 0 (textureWidth mousePicker) (textureHeight mousePicker)

    GL.glEnable GL.GL_DEPTH_FUNC
    GL.glClearColor 1 1 1 0
    GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)

    forM_ xs $ \(Pickable pickObj) -> do
        let mvp = vp !*! modelMatrix pickObj
        setUniform (mvpLocation mousePicker) mvp
        setUniform (objectIdLocation mousePicker) (objectId pickObj)
        renderForPicking pickObj

    Program.disable
    disable

-- | Get the 'ObjectId' for the thing beeing at the (x, y) pixel coordinate.
-- If no identifiable object can be found Nothing is returned.
getPickedObjectId :: MonadIO m => (Int, Int) -> MousePicker -> m (Maybe ObjectId)
getPickedObjectId (x, y) mousePicker = do
    let Framebuffer fbo = frameBuffer mousePicker

    GL.glBindFramebuffer GL.GL_READ_FRAMEBUFFER fbo
    GL.glReadBuffer GL.GL_COLOR_ATTACHMENT0

    objId <- liftIO $ readPixel

    GL.glReadBuffer GL.GL_NONE
    GL.glBindFramebuffer GL.GL_READ_FRAMEBUFFER 0

    if objId /= noObjectId
        then return $ Just objId
        else return Nothing
    where
        readPixel :: IO ObjectId
        readPixel =
            withArray [0, 0, 0] $ \ptr -> do
                GL.glReadPixels (fromIntegral x) (fromIntegral y) 1 1
                                GL.GL_RGB GL.GL_UNSIGNED_BYTE ptr
                [r, g, b] <- peekArray 3 ptr
                return $ mkObjectId r g b

-- | Initialize the shader programs.
initProgram :: MonadIO m => m (Either String Program)
initProgram =
    Program.fromByteString
        [ (VertexShader, "builtin/MousePicker/vertex.glsl", vertexShader)
        , (FragmentShader, "builtin/MousePicker/fragment.glsl", fragmentShader)
        ]

-- | Initialize the Framebuffer and Texture resources needed.
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
    configureTexture
    GL.glFramebufferTexture2D GL.GL_FRAMEBUFFER GL.GL_COLOR_ATTACHMENT0
                              GL.GL_TEXTURE_2D cTex 0

    -- Create the depth texture and attach it to the frame buffer.
    depthTexture'@(Texture dTex) <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_2D dTex
    GL.glTexImage2D GL.GL_TEXTURE_2D 0 (fromIntegral GL.GL_DEPTH_COMPONENT)
                    (fromIntegral width) (fromIntegral height)
                    0 GL.GL_DEPTH_COMPONENT GL.GL_FLOAT nullPtr
    configureTexture
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
    where
      configureTexture = do
        GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S (toGLint WrapClampToEdge)
        GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T (toGLint WrapClampToEdge)
        GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (toGLint MinNearest)
        GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (toGLint MagNearest)

-- | Vertex shader.
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

-- | Fragment shader.
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
