{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
-- |
-- Module: BigE.MousePicker
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module BigE.MousePicker
    ( MousePicker (framebuffer)
    , PickId
    , PickObject (..)
    , Pickable (..)
    , zeroPickId
    , nextPickId
    , unpickableId
    , literalPickId
    , init
    , resize
    , delete
    , render
    , getPickId
    ) where

import           BigE.Internal.GLResources (deleteProgram)
import qualified BigE.Program              as Program
import           BigE.Runtime.Render       (Render)
import           BigE.TexturedFramebuffer  (TexturedFramebuffer)
import qualified BigE.TexturedFramebuffer  as TFB
import           BigE.Types                (Location (..), Program,
                                            ShaderType (..), Uniform (..))
import           Control.Monad             (forM_)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Bits                 (shift, (.&.), (.|.))
import           Data.ByteString.Char8     (ByteString)
import           Foreign                   (peekArray, withArray)
import           Graphics.GL               (GLfloat, GLsizei, GLubyte, GLuint)
import qualified Graphics.GL               as GL
import           Linear                    (M44, (!*!))
import           Prelude                   hiding (init)
import           Text.Printf               (printf)

-- | The mouse picker record. Besides the color texture map the content of
-- the record is opaque to the user. If using the color texture directly beware
-- that the texture will be replaces when the display is resizing.
data MousePicker = MousePicker
    { framebuffer    :: !TexturedFramebuffer
    , program        :: !Program
    , mvpLocation    :: !Location
    , pickIdLocation :: !Location
    , textureWidth   :: !GLsizei
    , textureHeight  :: !GLsizei
    } deriving Show

-- | An id for a 'PickObject'. Will translate to a r,g,b color for the object
-- when rendered to the framebuffer.
newtype PickId = PickId GLuint
    deriving Eq

-- | Show instance for 'PickId'.
instance Show PickId where
    show (PickId pid) = printf "PickId %u [%u,%u,%u]" pid r g b
        where
            r = shift pid (-16) .&. 0xFF
            g = shift pid (-8) .&. 0xFF
            b = pid .&. 0xFF

-- | Uniform instance for 'PickId'.
instance Uniform PickId where
    setUniform (Location loc) (PickId pid) =
        GL.glUniform1ui loc pid

-- | All 3D types that need to be picked must derive the PickObject typeclass.
class PickObject a where
    -- | Get the type's 'PickId'.
    pickId :: a -> PickId

    -- | Get the type's model/world matrix.
    modelMatrix :: a -> M44 GLfloat

    -- | The type's simplified pick rendering function. Enabling VAO, render
    -- primitives and disabling VAO is what should be done. Nothing more,
    -- nothing less.
    renderForPicking :: a -> Render app ()

-- | Type to allow lists of PickObject typeclass implementations of different
-- types.
data Pickable = forall a. PickObject a => Pickable a

-- | Give the first possible - zero valued - 'PickId'.
zeroPickId :: PickId
zeroPickId = PickId 0

-- | Give the next - enumerated - 'PickId'.
nextPickId :: PickId -> PickId
nextPickId (PickId pid) = PickId (pid + 1)

-- | Give the 'unpickable id'. The id of things that's not pickable. Like
-- the background or explicitely not pickable object (which need to be
-- rendered for depth checking).
unpickableId :: PickId
unpickableId = literalPickId maxBound maxBound maxBound

-- | Make an object id with an explicit value given from r, g and b values.
literalPickId :: GLubyte -> GLubyte -> GLubyte -> PickId
literalPickId r g b =
    PickId $ shift (fromIntegral r) 16 .|. shift (fromIntegral g) 8 .|. fromIntegral b

-- | Initialize the mouse picker with the display dimensions of width and height.
init :: MonadIO m => Int -> Int -> m (Either String MousePicker)
init width height = do
    eProgram <- initProgram
    case eProgram of

        Right program' -> do
            framebuffer' <- TFB.init width height
            mvpLocation' <- Program.getUniformLocation program' "mvp"
            pickIdLocation' <- Program.getUniformLocation program' "pickId"

            return $ Right MousePicker
                { framebuffer = framebuffer'
                , program = program'
                , mvpLocation = mvpLocation'
                , pickIdLocation = pickIdLocation'
                , textureWidth = fromIntegral width
                , textureHeight = fromIntegral height
                }

        Left err      -> return $ Left err

-- | Resize the mouse picker.
resize :: MonadIO m => Int -> Int -> MousePicker -> m (Either String MousePicker)
resize width height mousePicker = do
    TFB.delete $ framebuffer mousePicker

    framebuffer' <- TFB.init width height

    return $ Right mousePicker
        { framebuffer = framebuffer'
        , textureWidth = fromIntegral width
        , textureHeight = fromIntegral height
        }

-- | Delete all mouse picker resources.
delete :: MonadIO m => MousePicker -> m ()
delete mousePicker = do
    TFB.delete $ framebuffer mousePicker
    deleteProgram $ program mousePicker

-- | Render the 'Pickable' objects, i.e. types implementing the 'PickObject'
-- typeclass from the list. The given matrix shall be a valid perspective *
-- view matrix for the scene. When done the OpenGL state is set with
-- 1 1 1 0 as clear color, and depth checking is activated.
render :: M44 GLfloat -> [Pickable] -> MousePicker -> Render app ()
render vp xs mousePicker = do
    TFB.enableDraw $ framebuffer mousePicker
    Program.enable $ program mousePicker

    GL.glViewport 0 0 (textureWidth mousePicker) (textureHeight mousePicker)

    GL.glEnable GL.GL_DEPTH_TEST
    GL.glClearColor 1 1 1 0 -- The clear color *must* be white.
    GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)

    forM_ xs $ \(Pickable pickObj) -> do
        let mvp = vp !*! modelMatrix pickObj
        setUniform (mvpLocation mousePicker) mvp
        setUniform (pickIdLocation mousePicker) (pickId pickObj)
        renderForPicking pickObj

    Program.disable
    TFB.disableDraw

-- | Get the 'PickId' for the thing beeing at the (x, y) pixel coordinate.
-- If an unpickable object is found, Nothing is returned.
getPickId :: MonadIO m => (Int, Int) -> MousePicker -> m (Maybe PickId)
getPickId (x, y) mousePicker = do
    TFB.enableRead $ framebuffer mousePicker
    pid <- liftIO readPixel
    TFB.disableRead

    if pid /= unpickableId
        then return $ Just pid
        else return Nothing
    where
        readPixel :: IO PickId
        readPixel =
            withArray [0, 0, 0] $ \ptr -> do
                GL.glReadPixels (fromIntegral x) (fromIntegral y) 1 1
                                GL.GL_RGB GL.GL_UNSIGNED_BYTE ptr
                [r, g, b] <- peekArray 3 ptr
                return $ literalPickId r g b

-- | Initialize the shader programs.
initProgram :: MonadIO m => m (Either String Program)
initProgram =
    Program.fromByteString
        [ (VertexShader, "builtin/MousePicker/vertex.glsl", vertexShader)
        , (FragmentShader, "builtin/MousePicker/fragment.glsl", fragmentShader)
        ]

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
    \uniform uint pickId;\n\
    \out vec4 color;\n\
    \\n\
    \void main()\n\
    \{\n\
    \  uint r = (pickId >> 16) & 0xFFu;\n\
    \  uint g = (pickId >> 8) & 0xFFu;\n\
    \  uint b = pickId & 0xFFu;\n\
    \  color = vec4(float(r) / 255.0, float(g) / 255.0, float(b) / 255.0, 1.0);\n\
    \}"
