-- Module: BigE.ProgramLoader
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.ProgramLoader
    ( fromByteString
    , fromFile
    , delete
    , enable
    , disable
    , getUniformLocation
    ) where

import qualified BigE.Internal.GLResources as GLResources
import           BigE.Types                (Location (..), Program (..),
                                            Shader (..), ShaderType (..))
import           Control.Exception         (SomeException, try)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Char8     as BS
import           Foreign                   (Ptr, nullPtr, peek, with)
import           Foreign.C                 (peekCString, withCString)
import           Graphics.GL               (GLboolean, GLchar, GLint, GLsizei,
                                            GLuint)
import qualified Graphics.GL               as GL

-- | Compile and link the provided shader sources to a program.
fromByteString :: MonadIO m => [(ShaderType, FilePath, ByteString)]
               -> m (Either String Program)
fromByteString xs = do
    eShaders <- sequence <$> mapM compileShader xs
    case eShaders of
        Right shaders -> linkShaders shaders
        Left err      -> return $ Left err

-- | Compile and link the provided shader sources to a program.
fromFile :: MonadIO m => [(ShaderType, FilePath)] -> m (Either String Program)
fromFile xs = do
    eByteStrings <- sequence <$> mapM (tryReadFile . snd) xs
    case eByteStrings of
        Right byteStrings -> fromByteString $ combine xs byteStrings
        Left err          -> return $ Left (show err)
    where
        combine = zipWith (\(s, p) bs -> (s, p, bs))

tryReadFile :: MonadIO m => FilePath -> m (Either SomeException ByteString)
tryReadFile file = liftIO $ try (BS.readFile file)

-- | Delete the shader program.
delete :: MonadIO m => Program -> m ()
delete = GLResources.deleteProgram

-- | Enable the program to become current.
enable :: MonadIO m => Program -> m ()
enable (Program program) = GL.glUseProgram program

-- | Disable the current program.
disable :: MonadIO m => m ()
disable = GL.glUseProgram 0

-- | Get the named uniform location.
getUniformLocation :: MonadIO m => Program -> String -> m Location
getUniformLocation (Program program) loc =
    Location <$>
        liftIO (withCString loc $ \cloc -> GL.glGetUniformLocation program cloc)

-- | Compile a single shader.
compileShader :: MonadIO m => (ShaderType, FilePath, ByteString)
              -> m (Either String Shader)
compileShader (shaderType, filePath, byteString) = do
    shader@(Shader handle) <- GLResources.createShader shaderType
    setShaderSource handle byteString
    GL.glCompileShader handle
    status <- getShaderStatus $ GL.glGetShaderiv handle GL.GL_COMPILE_STATUS
    if status == GL.GL_TRUE
        then return $ Right shader
        else do
            errLog <- getInfoLog handle GL.glGetShaderInfoLog
            return $ Left (filePath ++ ": " ++ errLog)

-- | Link shaders to a program.
linkShaders :: MonadIO m => [Shader] -> m (Either String Program)
linkShaders shaders = do
    program@(Program handle) <- GLResources.createProgram
    mapM_ (\(Shader shader) -> GL.glAttachShader handle shader) shaders
    GL.glLinkProgram handle
    status <- getShaderStatus $ GL.glGetProgramiv handle GL.GL_LINK_STATUS
    if status == GL.GL_TRUE
        then do
            mapM_ (\(Shader shader) -> GL.glDetachShader handle shader) shaders
            mapM_ GLResources.deleteShader shaders
            return $ Right program
        else do
            errLog <- getInfoLog handle GL.glGetProgramInfoLog
            mapM_ GLResources.deleteShader shaders
            delete program
            return $ Left errLog

-- | Set the shader source code.
setShaderSource :: MonadIO m => GLuint -> ByteString -> m ()
setShaderSource handle src = liftIO $
    BS.useAsCString src $ \cstring ->
        with cstring $ \ptr ->
            GL.glShaderSource handle 1 ptr nullPtr

-- | Read the shader status.
getShaderStatus :: MonadIO m => (Ptr GLint -> IO ()) -> m GLboolean
getShaderStatus getter = liftIO $
    with 0 $ \ptr -> do
        getter ptr
        v <- peek ptr
        if v == 0
            then return GL.GL_FALSE
            else return GL.GL_TRUE

-- | Get the info log using the provided getter (shader or program).
getInfoLog :: MonadIO m => GLuint
           -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
           -> m String
getInfoLog handle getter = liftIO $ do
    let str = Prelude.replicate 500 '\0'
    withCString str $ \ptr -> do
        getter handle 500 nullPtr ptr
        peekCString ptr
