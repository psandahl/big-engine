-- |
-- Module: Graphics.BigEngine.Callback
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.BigEngine.Callback
    ( initCallbacks
    ) where

import           Data.IORef                (IORef, modifyIORef, readIORef)
import           Graphics.BigEngine.Render (RenderState (..))
import qualified Graphics.GL               as GL
import           Graphics.UI.GLFW          (Window)
import qualified Graphics.UI.GLFW          as GLFW

-- | Init GLFW callbacks.
initCallbacks :: IORef (RenderState app) -> IO ()
initCallbacks ref = do
    state <- readIORef ref
    GLFW.setWindowSizeCallback (window state) $ Just (windowSizeCallback ref)

windowSizeCallback :: IORef (RenderState app) -> Window -> Int -> Int -> IO ()
windowSizeCallback ref _window width height = do
    GL.glViewport 0 0 (fromIntegral width) (fromIntegral height)
    modifyIORef ref $ \s -> s { dimension = (width, height) }
