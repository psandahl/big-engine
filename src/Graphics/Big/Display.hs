-- |
-- Module: Graphics.Big.Display
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.Big.Display
    ( initDisplay
    , renderLoop
    ) where

import           Control.Monad              (unless, void, when)
import           Control.Monad.Except       (runExceptT, throwError)
import           Data.IORef                 (IORef, modifyIORef, readIORef)
import           Data.Maybe                 (isJust, isNothing)
import           Graphics.Big.Configuration
import           Graphics.Big.Render        (Render, RenderState (..), liftIO,
                                             runRender)
import qualified Graphics.GL                as GL
import           Graphics.UI.GLFW           (OpenGLProfile (..), VideoMode (..),
                                             Window, WindowHint (..))
import qualified Graphics.UI.GLFW           as GLFW

-- | Initialize the display from the 'Configuration'. If successful a
-- 'RenderState' is returned.
initDisplay :: Configuration app -> IO (Either String (RenderState app))
initDisplay config = runExceptT $ do
    initSuccess <- liftIO GLFW.init
    unless initSuccess $
        throwError "GLFW initialization failed"

    liftIO (GLFW.windowHint $ WindowHint'Resizable True)
    liftIO (GLFW.windowHint $ WindowHint'Samples 4)
    liftIO (GLFW.windowHint $ WindowHint'ContextVersionMajor (versionMajor config))
    liftIO (GLFW.windowHint $ WindowHint'ContextVersionMinor (versionMinor config))
    liftIO (GLFW.windowHint $ WindowHint'OpenGLForwardCompat True)
    liftIO (GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core)

    (win, width, height) <- case displayMode config of
        FullScreen -> do
            monitor <- liftIO GLFW.getPrimaryMonitor
            when (isNothing monitor) $
                throwError "Cannot get hold of primary monitor"
            let Just monitor' = monitor

            mode <- liftIO $ GLFW.getVideoMode monitor'
            when (isNothing mode) $
                throwError "Cannot get hold of monitor's video mode"
            let Just mode' = mode
                width = videoModeWidth mode'
                height = videoModeHeight mode'

            win' <- liftIO $ GLFW.createWindow width height (windowCaption config)
                                 (Just monitor') Nothing
            maybe (throwError "Cannot create window")
                  (\ww -> return (ww, width, height)) win'

        SizedScreen (width, height) -> do
            win' <- liftIO $ GLFW.createWindow width height
                                 (windowCaption config) Nothing Nothing
            maybe (throwError "Cannot create window")
                  (\ww -> return (ww, width, height)) win'

    -- Activate the display.
    liftIO $ GLFW.makeContextCurrent (Just win)

    -- Setting viewport.
    GL.glViewport 0 0 (fromIntegral width) (fromIntegral height)

    -- Create and return the 'RenderState'. The application is ready to go.
    return RenderState
        { window = win
        , dimension = (width, height)
        , lastTime = 0
        , duration = 0
        , appWindowSizeCallback = Nothing
        , appState = Nothing
        }

-- | The render loop. Run frame by frame until a close of the display is
-- requested.
renderLoop :: IORef (RenderState app) -> Render app () -> IO ()
renderLoop ref action = go =<< (window <$> readIORef ref)
    where
        go :: Window -> IO ()
        go win = do
            -- Read the new timestamp (second offset to start of GLFW).
            now <- GLFW.getTime
            when (isJust now) $ do
                let Just now' = now

                -- Update the time values.
                modifyIORef ref $ \state ->
                    state { duration = now' - lastTime state
                          , lastTime = now'
                      }

                -- Render the frame.
                void $ runRender action ref

                -- Swap off-screen and screen buffers.
                GLFW.swapBuffers win

                -- Poll events and execute all callbacks.
                GLFW.pollEvents

                -- Check if a window close is requested. Otherwise continue one
                -- more iteration.
                shallClose <- GLFW.windowShouldClose win
                unless shallClose $ go win
