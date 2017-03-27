-- |
-- Module: Graphics.BigEngine.Display
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.BigEngine.Display
    ( initDisplay
    , renderLoop
    ) where

import           Control.Monad                    (unless, when)
import           Control.Monad.Except             (runExceptT, throwError)
import           Data.IORef                       (IORef, readIORef, writeIORef)
import           Data.Maybe                       (isJust, isNothing)
import           Graphics.BigEngine.Configuration
import           Graphics.BigEngine.Render        (Render, RenderState (..),
                                                   liftIO, runRender)
import qualified Graphics.GL                      as GL
import           Graphics.UI.GLFW                 (OpenGLProfile (..),
                                                   VideoMode (..),
                                                   WindowHint (..))
import qualified Graphics.UI.GLFW                 as GLFW

-- | Initialize the display from the 'Configuration'. If successful a
-- 'RenderState' is returned.
initDisplay :: Configuration app -> app -> IO (Either String (RenderState app))
initDisplay config app = runExceptT $ do
    initSuccess <- liftIO GLFW.init
    unless (initSuccess) $
        throwError "GLFW initialization failed"

    liftIO (GLFW.windowHint $ WindowHint'Resizable True)
    liftIO (GLFW.windowHint $ WindowHint'Samples 4)
    liftIO (GLFW.windowHint $ WindowHint'ContextVersionMajor (versionMajor config))
    liftIO (GLFW.windowHint $ WindowHint'ContextVersionMinor (versionMinor config))
    liftIO (GLFW.windowHint $ WindowHint'OpenGLForwardCompat True)
    liftIO (GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core)

    (win, width, height) <- case displayMode config of
        FullScreen -> do
            monitor <- liftIO $ GLFW.getPrimaryMonitor
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
        , duration = 0
        , appState = app
        }

-- | The render loop. Run frame by frame until a close of the display is
-- requested.
renderLoop :: IORef (RenderState app) -> Render app () -> IO ()
renderLoop ref action = go
    where
        go :: IO ()
        go = do
            -- Poll events and execute all callbacks.
            GLFW.pollEvents

            -- Read out the 'RenderState'.
            state <- readIORef ref

            -- Read the new timestamp (ms offset to start of GLFW).
            now <- GLFW.getTime
            when (isJust now) $ do
                let Just now' = now

                -- Render the frame. The state might have been changed by
                -- the application.
                ((), newState) <- runRender action $
                                    state { duration = now' - duration state }

                -- Swap off-screen and screen buffers.
                GLFW.swapBuffers (window newState)

                -- Save the new state for the next iteration.
                writeIORef ref newState

                -- Check if a window close is requested. Otherwise continue one
                -- more iteration.
                shallClose <- GLFW.windowShouldClose (window newState)
                unless shallClose go
