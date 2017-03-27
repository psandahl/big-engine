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
    ) where

import           Control.Concurrent.STM           (newTVarIO)
import           Control.Monad                    (unless, when)
import           Control.Monad.Except             (runExceptT, throwError)
import           Data.Maybe                       (isNothing)
import           Graphics.BigEngine.Configuration
import           Graphics.BigEngine.Render        (RenderState (..), liftIO)
import           Graphics.UI.GLFW                 (OpenGLProfile (..),
                                                   VideoMode (..), Window,
                                                   WindowHint (..))
import qualified Graphics.UI.GLFW                 as GLFW

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

            win' <- liftIO $ GLFW.createWindow width height (windowCaption config) (Just monitor') Nothing
            maybe (throwError "Cannot create window") (\ww -> return (ww, width, height)) win'

        SizedScreen (width, height) -> do
            win' <- liftIO $ GLFW.createWindow width height (windowCaption config) Nothing Nothing
            maybe (throwError "Cannot create window") (\ww -> return (ww, width, height)) win'

    liftIO $ GLFW.makeContextCurrent (Just win)

    appState' <- liftIO $ newTVarIO app

    -- Setting viewport.

    return RenderState {window = win, appState = appState' }
