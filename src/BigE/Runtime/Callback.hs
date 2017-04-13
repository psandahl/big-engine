-- |
-- Module: BigE.Runtime.Callback
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.Runtime.Callback
    ( initCallbacks
    ) where

import           BigE.Runtime.Render (RenderState (..), runRender)
import           Control.Monad       (when)
import           Data.IORef          (IORef, modifyIORef, readIORef)
import           Data.Maybe          (isJust)
import qualified Graphics.GL         as GL
import           Graphics.UI.GLFW    (Key, KeyState (..), ModifierKeys, Window)
import qualified Graphics.UI.GLFW    as GLFW

-- | Init GLFW callbacks.
initCallbacks :: IORef (RenderState app) -> IO ()
initCallbacks ref = do
    state <- readIORef ref
    GLFW.setWindowSizeCallback (window state) $ Just (windowSizeCallback ref)
    GLFW.setKeyCallback (window state) $ Just (keyCallback ref)

windowSizeCallback :: IORef (RenderState app) -> Window -> Int -> Int -> IO ()
windowSizeCallback ref _window width height = do
    -- Adjust the viewport with the new size values.
    GL.glViewport 0 0 (fromIntegral width) (fromIntegral height)

    -- Store the values in the state.
    modifyIORef ref $ \state -> state { dimensions = (width, height) }

    -- If the application callback is set, call it.
    state <- readIORef ref
    case appWindowSizeCallback state of
        Just cb -> runRender (cb width height) ref
        Nothing -> return ()

keyCallback :: IORef (RenderState app) -> Window -> Key
            -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback ref _window key _code keyState modKeys = do
    state <- readIORef ref

    case keyState of
        KeyState'Pressed   ->
            when (isJust (appKeyPressedCallback state)) $ do
                let Just cb = appKeyPressedCallback state
                runRender (cb key modKeys) ref

        KeyState'Released  ->
            when (isJust (appKeyReleasedCallback state)) $ do
                let Just cb = appKeyReleasedCallback state
                runRender (cb key modKeys) ref

        KeyState'Repeating ->
            when (isJust (appKeyRepeatingCallback state)) $ do
                let Just cb = appKeyRepeatingCallback state
                runRender (cb key modKeys) ref
