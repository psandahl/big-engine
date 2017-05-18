{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: BigE.Runtime.Render
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module BigE.Runtime.Render
    ( Render
    , RenderState (..)
    , Key (..)
    , ModifierKeys (..)
    , MouseButton (..)
    , WindowSizeCallback
    , KeyPressedCallback
    , KeyReleasedCallback
    , KeyRepeatingCallback
    , MousePressedCallback
    , MouseReleasedCallback
    , runRender
    , frameDuration
    , displayDimensions
    , getAppState
    , getAppStateUnsafe
    , putAppState
    , modifyAppState
    , setWindowSizeCallback
    , setKeyPressedCallback
    , setKeyRepeatingCallback
    , setKeyReleasedCallback
    , setMousePressedCallback
    , setMouseReleasedCallback
    , liftIO
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Data.IORef             (IORef, modifyIORef, readIORef)
import           Data.Maybe             (fromJust)
import           Graphics.UI.GLFW       (Key (..), ModifierKeys (..),
                                         MouseButton (..), Window)

type WindowSizeCallback app = Int -> Int -> Render app ()
type KeyPressedCallback app = Key -> ModifierKeys -> Render app ()
type KeyRepeatingCallback app = Key -> ModifierKeys -> Render app ()
type KeyReleasedCallback app = Key -> ModifierKeys -> Render app ()
type MousePressedCallback app = MouseButton -> ModifierKeys -> (Int, Int) -> Render app ()
type MouseReleasedCallback app = MouseButton -> ModifierKeys -> (Int, Int) -> Render app ()

-- | BigEngine's internal render state.
data RenderState app = RenderState
    { window                   :: !Window
      -- ^ The GLFW window.

    , dimensions               :: !(Int, Int)
      -- ^ The current dimensions for the window (width, height).

    , lastTime                 :: !Double
      -- ^ Last taken timestamp value.

    , duration                 :: !Double
      -- ^ The duration in (fractional) seconds since previous frame.

    , appWindowSizeCallback    :: !(Maybe (WindowSizeCallback app))
      -- ^ The application's window size change callback.

    , appKeyPressedCallback    :: !(Maybe (KeyPressedCallback app))
      -- ^ The application's key pressed callback.

    , appKeyRepeatingCallback  :: !(Maybe (KeyRepeatingCallback app))
      -- ^ The application's key repeating callback.

    , appKeyReleasedCallback   :: !(Maybe (KeyReleasedCallback app))
      -- ^ The application's key released callback.

    , appMousePressedCallback  :: !(Maybe (MousePressedCallback app))
      -- ^ The applications mouse pressed callback.

    , appMouseReleasedCallback :: !(Maybe (MouseReleasedCallback app))

    , appState                 :: !(Maybe app)
      -- ^ The user provided application state.
    }

--  | The Render monad transform. Just 'StateT' on top of IO.
newtype Render app reply =
    Render { extractRender :: ReaderT (IORef (RenderState app)) IO reply }
    deriving (Functor, Applicative, Monad, MonadReader (IORef (RenderState app)), MonadIO)

-- | Run the Render monad action.
runRender :: Render app reply -> IORef (RenderState app) -> IO reply
runRender action = runReaderT (extractRender action)

-- | The time in (fractional) seconds since last frame.
frameDuration :: Render app Double
frameDuration = duration <$> (readIORef' =<< ask)

-- | The display dimensions as (width, height).
displayDimensions :: Render app (Int, Int)
displayDimensions = dimensions <$> (readIORef' =<< ask)

-- | Read the app state.
getAppState :: Render app (Maybe app)
getAppState = appState <$> (readIORef' =<< ask)

-- | Read the app state and force away Maybe. Unsafe, but shall normally be
-- ok in eachFrame and teardown callbacks.
getAppStateUnsafe :: Render app app
getAppStateUnsafe = fromJust . appState <$> (readIORef' =<< ask)

-- | Put a new app state.
putAppState :: app -> Render app ()
putAppState app = do
    ref <- ask
    modifyIORef' ref $ \state -> state { appState = Just app }

-- | Modify the app state.
modifyAppState :: (app -> app) -> Render app ()
modifyAppState g = do
    ref <- ask
    modifyIORef' ref $ \state ->
        maybe state (\appState' -> state { appState = Just (g appState') }) (appState state)

-- | Set (or unset) the 'WindowSizeCallback'.
setWindowSizeCallback :: Maybe (WindowSizeCallback app) -> Render app ()
setWindowSizeCallback val = do
    ref <- ask
    modifyIORef' ref $ \state -> state { appWindowSizeCallback = val }

-- | Set (or unset) the 'KeyPressedCallback'.
setKeyPressedCallback :: Maybe (KeyPressedCallback app) -> Render app ()
setKeyPressedCallback val = do
    ref <- ask
    modifyIORef' ref $ \state -> state { appKeyPressedCallback = val }

-- | Set (or unset) the 'KeyRepeatingCallback'.
setKeyRepeatingCallback :: Maybe (KeyRepeatingCallback app) -> Render app ()
setKeyRepeatingCallback val = do
    ref <- ask
    modifyIORef' ref $ \state -> state { appKeyRepeatingCallback = val }

-- | Set (or unset) the 'KeyReleasedCallback'.
setKeyReleasedCallback :: Maybe (KeyReleasedCallback app) -> Render app ()
setKeyReleasedCallback val = do
    ref <- ask
    modifyIORef' ref $ \state -> state { appKeyReleasedCallback = val }

-- | Set (or unset) the 'MousePressedCallback'.
setMousePressedCallback :: Maybe (MousePressedCallback app) -> Render app ()
setMousePressedCallback val = do
    ref <- ask
    modifyIORef' ref $ \state -> state { appMousePressedCallback = val }

-- | Set (or unset) the 'MouseReleasedCallback'.
setMouseReleasedCallback :: Maybe (MouseReleasedCallback app) -> Render app ()
setMouseReleasedCallback val = do
    ref <- ask
    modifyIORef' ref $ \state -> state { appMouseReleasedCallback = val }

readIORef' :: IORef (RenderState app) -> Render app (RenderState app)
readIORef' = liftIO . readIORef

modifyIORef' :: IORef (RenderState app) -> (RenderState app -> RenderState app) -> Render app ()
modifyIORef' ref = liftIO . modifyIORef ref
