{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: Graphics.BigEngine.Render
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.BigEngine.Render
    ( Render
    , RenderState (..)
    , WindowSizeCallback
    , runRender
    , frameDuration
    , displayDimension
    , getAppState
    , putAppState
    , modifyAppState
    , setWindowSizeCallback
    , liftIO
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Data.IORef             (IORef, modifyIORef, readIORef)
import           Graphics.UI.GLFW       (Window)

type WindowSizeCallback app = Int -> Int -> Render app ()

-- | BigEngine's internal render state.
data RenderState app = RenderState
    { window                :: !Window
      -- ^ The GLFW window.

    , dimension             :: !(Int, Int)
      -- ^ The current dimensions for the window (width, height).

    , lastTime              :: !Double
      -- ^ Last taken timestamp value.

    , duration              :: !Double
      -- ^ The duration in (fractional) seconds since previous frame.

    , appWindowSizeCallback :: !(Maybe (WindowSizeCallback app))
      -- ^ The application's window size change callback.

    , appState              :: !app
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

-- | The display dimension as (width, height).
displayDimension :: Render app (Int, Int)
displayDimension = dimension <$> (readIORef' =<< ask)

-- | Read the app state.
getAppState :: Render app app
getAppState = appState <$> (readIORef' =<< ask)

-- | Put a new app state.
putAppState :: app -> Render app ()
putAppState app = do
    ref <- ask
    modifyIORef' ref $ \state -> state { appState = app }

-- | Modify the app state.
modifyAppState :: (app -> app) -> Render app ()
modifyAppState g = do
    ref <- ask
    modifyIORef' ref $ \state -> state { appState = g (appState state) }

-- | Set (or unset) the 'WindowSizeCallback'.
setWindowSizeCallback :: Maybe (WindowSizeCallback app) -> Render app ()
setWindowSizeCallback val = do
    ref <- ask
    modifyIORef' ref $ \state -> state { appWindowSizeCallback = val }

readIORef' :: IORef (RenderState app) -> Render app (RenderState app)
readIORef' = liftIO . readIORef

modifyIORef' :: IORef (RenderState app) -> (RenderState app -> RenderState app) -> Render app ()
modifyIORef' ref = liftIO . modifyIORef ref
