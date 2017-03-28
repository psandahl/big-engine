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
    , runRender
    , frameDuration
    , displayDimension
    , getAppState
    , putAppState
    , modifyAppState
    , liftIO
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Data.IORef             (IORef, modifyIORef, readIORef)
import           Graphics.UI.GLFW       (Window)

-- | BigEngine's internal render state.
data RenderState app = RenderState
    { window    :: !Window
    , dimension :: !(Int, Int)
    , lastTime  :: !Double
    , duration  :: !Double
    , appState  :: !app
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

readIORef' :: IORef (RenderState app) -> Render app (RenderState app)
readIORef' = liftIO . readIORef

modifyIORef' :: IORef (RenderState app) -> (RenderState app -> RenderState app) -> Render app ()
modifyIORef' ref = liftIO . modifyIORef ref
