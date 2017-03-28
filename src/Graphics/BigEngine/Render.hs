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
import           Control.Monad.State    (MonadState, StateT, get, modify,
                                         runStateT)
import           Graphics.UI.GLFW       (Window)

-- | BigEngine's internal render state.
data RenderState app = RenderState
    { window    :: !Window
    , dimension :: !(Int, Int)
    , lastTime  :: !Double
    , duration  :: !Double
    , appState  :: !app
    }

--  | The Render monad transform. Just state on top of IO.
newtype Render app reply =
    Render { extractRender :: StateT (RenderState app) IO reply }
    deriving (Functor, Applicative, Monad, MonadState (RenderState app), MonadIO)

-- | Run the Render monad action.
runRender :: Render app reply -> RenderState app -> IO (reply, RenderState app)
runRender action = runStateT (extractRender action)

-- | The time in (fractional) seconds since last frame.
frameDuration :: Render app Double
frameDuration = duration <$> get

-- | The display dimension as (width, height).
displayDimension :: Render app (Int, Int)
displayDimension = dimension <$> get

-- | Read the app state.
getAppState :: Render app app
getAppState = appState <$> get

-- | Put a new app state.
putAppState :: app -> Render app ()
putAppState app = modify $ \state -> state { appState = app }

-- | Modify the app state.
modifyAppState :: (app -> app) -> Render app ()
modifyAppState g = modify $ \state -> state { appState = g $ appState state }
