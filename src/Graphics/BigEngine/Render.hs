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
    , liftIO
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State    (MonadState, StateT, runStateT)
import           Graphics.UI.GLFW       (Window)

-- | BigEngine's internal render state.
data RenderState app = RenderState
    { window    :: !Window
    , dimension :: !(Int, Int)
    , duration  :: !Double
    , appState  :: !app
    }

newtype Render app reply =
    Render { extractRender :: StateT (RenderState app) IO reply }
    deriving (Functor, Applicative, Monad, MonadState (RenderState app), MonadIO)

runRender :: Render app reply -> RenderState app -> IO (reply, RenderState app)
runRender action = runStateT (extractRender action)
