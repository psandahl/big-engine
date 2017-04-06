-- |
-- Module: Graphics.Big
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
--
-- A Batteries Included Graphics Engine.
module Graphics.Big
    ( Configuration (..)
    , DisplayMode (..)
    , Render
    , WindowSizeCallback
    , runEngine
    , frameDuration
    , displayDimensions
    , getAppState
    , getAppStateUnsafe
    , putAppState
    , modifyAppState
    , setWindowSizeCallback
    , bindTexture2D
    , disableTexture2D
    , deleteTexture
    , module Graphics.Big.Mesh
    , module Graphics.Big.Program
    , module Graphics.Big.TextureLoader
    , module Graphics.Big.Types
    ) where

import           Control.Monad              (void)
import           Data.IORef                 (modifyIORef, newIORef)
import           Graphics.Big.Callback      (initCallbacks)
import           Graphics.Big.Configuration (Configuration (..),
                                             DisplayMode (..))
import           Graphics.Big.Display       (initDisplay, renderLoop)
import           Graphics.Big.GLResources   (bindTexture2D, deleteTexture,
                                             disableTexture2D)
import           Graphics.Big.Mesh
import           Graphics.Big.Program
import           Graphics.Big.Render        (Render, RenderState (..),
                                             WindowSizeCallback,
                                             displayDimensions, frameDuration,
                                             getAppState, getAppStateUnsafe,
                                             modifyAppState, putAppState,
                                             runRender, setWindowSizeCallback)
import           Graphics.Big.TextureLoader
import           Graphics.Big.Types

-- | Run the engine. Provided is the 'Configuration'. The application's
-- setup return an initial state, if successful.
-- The state will then be carried through all stages (animate, render and teardown).
-- All stages, and all callbacks, are guaranteed to be performed in the
-- same thread.
runEngine :: Configuration app -> IO (Either String ())
runEngine config = do

    -- Initalization stage, create the display according to configuration.
    eState <- initDisplay config
    case eState of
        Right state -> do

            -- Make a mutable reference of the state.
            ref <- newIORef state

            -- Application level initialization - setup.
            eAppState <- runRender (setup config) ref
            case eAppState of
                Right appState' -> do

                    -- Reset the timing values, and set the appState.
                    modifyIORef ref $ \s -> s { lastTime = 0
                                              , duration = 0
                                              , appState = Just appState'
                                              }

                    -- Initialize callbacks.
                    initCallbacks ref

                    -- Run the render loop until requested to stop.
                    renderLoop ref config

                    -- Final stage. Run the teardown.
                    void $ runRender (teardown config) ref

                    return $ Right ()

                -- Error. Unsucessful preamble.
                Left err -> return $ Left err

        -- Error. Cannot initialize display.
        Left err -> return $ Left err
