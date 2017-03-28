-- |
-- Module: Graphics.BigEngine
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
--
-- A Batteries Included Graphics Engine.
module Graphics.BigEngine
    ( Configuration (..)
    , DisplayMode (..)
    , Render
    , runEngine
    , frameDuration
    , displayDimension
    , getAppState
    , putAppState
    , modifyAppState
    ) where

import           Control.Monad                    (void)
import           Data.IORef                       (newIORef, readIORef)
import           Graphics.BigEngine.Callback      (initCallbacks)
import           Graphics.BigEngine.Configuration (Configuration (..),
                                                   DisplayMode (..))
import           Graphics.BigEngine.Display       (initDisplay, renderLoop)
import           Graphics.BigEngine.Render        (Render, RenderState (..),
                                                   displayDimension,
                                                   frameDuration, getAppState,
                                                   modifyAppState, putAppState,
                                                   runRender)

-- | Run the engine. Provided are the 'Configuration' and the application's
-- empty state. The state will then be carried through all stages (preamble,
-- frame and postable).
-- All stages, and all callbacks, are guaranteed to be performed in the
-- same thread.
runEngine :: Configuration app -> app -> IO (Either String ())
runEngine config app = do

    -- Initalization stage, create the display according to configuration.
    eState <- initDisplay config app
    case eState of
        Right state -> do

            -- Application level initialization - preamble.
            (initResult, newState) <- runRender (preamble config) state
            case initResult of
                Right () -> do

                    -- Make a mutable reference for the state. Needed for
                    -- the callbacks. Reset the timing values in the state
                    ref <- newIORef $ newState { lastTime = 0
                                               , duration = 0
                                               }

                    -- Initialize callbacks.
                    initCallbacks ref

                    -- Run the render loop until request to stop.
                    renderLoop ref (frame config)

                    -- Fetch the last state and run the postamble.
                    lastState <- readIORef ref
                    void $ runRender (postamble config) lastState

                    return $ Right ()

                -- Error. Unsucessful preamble.
                Left err -> return $ Left err

        -- Error. Cannot initialize display.
        Left err -> return $ Left err
