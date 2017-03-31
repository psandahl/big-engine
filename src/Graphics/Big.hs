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
    , displayDimension
    , getAppState
    , putAppState
    , modifyAppState
    , setWindowSizeCallback
    , module Graphics.Big.Mesh
    , module Graphics.Big.Program
    , module Graphics.Big.Types
    ) where

import           Control.Monad              (void)
import           Data.IORef                 (modifyIORef, newIORef)
import           Graphics.Big.Callback      (initCallbacks)
import           Graphics.Big.Configuration (Configuration (..),
                                             DisplayMode (..))
import           Graphics.Big.Display       (initDisplay, renderLoop)
import           Graphics.Big.Mesh
import           Graphics.Big.Program
import           Graphics.Big.Render        (Render, RenderState (..),
                                             WindowSizeCallback,
                                             displayDimension, frameDuration,
                                             getAppState, modifyAppState,
                                             putAppState, runRender,
                                             setWindowSizeCallback)
import           Graphics.Big.Types

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

            -- Make a mutable reference of the state.
            ref <- newIORef state

            -- Application level initialization - preamble.
            initResult <- runRender (setup config) ref
            case initResult of
                Right () -> do

                    -- Reset the timing values in the state.
                    modifyIORef ref $ \s -> s { lastTime = 0
                                              , duration = 0
                                              }

                    -- Initialize callbacks.
                    initCallbacks ref

                    -- Run the render loop until request to stop.
                    renderLoop ref (eachFrame config)

                    -- Final stage. Run the postamble.
                    void $ runRender (teardown config) ref

                    return $ Right ()

                -- Error. Unsucessful preamble.
                Left err -> return $ Left err

        -- Error. Cannot initialize display.
        Left err -> return $ Left err
