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
    ( runEngine
    ) where

import           Control.Monad                    (void)
import           Data.IORef                       (newIORef, readIORef)
import           Graphics.BigEngine.Callback      (initCallbacks)
import           Graphics.BigEngine.Configuration (Configuration (..))
import           Graphics.BigEngine.Display       (initDisplay, renderLoop)
import           Graphics.BigEngine.Render        (runRender)

runEngine :: Configuration app -> app -> IO (Either String ())
runEngine config app = do
    eState <- initDisplay config app
    case eState of
        Right state -> do
            (initResult, newState) <- runRender (preamble config) state
            case initResult of
                Right () -> do
                    ref <- newIORef newState
                    initCallbacks ref
                    renderLoop ref (frame config)
                    lastState <- readIORef ref
                    void $ runRender (postamble config) lastState

                    return $ Right ()

                Left err -> return $ Left err

        Left err -> return $ Left err
