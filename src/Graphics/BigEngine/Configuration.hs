-- |
-- Module: Graphics.BigEngine.Configuration
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.BigEngine.Configuration
    ( Configuration (..)
    , DisplayMode (..)
    ) where

import           Graphics.BigEngine.Render (Render)

data DisplayMode
    = FullScreen
    | SizedScreen !(Int, Int)
    deriving Show

data Configuration app = Configuration
    { versionMajor  :: !Int
    , versionMinor  :: !Int
    , displayMode   :: !DisplayMode
    , windowCaption :: !String
    , prepare       :: !(Render app ())
    , renderFrame   :: !(Render app ())
    }
