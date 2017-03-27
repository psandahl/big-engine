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

-- | The modes in which the display can be open.
data DisplayMode
    = FullScreen
      -- ^ Fullscreen mode.
    | SizedScreen !(Int, Int)
      -- ^ Windowed mode with the requested size.
    deriving Show

-- | Configuration data for the Big Engine.
data Configuration app = Configuration
    { versionMajor  :: !Int
      -- ^ The requested major version of OpenGL.

    , versionMinor  :: !Int
      -- ^ The requested minor version of OpenGL.

    , displayMode   :: !DisplayMode
      -- ^ The requested display model

    , windowCaption :: !String
      -- ^ The text caption for the window.

    , preamble      :: !(Render app (Either String ()))
      -- ^ Preamble callback executed before the render loop starts.

    , frame         :: !(Render app ())
      -- ^ Render one frame in the scene.

    , postamble     :: !(Render app ())
      -- ^ Postamble callback executed after the render loop has terminated.
    }
