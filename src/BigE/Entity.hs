{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Module: BigE.Entity
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.Entity
    ( Entity (..)
    , Renderable (..)
    ) where

import           BigE.Runtime.Render (Render)
import           Graphics.GL         (GLfloat)
import           Linear              (M44)

-- | An abstract interface for something that can be rendered. For use in e.g.
-- render lists.
class Entity a where
    -- | Get the numeric id for the entity. Can be used for e.g. mouse picking.
    objectId :: a -> Int

    -- | Render the entity. Given only the perspective-view matrix as the
    -- only mandatory argument.
    render :: M44 GLfloat -> a -> Render app ()

-- | A "tag" usable for render lists.
data Renderable = forall a. Entity a => Renderable a
