-- |
-- Module: BigE.Model.IndexAllocator
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module BigE.Model.IndexAllocator
    ( IndexAllocator
    , AllocReply (..)
    , init
    , alloc
    ) where

import           BigE.Model.Parser   (Point)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Graphics.GL         (GLuint)
import           Prelude             hiding (init)

-- | Allocate indices to vertex points.
data IndexAllocator = IndexAllocator
    { hashMap   :: !(HashMap Point GLuint)
    , nextIndex :: !GLuint
    } deriving Show

-- | Allocation reply.
data AllocReply
    = Index !GLuint !IndexAllocator
    | AlreadyIndexed !GLuint !IndexAllocator
    deriving Show

-- | Get a new allocator.
init :: IndexAllocator
init =
    IndexAllocator
        { hashMap = HashMap.empty
        , nextIndex = 0
        }

-- | Get an index for the point.
alloc :: Point -> IndexAllocator -> AllocReply
alloc point allocator =
    case HashMap.lookup point (hashMap allocator) of
        Just index -> AlreadyIndexed index allocator
        Nothing ->
            let newHashMap = HashMap.insert point
                                            (nextIndex allocator)
                                            (hashMap allocator)
                newAllocator = allocator { hashMap = newHashMap
                                         , nextIndex = nextIndex allocator + 1
                                         }
            in Index (nextIndex allocator) newAllocator
