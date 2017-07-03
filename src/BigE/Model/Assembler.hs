-- |
-- Module: BigE.Model.Assembler
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module BigE.Model.Assembler
    ( assembleVertP
    ) where

import qualified BigE.Attribute.Vert_P     as VertP
import           BigE.Model.IndexAllocator (AllocReply (..), IndexAllocator)
import qualified BigE.Model.IndexAllocator as Alloc
import           BigE.Model.Parser         (FilePart (..), Point (..),
                                            splitParts)
import           Control.Monad             (foldM)
import           Data.Vector               (Vector, fromList, (!?))
import           Graphics.GL               (GLfloat, GLuint)
import           Linear                    (V2 (..), V3 (..))

data AssemblyState a = AssemblyState
    { positions :: !(Vector (V3 GLfloat))
    , normals   :: !(Vector (V3 GLfloat))
    , texCoords :: !(Vector (V2 GLfloat))
    , allocator :: !IndexAllocator
    , vertices  :: ![a]
    , indices   :: ![GLuint]
    } deriving Show

type VerticeMaker a = AssemblyState a -> Point -> Maybe (AssemblyState a)

-- | Assemble the 'FilePart's to a list of vertices and indices.
assembleVertP :: [FilePart] -> Maybe ([VertP.Vertex], [GLuint])
assembleVertP = assembleVertices vertP

-- | Traverse the list of 'FilePart', and for each triangle travserse the
-- face points that build it up.
assembleVertices :: VerticeMaker a -> [FilePart] -> Maybe ([a], [GLuint])
assembleVertices maker parts = do
    let (inputState, triangles) = mkAssemblyState parts
    outputState <- foldM traverseTriangle inputState triangles
    return (reverse $ vertices outputState, reverse $ indices outputState)
    where
        traverseTriangle state (Triangle p1 p2 p3) =
            foldM maker state [p1, p2, p3]

        traverseTriangle _ _ = Nothing

-- Rules for vertice assemblers:
-- 1. Lookup index for the point. The index is always emitted.
-- 2. If the index is a newly allocated index, construct the vertex and
-- emit vertex and index.

vertP :: AssemblyState VertP.Vertex -> Point -> Maybe (AssemblyState VertP.Vertex)
vertP state p@(Point posI _ _) =
    case Alloc.alloc p (allocator state) of
        Index i allocator' -> do
            let positions' = positions state
            pos <- positions' !? ind posI
            let vert = VertP.Vertex { VertP.position = pos }
            return $ state { vertices = vert:vertices state
                           , indices = i:indices state
                           , allocator = allocator'
                           }
        AlreadyIndexed i   ->
            return $ state { indices = i:indices state }

-- | Make an assembly state for the parameterixed Vertex type a.
mkAssemblyState :: [FilePart] -> (AssemblyState a, [FilePart])
mkAssemblyState parts =
    let (verts, norms, coords, triangles) = splitParts parts
        state =
            AssemblyState
                { positions = fromList $ map (\(Vertex v) -> v) verts
                , normals = fromList $ map (\(Normal n) -> n) norms
                , texCoords = fromList $ map (\(TexCoord c) -> c) coords
                , allocator = Alloc.init
                , vertices = []
                , indices = []
                }
    in (state, triangles)

ind :: Int -> Int
ind n = n - 1
