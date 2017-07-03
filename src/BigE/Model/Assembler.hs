-- |
-- Module: BigE.Model.Assembler
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module BigE.Model.Assembler
    ( assembleVertP
    , assembleVertPN
    , assembleVertPNTx
    ) where

import qualified BigE.Attribute.Vert_P      as Vert_P
import qualified BigE.Attribute.Vert_P_N    as Vert_P_N
import qualified BigE.Attribute.Vert_P_N_Tx as Vert_P_N_Tx
import           BigE.Model.IndexAllocator  (AllocReply (..), IndexAllocator)
import qualified BigE.Model.IndexAllocator  as Alloc
import           BigE.Model.Parser          (FilePart (..), Point (..),
                                             splitParts)
import           Control.Monad              (foldM)
import           Data.Vector                (Vector, fromList, (!?))
import           Graphics.GL                (GLfloat, GLuint)
import           Linear                     (V2 (..), V3 (..))

data AssemblyState a = AssemblyState
    { positions :: !(Vector (V3 GLfloat))
    , normals   :: !(Vector (V3 GLfloat))
    , texCoords :: !(Vector (V2 GLfloat))
    , allocator :: !IndexAllocator
    , vertices  :: ![a]
      -- ^ Must be reversed in the output state.
    , indices   :: ![GLuint]
      -- ^ Must be reversed in the output state.
    } deriving Show

type VerticeMaker a = AssemblyState a -> Point -> Maybe (AssemblyState a)

-- | Assemble the 'FilePart's to a list of vertices and indices.
assembleVertP :: [FilePart] -> Maybe ([Vert_P.Vertex], [GLuint])
assembleVertP = assembleVertices vertP

-- | Assemble the 'FilePart's to a list of vertices and indices.
assembleVertPN :: [FilePart] -> Maybe ([Vert_P_N.Vertex], [GLuint])
assembleVertPN = assembleVertices vertPN

-- | Assemble the 'FilePart's to a list of vertices and indices.
assembleVertPNTx :: [FilePart] -> Maybe ([Vert_P_N_Tx.Vertex], [GLuint])
assembleVertPNTx = assembleVertices vertPNTx

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

vertP :: AssemblyState Vert_P.Vertex -> Point
      -> Maybe (AssemblyState Vert_P.Vertex)
vertP state p@(Point posI _ _) =
    case Alloc.alloc p (allocator state) of
        Index i allocator' -> do
            let positions' = positions state
            pos <- positions' !? ind posI
            let vert = Vert_P.Vertex { Vert_P.position = pos }
            return $ state { vertices = vert:vertices state
                           , indices = i:indices state
                           , allocator = allocator'
                           }
        AlreadyIndexed i   ->
            return $ state { indices = i:indices state }

vertPN :: AssemblyState Vert_P_N.Vertex -> Point
       -> Maybe (AssemblyState Vert_P_N.Vertex)
vertPN state p@(Point posI _ (Just normI)) =
    case Alloc.alloc p (allocator state) of
        Index i allocator' -> do
            let positions' = positions state
                normals' = normals state
            pos <- positions' !? ind posI
            norm <- normals' !? ind normI
            let vert = Vert_P_N.Vertex { Vert_P_N.position = pos
                                       , Vert_P_N.normal = norm
                                       }
            return $ state { vertices = vert:vertices state
                           , indices = i:indices state
                           , allocator = allocator'
                           }

        AlreadyIndexed i ->
            return $ state { indices = i:indices state }

vertPN _ _                                 = Nothing

vertPNTx :: AssemblyState Vert_P_N_Tx.Vertex -> Point
         -> Maybe (AssemblyState Vert_P_N_Tx.Vertex)
vertPNTx state p@(Point posI (Just texI) (Just normI)) =
    case Alloc.alloc p (allocator state) of
        Index i allocator' -> do
            let positions' = positions state
                normals' = normals state
                texCoords' = texCoords state
            pos <- positions' !? ind posI
            norm <- normals' !? ind normI
            tex <- texCoords' !? ind texI
            let vert = Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = pos
                                          , Vert_P_N_Tx.normal = norm
                                          , Vert_P_N_Tx.texCoord = tex
                                          }
            return $ state { vertices = vert:vertices state
                           , indices = i:indices state
                           , allocator = allocator'
                           }

        AlreadyIndexed i ->
            return $ state { indices = i:indices state }

vertPNTx _ _                                 = Nothing

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
