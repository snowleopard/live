-- Algebraic graphs: https://hackage.haskell.org/package/algebraic-graphs.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Graph where

import Data.Set (Set)

-- An algebraic data type for describing graphs.
data Graph a = Empty
             | Vertex a
             | Overlay (Graph a) (Graph a)
             | Connect (Graph a) (Graph a)
             deriving Show

-- Check if a graph is empty.
isEmpty :: Graph a -> Bool
isEmpty = undefined

-- The meaning of a graph expression as a pair sets (vertices and edges).
directedGraphSemantics :: Ord a => Graph a -> (Set a, Set (a, a))
directedGraphSemantics = undefined

-- Fold a graph by recursively applying the provided functions to the leaves
-- and internal nodes of the expression.
-- The order of arguments is: empty, vertex, overlay and connect.
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg = undefined

-- Graph equality.
instance Ord a => Eq (Graph a) where
  (==) = undefined

-- Convenient syntax sugar for constructing graphs with numeric vertices.
instance Num a => Num (Graph a) where
  fromInteger = undefined
  (+)         = undefined
  (*)         = undefined
  signum      = undefined
  abs         = undefined
  negate      = undefined

-- The sorted list of vertices of a given graph.
vertexList :: Ord a => Graph a -> [a]
vertexList = undefined

-- The sorted list of edges of a graph.
edgeList :: Ord a => Graph a -> [(a, a)]
edgeList = undefined

-- Check if the first graph is a subgraph of the second.
isSubgraphOf :: Ord a => Graph a -> Graph a -> Bool
isSubgraphOf = undefined

-- Construct the graph comprising a single edge.
edge :: a -> a -> Graph a
edge = undefined

-- Overlay a given list of graphs.
overlays :: [Graph a] -> Graph a
overlays = undefined

-- Connect a given list of graphs.
connects :: [Graph a] -> Graph a
connects = undefined

-- Construct the graph comprising a given list of isolated vertices.
vertices :: [a] -> Graph a
vertices = undefined

-- The clique on a list of vertices.
clique :: [a] -> Graph a
clique = undefined

-- Construct the graph from a list of edges.
edges :: [(a, a)] -> Graph a
edges = undefined

-- Check if a graph contains a given vertex.
hasVertex :: Eq a => a -> Graph a -> Bool
hasVertex = undefined

-- Check if a graph contains a given edge.
hasEdge :: Ord a => a -> a -> Graph a -> Bool
hasEdge = undefined

-- Transpose a graph, i.e. flip the direction of all edges.
transpose :: Graph a -> Graph a
transpose = undefined

-- Simplify a graph.
simplify :: Ord a => Graph a -> Graph a
simplify = undefined

-- The path on a list of vertices.
path :: [a] -> Graph a
path = undefined

-- The circuit on a list of vertices.
circuit :: [a] -> Graph a
circuit = undefined

-- The star formed by a centre vertex connected to a list of leaves.
star :: a -> [a] -> Graph a
star = undefined

-- The biclique on two lists of vertices.
biclique :: [a] -> [a] -> Graph a
biclique = undefined

instance Functor Graph where
  fmap = undefined

-- Replace the first vertex with the second vertex.
replaceVertex :: Eq a => a -> a -> Graph a -> Graph a
replaceVertex = undefined

-- Merge vertices satisfying a given predicate into a given vertex.
mergeVertices :: (a -> Bool) -> a -> Graph a -> Graph a
mergeVertices = undefined

instance Monad Graph where
  return = undefined
  (>>=)  = undefined

-- Construct the induced subgraph of a given graph.
induce :: (a -> Bool) -> Graph a -> Graph a
induce = undefined

-- Remove a vertex from a given graph.
removeVertex :: Eq a => a -> Graph a -> Graph a
removeVertex = undefined

-- Split a vertex into a list of vertices with the same connectivity.
splitVertex :: Eq a => a -> [a] -> Graph a -> Graph a
splitVertex = undefined

-- Remove an edge from a given graph.
removeEdge :: Eq a => a -> a -> Graph a -> Graph a
removeEdge = undefined

instance Applicative Graph where
  pure  = undefined
  (<*>) = undefined

-- Compute the Cartesian product of graphs.
box :: Graph a -> Graph b -> Graph (a, b)
box = undefined

-- Construct a mesh graph from two lists of vertices.
mesh :: [a] -> [b] -> Graph (a, b)
mesh = undefined

-- Construct a torus graph from two lists of vertices.
torus :: [a] -> [b] -> Graph (a, b)
torus = undefined
