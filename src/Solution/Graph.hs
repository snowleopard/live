-- Algebraic graphs: https://hackage.haskell.org/package/algebraic-graphs.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Solution.Graph where

import Data.Set (Set)
import qualified Data.Set as Set

-- An algebraic data type for describing graphs.
data Graph a = Empty
             | Vertex a
             | Overlay (Graph a) (Graph a)
             | Connect (Graph a) (Graph a)
             deriving Show

-- Check if a graph is empty.
isEmpty :: Graph a -> Bool
isEmpty = foldg True (const False) (&&) (&&)

-- The meaning of a graph expression as a pair sets (vertices and edges).
directedGraphSemantics :: Ord a => Graph a -> (Set a, Set (a, a))
directedGraphSemantics = foldg e v o c
 where
  e                 = (Set.empty      , Set.empty)
  v a               = (Set.singleton a, Set.empty)
  o (v1,e1) (v2,e2) = (Set.union v1 v2, Set.union e1 e2)
  c (v1,e1) (v2,e2) = (Set.union v1 v2, Set.unions [e1, e2, Set.cartesianProduct v1 v2])

-- Fold a graph by recursively applying the provided functions to the leaves
-- and internal nodes of the expression.
-- The order of arguments is: empty, vertex, overlay and connect.
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg e v o c = go
  where
   go Empty         = e
   go (Vertex a   ) = v a
   go (Overlay x y) = o (go x) (go y)
   go (Connect x y) = c (go x) (go y)

-- Graph equality.
instance Ord a => Eq (Graph a) where
  x == y = directedGraphSemantics x == directedGraphSemantics y

-- Convenient syntax sugar for constructing graphs with numeric vertices.
instance Num a => Num (Graph a) where
  fromInteger = Vertex . fromInteger
  (+)         = Overlay
  (*)         = Connect
  signum      = const Empty
  abs         = id
  negate      = id

-- The sorted list of vertices of a given graph.
vertexList :: Ord a => Graph a -> [a]
vertexList = Set.toAscList . fst . directedGraphSemantics

-- The sorted list of edges of a graph.
edgeList :: Ord a => Graph a -> [(a, a)]
edgeList = Set.toAscList . snd . directedGraphSemantics

-- Check if the first graph is a subgraph of the second.
isSubgraphOf :: Ord a => Graph a -> Graph a -> Bool
isSubgraphOf x y = Overlay x y == y

-- Construct the graph comprising a single edge.
edge :: a -> a -> Graph a
edge a b = Connect (Vertex a) (Vertex b)

-- Overlay a given list of graphs.
overlays :: [Graph a] -> Graph a
overlays = foldr Overlay Empty

-- Connect a given list of graphs.
connects :: [Graph a] -> Graph a
connects = foldr Connect Empty

-- Construct the graph comprising a given list of isolated vertices.
vertices :: [a] -> Graph a
vertices = overlays . map Vertex

-- The clique on a list of vertices.
clique :: [a] -> Graph a
clique = connects . map Vertex

-- Construct the graph from a list of edges.
edges :: [(a, a)] -> Graph a
edges = overlays . map (uncurry edge)

-- Check if a graph contains a given vertex.
hasVertex :: Eq a => a -> Graph a -> Bool
hasVertex a = foldg False (==a) (||) (||)

-- Check if a graph contains a given edge.
hasEdge :: Ord a => a -> a -> Graph a -> Bool
hasEdge a b = isSubgraphOf (edge a b)

-- Transpose a graph, i.e. flip the direction of all edges.
transpose :: Graph a -> Graph a
transpose = foldg Empty Vertex Overlay (flip Connect)

-- Simplify a graph.
simplify :: Ord a => Graph a -> Graph a
simplify = foldg Empty Vertex (simple Overlay) (simple Connect)
 where
  simple f x y | x == z    = x
               | y == z    = y
               | otherwise = z
   where
    z = f x y

-- The path on a list of vertices.
path :: [a] -> Graph a
path as = case as of []     -> Empty
                     [a]    -> Vertex a
                     (_:bs) -> edges (zip as bs)

-- The circuit on a list of vertices.
circuit :: [a] -> Graph a
circuit []     = Empty
circuit (a:as) = path $ [a] ++ as ++ [a]

-- The star formed by a centre vertex connected to a list of leaves.
star :: a -> [a] -> Graph a
star = undefined

-- The biclique on two lists of vertices.
biclique :: [a] -> [a] -> Graph a
biclique as bs = Connect (vertices as) (vertices bs)

instance Functor Graph where
  fmap f = foldg Empty (Vertex . f) Overlay Connect

-- Replace the first vertex with the second vertex.
replaceVertex :: Eq a => a -> a -> Graph a -> Graph a
replaceVertex a b = fmap $ \c -> if c == a then b else c

-- Merge vertices satisfying a given predicate into a given vertex.
mergeVertices :: (a -> Bool) -> a -> Graph a -> Graph a
mergeVertices p a = fmap $ \b -> if p b then a else b

instance Applicative Graph where
  pure    = Vertex
  f <*> x = foldg Empty (<$> x) Overlay Connect f

instance Monad Graph where
  return  = Vertex
  x >>= f = foldg Empty f Overlay Connect x

-- Construct the induced subgraph of a given graph.
induce :: (a -> Bool) -> Graph a -> Graph a
induce p x = x >>= \a -> if p a then Vertex a else Empty

-- Remove a vertex from a given graph.
removeVertex :: Eq a => a -> Graph a -> Graph a
removeVertex a = induce (/=a)

-- Split a vertex into a list of vertices with the same connectivity.
splitVertex :: Eq a => a -> [a] -> Graph a -> Graph a
splitVertex a bs x = x >>= \b -> if b == a then vertices bs else Vertex b

-- Remove an edge from a given graph.
removeEdge :: Eq a => a -> a -> Graph a -> Graph a
removeEdge a b = foldg Empty Vertex Overlay c
 where
  c x y = Connect (removeEdge a b x) (removeVertex b y) `Overlay`
          Connect (removeVertex a x) (removeEdge a b y)

-- Compute the Cartesian product of graphs.
box :: Graph a -> Graph b -> Graph (a, b)
box x y = Overlay (fx <*> y) (fy <*> x)
 where
  fx = foldg Empty (Vertex .      (,)) Overlay Overlay x
  fy = foldg Empty (Vertex . flip (,)) Overlay Overlay y

-- Construct a mesh graph from two lists of vertices.
mesh :: [a] -> [b] -> Graph (a, b)
mesh as bs = box (path as) (path bs)

-- Construct a torus graph from two lists of vertices.
torus :: [a] -> [b] -> Graph (a, b)
torus as bs = box (circuit as) (circuit bs)
