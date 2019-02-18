{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
# if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE Safe #-}
# else
{-# LANGUAGE Trustworthy #-}
# endif
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = Finite Graphs
--
-- The @'Graph'@ type is an adjacency list representation of a finite, directed
-- graph with vertices of type @Int@.
--
-- The @'SCC'@ type represents a
-- <https://en.wikipedia.org/wiki/Strongly_connected_component strongly-connected component>
-- of a graph.
--
-- == Implementation
--
-- The implementation is based on
--
--   * /Structuring Depth-First Search Algorithms in Haskell/,
--     by David King and John Launchbury, <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.52.6526>
--
-----------------------------------------------------------------------------

module Data.Graph (

    -- * Graphs
      Graph
    , Bounds
    , Edge
    , Vertex
    , Table

    -- ** Graph Construction
    , graphFromEdges
    , graphFromEdges'
    , buildG

    -- ** Graph Properties
    , vertices
    , edges
    , outdegree
    , indegree

    -- ** Graph Transformations
    , transposeG

    -- ** Graph Algorithms
    , dfs
    , dff
    , topSort
    , components
    , scc
    , bcc
    , reachable
    , path


    -- * Strongly Connected Components
    , SCC(..)

    -- ** Construction
    , stronglyConnComp
    , stronglyConnCompR

    -- ** Conversion
    , flattenSCC
    , flattenSCCs

    -- * Trees
    , module Data.Tree

    ) where

#if USE_ST_MONAD
import Control.Monad.ST
import Data.Array.ST.Safe (newArray, readArray, writeArray)
# if USE_UNBOXED_ARRAYS
import Data.Array.ST.Safe (STUArray)
# else
import Data.Array.ST.Safe (STArray)
# endif
#else
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
#endif
import Data.Tree (Tree(Node), Forest)

-- std interfaces
import Control.Applicative
#if !MIN_VERSION_base(4,8,0)
import qualified Data.Foldable as F
import Data.Traversable
#else
import Data.Foldable as F
#endif
import Control.DeepSeq (NFData(rnf))
import Data.Maybe
import Data.Array
#if USE_UNBOXED_ARRAYS
import qualified Data.Array.Unboxed as UA
import Data.Array.Unboxed ( UArray )
#else
import qualified Data.Array as UA
#endif
import Data.List
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes
#endif
#if (!MIN_VERSION_base(4,11,0)) && MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup (..))
#endif
#ifdef __GLASGOW_HASKELL__
import GHC.Generics (Generic, Generic1)
import Data.Data (Data)
import Data.Typeable
#endif

-- Make sure we don't use Integer by mistake.
default ()

-------------------------------------------------------------------------
--                                                                      -
--      Strongly Connected Components
--                                                                      -
-------------------------------------------------------------------------

-- | Strongly connected component.
data SCC vertex = AcyclicSCC vertex     -- ^ A single vertex that is not
                                        -- in any cycle.
                | CyclicSCC  [vertex]   -- ^ A maximal set of mutually
                                        -- reachable vertices.
#if __GLASGOW_HASKELL__ >= 802
  deriving ( Eq   -- ^ @since 0.5.9
           , Show -- ^ @since 0.5.9
           , Read -- ^ @since 0.5.9
           )
#else
  deriving (Eq, Show, Read)
#endif

INSTANCE_TYPEABLE1(SCC)

#ifdef __GLASGOW_HASKELL__
-- | @since 0.5.9
deriving instance Data vertex => Data (SCC vertex)

-- | @since 0.5.9
deriving instance Generic1 SCC

-- | @since 0.5.9
deriving instance Generic (SCC vertex)
#endif

#if MIN_VERSION_base(4,9,0)
-- | @since 0.5.9
instance Eq1 SCC where
  liftEq eq (AcyclicSCC v1) (AcyclicSCC v2) = eq v1 v2
  liftEq eq (CyclicSCC vs1) (CyclicSCC vs2) = liftEq eq vs1 vs2
  liftEq _ _ _ = False
-- | @since 0.5.9
instance Show1 SCC where
  liftShowsPrec sp _sl d (AcyclicSCC v) = showsUnaryWith sp "AcyclicSCC" d v
  liftShowsPrec _sp sl d (CyclicSCC vs) = showsUnaryWith (const sl) "CyclicSCC" d vs
-- | @since 0.5.9
instance Read1 SCC where
  liftReadsPrec rp rl = readsData $
    readsUnaryWith rp "AcyclicSCC" AcyclicSCC <>
    readsUnaryWith (const rl) "CyclicSCC" CyclicSCC
#endif

-- | @since 0.5.9
instance F.Foldable SCC where
  foldr c n (AcyclicSCC v) = c v n
  foldr c n (CyclicSCC vs) = foldr c n vs

-- | @since 0.5.9
instance Traversable SCC where
  -- We treat the non-empty cyclic case specially to cut one
  -- fmap application.
  traverse f (AcyclicSCC vertex) = AcyclicSCC <$> f vertex
  traverse _f (CyclicSCC []) = pure (CyclicSCC [])
  traverse f (CyclicSCC (x : xs)) =
    liftA2 (\x' xs' -> CyclicSCC (x' : xs')) (f x) (traverse f xs)

instance NFData a => NFData (SCC a) where
    rnf (AcyclicSCC v) = rnf v
    rnf (CyclicSCC vs) = rnf vs

-- | @since 0.5.4
instance Functor SCC where
    fmap f (AcyclicSCC v) = AcyclicSCC (f v)
    fmap f (CyclicSCC vs) = CyclicSCC (fmap f vs)

-- | The vertices of a list of strongly connected components.
flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flattenSCC

-- | The vertices of a strongly connected component.
flattenSCC :: SCC vertex -> [vertex]
flattenSCC (AcyclicSCC v) = [v]
flattenSCC (CyclicSCC vs) = vs

-- | The strongly connected components of a directed graph, reverse topologically
-- sorted.
--
-- ==== __Examples__
--
-- > stronglyConnComp [("a",0,[1]),("b",1,[2,3]),("c",2,[1]),("d",3,[3])]
-- >   == [CyclicSCC ["d"],CyclicSCC ["b","c"],AcyclicSCC "a"]
stronglyConnComp
        :: Ord key
        => [(node, key, [key])]
                -- ^ The graph: a list of nodes uniquely identified by keys,
                -- with a list of keys of nodes this node has edges to.
                -- The out-list may contain keys that don't correspond to
                -- nodes of the graph; such edges are ignored.
        -> [SCC node]

stronglyConnComp edges0
  = map get_node (stronglyConnCompR edges0)
  where
    get_node (AcyclicSCC (n, _, _)) = AcyclicSCC n
    get_node (CyclicSCC triples)     = CyclicSCC [n | (n,_,_) <- triples]

-- | The strongly connected components of a directed graph, reverse topologically
-- sorted.  The function is the same as 'stronglyConnComp', except that
-- all the information about each node retained.
-- This interface is used when you expect to apply 'SCC' to
-- (some of) the result of 'SCC', so you don't want to lose the
-- dependency information.
--
-- ==== __Examples__
--
-- > stronglyConnCompR [("a",0,[1]),("b",1,[2,3]),("c",2,[1]),("d",3,[3])]
-- >  == [CyclicSCC [("d",3,[3])],CyclicSCC [("b",1,[2,3]),("c",2,[1])],AcyclicSCC ("a",0,[1])]
stronglyConnCompR
        :: Ord key
        => [(node, key, [key])]
                -- ^ The graph: a list of nodes uniquely identified by keys,
                -- with a list of keys of nodes this node has edges to.
                -- The out-list may contain keys that don't correspond to
                -- nodes of the graph; such edges are ignored.
        -> [SCC (node, key, [key])]     -- ^ Reverse topologically sorted

stronglyConnCompR [] = []  -- added to avoid creating empty array in graphFromEdges -- SOF
stronglyConnCompR edges0
  = map decode forest
  where
    (graph, vertex_fn,_) = graphFromEdges edges0
    forest             = scc graph
    decode (Node v []) | mentions_itself v = CyclicSCC [vertex_fn v]
                       | otherwise         = AcyclicSCC (vertex_fn v)
    decode other = CyclicSCC (dec other [])
                 where
                   dec (Node v ts) vs = vertex_fn v : foldr dec vs ts
    mentions_itself v = v `elem` (graph ! v)

-------------------------------------------------------------------------
--                                                                      -
--      Graphs
--                                                                      -
-------------------------------------------------------------------------

-- | Abstract representation of vertices.
type Vertex  = Int
-- | Table indexed by a contiguous set of vertices.
--
-- /Note: This is included for backwards compatibility./
type Table a = Array Vertex a
-- | Adjacency list representation of a graph, mapping each vertex to its
-- list of successors.
type Graph   = Array Vertex [Vertex]
-- | The bounds of an @Array@.
type Bounds  = (Vertex, Vertex)
-- | An edge from the first vertex to the second.
type Edge    = (Vertex, Vertex)

#if !USE_UNBOXED_ARRAYS
type UArray i a = Array i a
#endif

-- | Returns the list of vertices in the graph.
--
-- ==== __Examples__
--
-- > vertices (buildG (0,-1) []) == []
--
-- > vertices (buildG (0,2) [(0,1),(1,2)]) == [0,1,2]
vertices :: Graph -> [Vertex]
vertices  = indices

-- | Returns the list of edges in the graph.
--
-- ==== __Examples__
--
-- > edges (buildG (0,-1) []) == []
--
-- > edges (buildG (0,2) [(0,1),(1,2)]) == [(0,1),(1,2)]
edges    :: Graph -> [Edge]
edges g   = [ (v, w) | v <- vertices g, w <- g!v ]

-- | Build a graph from a list of edges.
--
-- Warning: This function will cause a runtime exception if a vertex in the edge
-- list is not within the given @Bounds@.
--
-- ==== __Examples__
--
-- > buildG (0,-1) [] == array (0,-1) []
-- > buildG (0,2) [(0,1), (1,2)] == array (0,1) [(0,[1]),(1,[2])]
-- > buildG (0,2) [(0,1), (0,2), (1,2)] == array (0,2) [(0,[2,1]),(1,[2]),(2,[])]
buildG :: Bounds -> [Edge] -> Graph
buildG bounds0 edges0 = accumArray (flip (:)) [] bounds0 edges0

-- | The graph obtained by reversing all edges.
--
-- ==== __Examples__
--
-- > transposeG (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,[]),(1,[0]),(2,[1])]
transposeG  :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE    :: Graph -> [Edge]
reverseE g   = [ (w, v) | (v, w) <- edges g ]

-- | A table of the count of edges from each node.
--
-- ==== __Examples__
--
-- > outdegree (buildG (0,-1) []) == array (0,-1) []
--
-- > outdegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,1),(1,1),(2,0)]
outdegree :: Graph -> Array Vertex Int
-- This is bizarrely lazy. We build an array filled with thunks, instead
-- of actually calculating anything. This is the historical behavior, and I
-- suppose someone *could* be relying on it, but it might be worth finding
-- out. Note that we *can't* be so lazy with indegree.
outdegree  = fmap length

-- | A table of the count of edges into each node.
--
-- ==== __Examples__
--
-- > indegree (buildG (0,-1) []) == array (0,-1) []
--
-- > indegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,0),(1,1),(2,1)]
indegree :: Graph -> Array Vertex Int
indegree g = accumArray (+) 0 (bounds g) [(v, 1) | (_, outs) <- assocs g, v <- outs]

-- | Identical to 'graphFromEdges', except that the return value
-- does not include the function which maps keys to vertices.  This
-- version of 'graphFromEdges' is for backwards compatibility.
graphFromEdges'
        :: Ord key
        => [(node, key, [key])]
        -> (Graph, Vertex -> (node, key, [key]))
graphFromEdges' x = (a,b) where
    (a,b,_) = graphFromEdges x

-- | Build a graph from a list of nodes uniquely identified by keys,
-- with a list of keys of nodes this node should have edges to.
--
-- This function takes an adjacency list representing a graph with vertices of
-- type @key@ labeled by values of type @node@ and produces a @Graph@-based
-- representation of that list. The @Graph@ result represents the /shape/ of the
-- graph, and the functions describe a) how to retrieve the label and adjacent
-- vertices of a given vertex, and b) how to retrive a vertex given a key.
--
-- @(graph, nodeFromVertex, vertexFromKey) = graphFromEdges edgeList@
--
-- * @graph :: Graph@ is the raw, array based adjacency list for the graph.
-- * @nodeFromVertex :: Vertex -> (node, key, [key])@ returns the node
--   associated with the given 0-based @Int@ vertex; see /warning/ below.
-- * @vertexFromKey :: key -> Maybe Vertex@ returns the @Int@ vertex for the
--   key if it exists in the graph, @Nothing@ otherwise.
--
-- To safely use this API you must either extract the list of vertices directly
-- from the graph or first call @vertexFromKey k@ to check if a vertex
-- corresponds to the key @k@. Once it is known that a vertex exists you can use
-- @nodeFromVertex@ to access the labelled node and adjacent vertices. See below
-- for examples.
--
-- Note: The out-list may contain keys that don't correspond to nodes of the
-- graph; they are ignored.
--
-- Warning: The @nodeFromVertex@ function will cause a runtime exception if the
-- given @Vertex@ does not exist.
--
-- ==== __Examples__
--
-- An empty graph.
--
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges []
-- > graph = array (0,-1) []
--
-- A graph where the out-list references unspecified nodes (@'c'@), these are
-- ignored.
--
-- > (graph, _, _) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c'])]
-- > array (0,1) [(0,[1]),(1,[])]
--
--
-- A graph with 3 vertices: ("a") -> ("b") -> ("c")
--
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]
-- > graph == array (0,2) [(0,[1]),(1,[2]),(2,[])]
-- > nodeFromVertex 0 == ("a",'a',"b")
-- > vertexFromKey 'a' == Just 0
--
-- Get the label for a given key.
--
-- > let getNodePart (n, _, _) = n
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]
-- > getNodePart . nodeFromVertex <$> vertexFromKey 'a' == Just "A"
--
graphFromEdges
        :: Ord key
        => [(node, key, [key])]
        -> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)
graphFromEdges edges0
  = (graph, \v -> vertex_map ! v, key_vertex)
  where
    max_v           = length edges0 - 1
    bounds0         = (0,max_v) :: (Vertex, Vertex)
    sorted_edges    = sortBy lt edges0
    edges1          = zipWith (,) [0..] sorted_edges

    graph           = array bounds0 [(,) v (mapMaybe key_vertex ks) | (,) v (_,    _, ks) <- edges1]
    key_map         = array bounds0 [(,) v k                       | (,) v (_,    k, _ ) <- edges1]
    vertex_map      = array bounds0 edges1

    (_,k1,_) `lt` (_,k2,_) = k1 `compare` k2

    -- key_vertex :: key -> Maybe Vertex
    --  returns Nothing for non-interesting vertices
    key_vertex k   = findVertex 0 max_v
                   where
                     findVertex a b | a > b
                              = Nothing
                     findVertex a b = case compare k (key_map ! mid) of
                                   LT -> findVertex a (mid-1)
                                   EQ -> Just mid
                                   GT -> findVertex (mid+1) b
                              where
                                mid = a + (b - a) `div` 2

-------------------------------------------------------------------------
--                                                                      -
--      Depth first search
--                                                                      -
-------------------------------------------------------------------------

-- | A spanning forest of the graph, obtained from a depth-first search of
-- the graph starting from each vertex in an unspecified order.
dff          :: Graph -> Forest Vertex
dff g         = dfs g (vertices g)

-- | A spanning forest of the part of the graph reachable from the listed
-- vertices, obtained from a depth-first search of the graph starting at
-- each of the listed vertices in order.
dfs          :: Graph -> [Vertex] -> Forest Vertex
dfs g vs      = prune (bounds g) (map (generate g) vs)

generate     :: Graph -> Vertex -> Tree Vertex
generate g v  = Node v (map (generate g) (g!v))

prune        :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = run bnds (chop ts)

chop         :: Forest Vertex -> SetM s (Forest Vertex)
chop []       = return []
chop (Node v ts : us)
              = do
                visited <- contains v
                if visited then
                  chop us
                 else do
                  include v
                  as <- chop ts
                  bs <- chop us
                  return (Node v as : bs)

-- A monad holding a set of vertices visited so far.
#if USE_ST_MONAD

-- Use the ST monad if available, for constant-time primitives.

#if USE_UNBOXED_ARRAYS
newtype SetM s a = SetM { runSetM :: STUArray s Vertex Bool -> ST s a }
#else
newtype SetM s a = SetM { runSetM :: STArray  s Vertex Bool -> ST s a }
#endif

instance Monad (SetM s) where
    return = pure
    {-# INLINE return #-}
    SetM v >>= f = SetM $ \s -> do { x <- v s; runSetM (f x) s }
    {-# INLINE (>>=) #-}

instance Functor (SetM s) where
    f `fmap` SetM v = SetM $ \s -> f `fmap` v s
    {-# INLINE fmap #-}

instance Applicative (SetM s) where
    pure x = SetM $ const (return x)
    {-# INLINE pure #-}
    SetM f <*> SetM v = SetM $ \s -> f s >>= (`fmap` v s)
    -- We could also use the following definition
    --   SetM f <*> SetM v = SetM $ \s -> f s <*> v s
    -- but Applicative (ST s) instance is present only in GHC 7.2+
    {-# INLINE (<*>) #-}

run          :: Bounds -> (forall s. SetM s a) -> a
run bnds act  = runST (newArray bnds False >>= runSetM act)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> readArray m v

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> writeArray m v True

#else /* !USE_ST_MONAD */

-- Portable implementation using IntSet.

newtype SetM s a = SetM { runSetM :: IntSet -> (a, IntSet) }

instance Monad (SetM s) where
    return x     = SetM $ \s -> (x, s)
    SetM v >>= f = SetM $ \s -> case v s of (x, s') -> runSetM (f x) s'

instance Functor (SetM s) where
    f `fmap` SetM v = SetM $ \s -> case v s of (x, s') -> (f x, s')
    {-# INLINE fmap #-}

instance Applicative (SetM s) where
    pure x = SetM $ \s -> (x, s)
    {-# INLINE pure #-}
    SetM f <*> SetM v = SetM $ \s -> case f s of (k, s') -> case v s' of (x, s'') -> (k x, s'')
    {-# INLINE (<*>) #-}

run          :: Bounds -> SetM s a -> a
run _ act     = fst (runSetM act Set.empty)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> (Set.member v m, m)

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> ((), Set.insert v m)

#endif /* !USE_ST_MONAD */

-------------------------------------------------------------------------
--                                                                      -
--      Algorithms
--                                                                      -
-------------------------------------------------------------------------

------------------------------------------------------------
-- Algorithm 1: depth first search numbering
------------------------------------------------------------

preorder' :: Tree a -> [a] -> [a]
preorder' (Node a ts) = (a :) . preorderF' ts

preorderF' :: Forest a -> [a] -> [a]
preorderF' ts = foldr (.) id $ map preorder' ts

preorderF :: Forest a -> [a]
preorderF ts = preorderF' ts []

tabulate        :: Bounds -> [Vertex] -> UArray Vertex Int
tabulate bnds vs = UA.array bnds (zipWith (flip (,)) [1..] vs)
-- Why zipWith (flip (,)) instead of just using zip with the
-- arguments in the other order? We want the [1..] to fuse
-- away, and these days that only happens when it's the first
-- list argument.

preArr          :: Bounds -> Forest Vertex -> UArray Vertex Int
preArr bnds      = tabulate bnds . preorderF

------------------------------------------------------------
-- Algorithm 2: topological sorting
------------------------------------------------------------

postorder :: Tree a -> [a] -> [a]
postorder (Node a ts) = postorderF ts . (a :)

postorderF   :: Forest a -> [a] -> [a]
postorderF ts = foldr (.) id $ map postorder ts

postOrd :: Graph -> [Vertex]
postOrd g = postorderF (dff g) []

-- | A topological sort of the graph.
-- The order is partially specified by the condition that a vertex /i/
-- precedes /j/ whenever /j/ is reachable from /i/ but not vice versa.
topSort      :: Graph -> [Vertex]
topSort       = reverse . postOrd

------------------------------------------------------------
-- Algorithm 3: connected components
------------------------------------------------------------

-- | The connected components of a graph.
-- Two vertices are connected if there is a path between them, traversing
-- edges in either direction.
components   :: Graph -> Forest Vertex
components    = dff . undirected

undirected   :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)

-- Algorithm 4: strongly connected components

-- | The strongly connected components of a graph, in reverse topological order.
--
-- ==== __Examples__
--
-- > scc (buildG (0,3) [(3,1),(1,2),(2,0),(0,1)])
-- >   == [Node {rootLabel = 0, subForest = [Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []}]}]}
-- >      ,Node {rootLabel = 3, subForest = []}]

scc  :: Graph -> Forest Vertex
scc g = dfs g (reverse (postOrd (transposeG g)))

------------------------------------------------------------
-- Algorithm 5: Classifying edges
------------------------------------------------------------

{-
XXX unused code

tree              :: Bounds -> Forest Vertex -> Graph
tree bnds ts       = buildG bnds (concat (map flat ts))
 where flat (Node v ts') = [ (v, w) | Node w _us <- ts' ]
                        ++ concat (map flat ts')

back              :: Graph -> Table Int -> Graph
back g post        = mapT select g
 where select v ws = [ w | w <- ws, post!v < post!w ]

cross             :: Graph -> Table Int -> Table Int -> Graph
cross g pre post   = mapT select g
 where select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward           :: Graph -> Graph -> Table Int -> Graph
forward g tree' pre = mapT select g
 where select v ws = [ w | w <- ws, pre!v < pre!w ] \\ tree' ! v

mapT    :: (Vertex -> a -> b) -> Array Vertex a -> Array Vertex b
mapT f t = array (bounds t) [ (,) v (f v (t!v)) | v <- indices t ]
-}

------------------------------------------------------------
-- Algorithm 6: Finding reachable vertices
------------------------------------------------------------

-- | Returns the list of vertices reachable from a given vertex.
--
-- ==== __Examples__
--
-- > reachable (buildG (0,0) []) 0 == [0]
--
-- > reachable (buildG (0,2) [(0,1), (1,2)]) 0 == [0,1,2]
reachable :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dfs g [v])

-- | Returns @True@ if the second vertex reachable from the first.
--
-- ==== __Examples__
--
-- > path (buildG (0,0) []) 0 0 == True
--
-- > path (buildG (0,2) [(0,1), (1,2)]) 0 2 == True
--
-- > path (buildG (0,2) [(0,1), (1,2)]) 2 0 == False
path :: Graph -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g v)

------------------------------------------------------------
-- Algorithm 7: Biconnected components
------------------------------------------------------------

-- | The biconnected components of a graph.
-- An undirected graph is biconnected if the deletion of any vertex
-- leaves it connected.
bcc :: Graph -> Forest [Vertex]
bcc g = (concat . map bicomps . map (do_label g dnum)) forest
 where forest = dff g
       dnum   = preArr (bounds g) forest

do_label :: Graph -> UArray Vertex Int -> Tree Vertex -> Tree (Vertex,Int,Int)
do_label g dnum (Node v ts) = Node (v, dnum UA.! v, lv) us
 where us = map (do_label g dnum) ts
       lv = minimum ([dnum UA.! v] ++ [dnum UA.! w | w <- g!v]
                     ++ [lu | Node (_,_,lu) _ <- us])

bicomps :: Tree (Vertex,Int,Int) -> Forest [Vertex]
bicomps (Node (v,_,_) ts)
      = [ Node (v:vs) us | (_,Node vs us) <- map collect ts]

collect :: Tree (Vertex,Int,Int) -> (Int, Tree [Vertex])
collect (Node (v,dv,lv) ts) = (lv, Node (v:vs) cs)
 where collected = map collect ts
       vs = concat [ ws | (lw, Node ws _) <- collected, lw<dv]
       cs = concat [ if lw<dv then us else [Node (v:ws) us]
                        | (lw, Node ws us) <- collected ]
