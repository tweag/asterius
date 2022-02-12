{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE RankNTypes, BangPatterns, FlexibleContexts, Strict #-}

{- |
  Module      :  Data.Graph.Dom
  Copyright   :  (c) Matt Morrow 2009
  License     :  BSD3

  The Lengauer-Tarjan graph dominators algorithm.

    \[1\] Lengauer, Tarjan,
      /A Fast Algorithm for Finding Dominators in a Flowgraph/, 1979.

    \[2\] Muchnick,
      /Advanced Compiler Design and Implementation/, 1997.

    \[3\] Brisk, Sarrafzadeh,
      /Interference Graphs for Procedures in Static Single/
      /Information Form are Interval Graphs/, 2007.

  Taken from the dom-lt package.

-}

module Dom (
   Node,Path,Edge
  ,Graph,Rooted
  ,idom,ipdom
  ,domTree,pdomTree
  ,dom,pdom
  ,pddfs,rpddfs
  ,fromAdj,fromEdges
  ,toAdj,toEdges
  ,asTree,asGraph
  ,parents,ancestors
) where

import Data.Monoid(Monoid(..))
import Data.Bifunctor
import Data.Tuple (swap)

import Data.Tree
import Data.List
import Data.IntMap(IntMap)
import Data.IntSet(IntSet)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Control.Monad
import Control.Monad.ST.Strict

import Data.Array.ST
import Data.Array.Base
  (unsafeNewArray_
  ,unsafeWrite,unsafeRead)

-----------------------------------------------------------------------------

type Node       = Int
type Path       = [Node]
type Edge       = (Node,Node)
type Graph      = IntMap IntSet
type Rooted     = (Node, Graph)

-----------------------------------------------------------------------------

-- | /Dominators/.
-- Complexity as for @idom@
dom :: Rooted -> [(Node, Path)]
dom = ancestors . domTree

-- | /Post-dominators/.
-- Complexity as for @idom@.
pdom :: Rooted -> [(Node, Path)]
pdom = ancestors . pdomTree

-- | /Dominator tree/.
-- Complexity as for @idom@.
domTree :: Rooted -> Tree Node
domTree a@(r,_) =
  let is = filter ((/=r).fst) (idom a)
      tg = fromEdges (fmap swap is)
  in asTree (r,tg)

-- | /Post-dominator tree/.
-- Complexity as for @idom@.
pdomTree :: Rooted -> Tree Node
pdomTree a@(r,_) =
  let is = filter ((/=r).fst) (ipdom a)
      tg = fromEdges (fmap swap is)
  in asTree (r,tg)

-- | /Immediate dominators/.
-- /O(|E|*alpha(|E|,|V|))/, where /alpha(m,n)/ is
-- \"a functional inverse of Ackermann's function\".
--
-- This Complexity bound assumes /O(1)/ indexing. Since we're
-- using @IntMap@, it has an additional /lg |V|/ factor
-- somewhere in there. I'm not sure where.
idom :: Rooted -> [(Node,Node)]
idom rg = runST (evalS idomM =<< initEnv (pruneReach rg))

-- | /Immediate post-dominators/.
-- Complexity as for @idom@.
ipdom :: Rooted -> [(Node,Node)]
ipdom rg = runST (evalS idomM =<< initEnv (pruneReach (second predG rg)))

-----------------------------------------------------------------------------

-- | /Post-dominated depth-first search/.
pddfs :: Rooted -> [Node]
pddfs = reverse . rpddfs

-- | /Reverse post-dominated depth-first search/.
rpddfs :: Rooted -> [Node]
rpddfs = concat . levels . pdomTree

-----------------------------------------------------------------------------

type Dom s a = S s (Env s) a
type NodeSet    = IntSet
type NodeMap a  = IntMap a
data Env s = Env
  {succE      :: !Graph
  ,predE      :: !Graph
  ,bucketE    :: !Graph
  ,dfsE       :: {-# UNPACK #-}!Int
  ,zeroE      :: {-# UNPACK #-}!Node
  ,rootE      :: {-# UNPACK #-}!Node
  ,labelE     :: {-# UNPACK #-}!(Arr s Node)
  ,parentE    :: {-# UNPACK #-}!(Arr s Node)
  ,ancestorE  :: {-# UNPACK #-}!(Arr s Node)
  ,childE     :: {-# UNPACK #-}!(Arr s Node)
  ,ndfsE      :: {-# UNPACK #-}!(Arr s Node)
  ,dfnE       :: {-# UNPACK #-}!(Arr s Int)
  ,sdnoE      :: {-# UNPACK #-}!(Arr s Int)
  ,sizeE      :: {-# UNPACK #-}!(Arr s Int)
  ,domE       :: {-# UNPACK #-}!(Arr s Node)
  ,rnE        :: {-# UNPACK #-}!(Arr s Node)}

-----------------------------------------------------------------------------

idomM :: Dom s [(Node,Node)]
idomM = do
  dfsDom =<< rootM
  n <- gets dfsE
  forM_ [n,n-1..1] (\i-> do
    w <- ndfsM i
    sw <- sdnoM w
    ps <- predsM w
    forM_ ps (\v-> do
      u <- eval v
      su <- sdnoM u
      when (su < sw)
        (store sdnoE w su))
    z <- ndfsM =<< sdnoM w
    modify(\e->e{bucketE=IM.adjust
                      (w`IS.insert`)
                      z (bucketE e)})
    pw <- parentM w
    link pw w
    bps <- bucketM pw
    forM_ bps (\v-> do
      u <- eval v
      su <- sdnoM u
      sv <- sdnoM v
      let dv = case su < sv of
                True-> u
                False-> pw
      store domE v dv))
  forM_ [1..n] (\i-> do
    w <- ndfsM i
    j <- sdnoM w
    z <- ndfsM j
    dw <- domM w
    when (dw /= z)
      (do ddw <- domM dw
          store domE w ddw))
  fromEnv

-----------------------------------------------------------------------------

eval :: Node -> Dom s Node
eval v = do
  n0 <- zeroM
  a  <- ancestorM v
  case a==n0 of
    True-> labelM v
    False-> do
      compress v
      a   <- ancestorM v
      l   <- labelM v
      la  <- labelM a
      sl  <- sdnoM l
      sla <- sdnoM la
      case sl <= sla of
        True-> return l
        False-> return la

compress :: Node -> Dom s ()
compress v = do
  n0  <- zeroM
  a   <- ancestorM v
  aa  <- ancestorM a
  when (aa /= n0) (do
    compress a
    a   <- ancestorM v
    aa  <- ancestorM a
    l   <- labelM v
    la  <- labelM a
    sl  <- sdnoM l
    sla <- sdnoM la
    when (sla < sl)
      (store labelE v la)
    store ancestorE v aa)

-----------------------------------------------------------------------------

link :: Node -> Node -> Dom s ()
link v w = do
  n0  <- zeroM
  lw  <- labelM w
  slw <- sdnoM lw
  let balance s = do
        c   <- childM s
        lc  <- labelM c
        slc <- sdnoM lc
        case slw < slc of
          False-> return s
          True-> do
            zs  <- sizeM s
            zc  <- sizeM c
            cc  <- childM c
            zcc <- sizeM cc
            case 2*zc <= zs+zcc of
              True-> do
                store ancestorE c s
                store childE s cc
                balance s
              False-> do
                store sizeE c zs
                store ancestorE s c
                balance c
  s   <- balance w
  lw  <- labelM w
  zw  <- sizeM w
  store labelE s lw
  store sizeE v . (+zw) =<< sizeM v
  let follow s = do
        when (s /= n0) (do
          store ancestorE s v
          follow =<< childM s)
  zv  <- sizeM v
  follow =<< case zv < 2*zw of
              False-> return s
              True-> do
                cv <- childM v
                store childE v s
                return cv

-----------------------------------------------------------------------------

dfsDom :: Node -> Dom s ()
dfsDom i = do
  _   <- go i
  n0  <- zeroM
  r   <- rootM
  store parentE r n0
  where go i = do
          n <- nextM
          store dfnE   i n
          store sdnoE  i n
          store ndfsE  n i
          store labelE i i
          ss <- succsM i
          forM_ ss (\j-> do
            s <- sdnoM j
            case s==0 of
              False-> return()
              True-> do
                store parentE j i
                go j)

-----------------------------------------------------------------------------

initEnv :: Rooted -> ST s (Env s)
initEnv (r0,g0) = do
  let (g,rnmap) = renum 1 g0
      pred      = predG g
      r         = rnmap IM.! r0
      n         = IM.size g
      ns        = [0..n]
      m         = n+1

  let bucket = IM.fromList
        (zip ns (repeat mempty))

  rna <- newI m
  writes rna (fmap swap
        (IM.toList rnmap))

  doms      <- newI m
  sdno      <- newI m
  size      <- newI m
  parent    <- newI m
  ancestor  <- newI m
  child     <- newI m
  label     <- newI m
  ndfs      <- newI m
  dfn       <- newI m

  forM_ [0..n] (doms.=0)
  forM_ [0..n] (sdno.=0)
  forM_ [1..n] (size.=1)
  forM_ [0..n] (ancestor.=0)
  forM_ [0..n] (child.=0)

  (doms.=r) r
  (size.=0) 0
  (label.=0) 0

  return (Env
    {rnE        = rna
    ,dfsE       = 0
    ,zeroE      = 0
    ,rootE      = r
    ,labelE     = label
    ,parentE    = parent
    ,ancestorE  = ancestor
    ,childE     = child
    ,ndfsE      = ndfs
    ,dfnE       = dfn
    ,sdnoE      = sdno
    ,sizeE      = size
    ,succE      = g
    ,predE      = pred
    ,bucketE    = bucket
    ,domE       = doms})

fromEnv :: Dom s [(Node,Node)]
fromEnv = do
  dom   <- gets domE
  rn    <- gets rnE
  -- r     <- gets rootE
  (_,n) <- st (getBounds dom)
  forM [1..n] (\i-> do
    j <- st(rn!:i)
    d <- st(dom!:i)
    k <- st(rn!:d)
    return (j,k))

-----------------------------------------------------------------------------

zeroM :: Dom s Node
zeroM = gets zeroE
domM :: Node -> Dom s Node
domM = fetch domE
rootM :: Dom s Node
rootM = gets rootE
succsM :: Node -> Dom s [Node]
succsM i = gets (IS.toList . (! i) . succE)
predsM :: Node -> Dom s [Node]
predsM i = gets (IS.toList . (! i) . predE)
bucketM :: Node -> Dom s [Node]
bucketM i = gets (IS.toList . (! i) . bucketE)
sizeM :: Node -> Dom s Int
sizeM = fetch sizeE
sdnoM :: Node -> Dom s Int
sdnoM = fetch sdnoE
-- dfnM :: Node -> Dom s Int
-- dfnM = fetch dfnE
ndfsM :: Int -> Dom s Node
ndfsM = fetch ndfsE
childM :: Node -> Dom s Node
childM = fetch childE
ancestorM :: Node -> Dom s Node
ancestorM = fetch ancestorE
parentM :: Node -> Dom s Node
parentM = fetch parentE
labelM :: Node -> Dom s Node
labelM = fetch labelE
nextM :: Dom s Int
nextM = do
  n <- gets dfsE
  let n' = n+1
  modify(\e->e{dfsE=n'})
  return n'

-----------------------------------------------------------------------------

type A = STUArray
type Arr s a = A s Int a

infixl 9 !:
infixr 2 .=

(.=) :: (MArray (A s) a (ST s))
     => Arr s a -> a -> Int -> ST s ()
(v .= x) i = unsafeWrite v i x

(!:) :: (MArray (A s) a (ST s))
     => A s Int a -> Int -> ST s a
a !: i = do
  o <- unsafeRead a i
  return $! o

new :: (MArray (A s) a (ST s))
    => Int -> ST s (Arr s a)
new n = unsafeNewArray_ (0,n-1)

newI :: Int -> ST s (Arr s Int)
newI = new

-- newD :: Int -> ST s (Arr s Double)
-- newD = new

-- dump :: (MArray (A s) a (ST s)) => Arr s a -> ST s [a]
-- dump a = do
--   (m,n) <- getBounds a
--   forM [m..n] (\i -> a!:i)

writes :: (MArray (A s) a (ST s))
     => Arr s a -> [(Int,a)] -> ST s ()
writes a xs = forM_ xs (\(i,x) -> (a.=x) i)

-- arr :: (MArray (A s) a (ST s)) => [a] -> ST s (Arr s a)
-- arr xs = do
--   let n = length xs
--   a <- new n
--   go a n 0 xs
--   return a
--   where go _ _ _    [] = return ()
--         go a n i (x:xs)
--           | i <= n = (a.=x) i >> go a n (i+1) xs
--           | otherwise = return ()

-----------------------------------------------------------------------------

(!) :: Monoid a => IntMap a -> Int -> a
(!) g n = maybe mempty id (IM.lookup n g)

fromAdj :: [(Node, [Node])] -> Graph
fromAdj = IM.fromList . fmap (second IS.fromList)

fromEdges :: [Edge] -> Graph
fromEdges = collectI IS.union fst (IS.singleton . snd)

toAdj :: Graph -> [(Node, [Node])]
toAdj = fmap (second IS.toList) . IM.toList

toEdges :: Graph -> [Edge]
toEdges = concatMap (uncurry (fmap . (,))) . toAdj

predG :: Graph -> Graph
predG g = IM.unionWith IS.union (go g) g0
  where g0 = fmap (const mempty) g
        f :: IntMap IntSet -> Int -> IntSet -> IntMap IntSet
        f m i a = foldl' (\m p -> IM.insertWith mappend p
                                      (IS.singleton i) m)
                        m
                       (IS.toList a)
        go :: IntMap IntSet -> IntMap IntSet
        go = flip IM.foldlWithKey' mempty f

pruneReach :: Rooted -> Rooted
pruneReach (r,g) = (r,g2)
  where is = reachable
              (maybe mempty id
                . flip IM.lookup g) $ r
        g2 = IM.fromList
            . fmap (second (IS.filter (`IS.member`is)))
            . filter ((`IS.member`is) . fst)
            . IM.toList $ g

tip :: Tree a -> (a, [Tree a])
tip (Node a ts) = (a, ts)

parents :: Tree a -> [(a, a)]
parents (Node i xs) = p i xs
        ++ concatMap parents xs
  where p i = fmap (flip (,) i . rootLabel)

ancestors :: Tree a -> [(a, [a])]
ancestors = go []
  where go acc (Node i xs)
          = let acc' = i:acc
            in p acc' xs ++ concatMap (go acc') xs
        p is = fmap (flip (,) is . rootLabel)

asGraph :: Tree Node -> Rooted
asGraph t@(Node a _) = let g = go t in (a, fromAdj g)
  where go (Node a ts) = let as = (fst . unzip . fmap tip) ts
                          in (a, as) : concatMap go ts

asTree :: Rooted -> Tree Node
asTree (r,g) = let go a = Node a (fmap go ((IS.toList . f) a))
                   f = (g !)
            in go r

reachable :: (Node -> NodeSet) -> (Node -> NodeSet)
reachable f a = go (IS.singleton a) a
  where go seen a = let s = f a
                        as = IS.toList (s `IS.difference` seen)
                    in foldl' go (s `IS.union` seen) as

collectI :: (c -> c -> c)
        -> (a -> Int) -> (a -> c) -> [a] -> IntMap c
collectI (<>) f g
  = foldl' (\m a -> IM.insertWith (<>)
                                  (f a)
                                  (g a) m) mempty

-- collect :: (Ord b) => (c -> c -> c)
--         -> (a -> b) -> (a -> c) -> [a] -> Map b c
-- collect (<>) f g
--   = foldl' (\m a -> SM.insertWith (<>)
--                                   (f a)
--                                   (g a) m) mempty

-- (renamed, old -> new)
renum :: Int -> Graph -> (Graph, NodeMap Node)
renum from = (\(_,m,g)->(g,m))
  . IM.foldlWithKey'
      f (from,mempty,mempty)
  where
    f :: (Int, NodeMap Node, IntMap IntSet) -> Node -> IntSet
      -> (Int, NodeMap Node, IntMap IntSet)
    f (!n,!env,!new) i ss =
            let (j,n2,env2) = go n env i
                (n3,env3,ss2) = IS.fold
                  (\k (!n,!env,!new)->
                      case go n env k of
                        (l,n2,env2)-> (n2,env2,l `IS.insert` new))
                  (n2,env2,mempty) ss
                new2 = IM.insertWith IS.union j ss2 new
            in (n3,env3,new2)
    go :: Int
        -> NodeMap Node
        -> Node
        -> (Node,Int,NodeMap Node)
    go !n !env i =
        case IM.lookup i env of
        Just j -> (j,n,env)
        Nothing -> (n,n+1,IM.insert i n env)

-----------------------------------------------------------------------------

newtype S z s a = S {unS :: forall o. (a -> s -> ST z o) -> s -> ST z o}
instance Functor (S z s) where
  fmap f (S g) = S (\k -> g (k . f))
instance Monad (S z s) where
  return = pure
  S g >>= f = S (\k -> g (\a -> unS (f a) k))
instance Applicative (S z s) where
  pure a = S (\k -> k a)
  (<*>) = ap
-- get :: S z s s
-- get = S (\k s -> k s s)
gets :: (s -> a) -> S z s a
gets f = S (\k s -> k (f s) s)
-- set :: s -> S z s ()
-- set s = S (\k _ -> k () s)
modify :: (s -> s) -> S z s ()
modify f = S (\k -> k () . f)
-- runS :: S z s a -> s -> ST z (a, s)
-- runS (S g) = g (\a s -> return (a,s))
evalS :: S z s a -> s -> ST z a
evalS (S g) = g ((return .) . const)
-- execS :: S z s a -> s -> ST z s
-- execS (S g) = g ((return .) . flip const)
st :: ST z a -> S z s a
st m = S (\k s-> do
  a <- m
  k a s)
store :: (MArray (A z) a (ST z))
      => (s -> Arr z a) -> Int -> a -> S z s ()
store f i x = do
  a <- gets f
  st ((a.=x) i)
fetch :: (MArray (A z) a (ST z))
      => (s -> Arr z a) -> Int -> S z s a
fetch f i = do
  a <- gets f
  st (a!:i)

-----------------------------------------------------------------------------

-- g0 = fromAdj
--   [(1,[2,3])
--   ,(2,[3])
--   ,(3,[4])
--   ,(4,[3,5,6])
--   ,(5,[7])
--   ,(6,[7])
--   ,(7,[4,8])
--   ,(8,[3,9,10])
--   ,(9,[1])
--   ,(10,[7])]

-- g1 = fromAdj
--   [(0,[1])
--   ,(1,[2,3])
--   ,(2,[7])
--   ,(3,[4])
--   ,(4,[5,6])
--   ,(5,[7])
--   ,(6,[4])
--   ,(7,[])]

-----------------------------------------------------------------------------
