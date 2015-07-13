
module Internal.THeapPriorityQueue(
    THeapPriorityQueue
) where

import Data.Functor((<$>))
import Control.Concurrent.STM
import PriorityQueue


data Heap k v = Nil
              | Node {-# UNPACK #-} !Int
                     {-# UNPACK #-} !Int
                     k
                     v
                     (TVar (Heap k v))
                     (TVar (Heap k v))


data THeapPriorityQueue k v = THPQ (TVar (Heap k v))


rank :: Heap k v -> Int
rank Nil = 0
rank (Node r _ _ _ _ _) = r


size :: Heap k v -> Int
size Nil = 0
size (Node _ s _ _ _ _) = s


union :: Ord k => Heap k v -> Heap k v -> STM (Heap k v)
h `union` Nil = return h
Nil `union` h = return h
union h1@(Node _ _ k1 v1 vl1 vr1)
      h2@(Node _ _ k2 v2 vl2 vr2) = do
  r1 <- readTVar vr1
  r2 <- readTVar vr2
  if k1 < k2
  then do
    nr <- r1 `union` h2
    vr' <- newTVar nr
    mk k1 v1 vl1 vr'
  else do
    nr <- r2 `union` h1
    vr' <- newTVar nr
    mk k2 v2 vl2 vr'


mk :: Ord k => k -> v -> TVar (Heap k v) -> TVar (Heap k v) -> STM (Heap k v)
mk k v vh1 vh2 = do
  h1 <- readTVar vh1
  h2 <- readTVar vh2
  let (r1, r2) = both ((+1).rank) (h1, h2)
  let ss = size h1 + size h2 + 1 -- uncurry ((.)(.)(.)(+1)(+)) $ both size (h1, h2)
  return $ if r1 > r2
    then Node r1 ss k v vh1 vh2
    else Node r2 ss k v vh2 vh1
  where
    both f (a, b) = (f a, f b)


thpqInsert :: Ord k => THeapPriorityQueue k v -> k -> v -> STM ()
thpqInsert (THPQ hp) k v = do
  h <- readTVar hp
  l <- newTVar Nil
  r <- newTVar Nil
  h' <- h `union` Node 1 1 k v l r
  writeTVar hp h'


thpqPeekMin :: Ord k => THeapPriorityQueue k v -> STM v
thpqPeekMin (THPQ hp) = do
  h <- readTVar hp
  case h of
    Nil                -> retry
    (Node _ _ _ v _ _) -> return v


thpqDeleteMin :: Ord k => THeapPriorityQueue k b -> STM b
thpqDeleteMin (THPQ hp) = do
  h <- readTVar hp
  case h of
    Nil                  -> retry
    (Node _ _ _ v vl vr) -> do
      l <- readTVar vl
      r <- readTVar vr
      h' <- l `union` r
      writeTVar hp h'
      return v


instance PriorityQueue THeapPriorityQueue where
    new            = THPQ <$> newTVar Nil
    insert         = thpqInsert
    peekMin        = thpqPeekMin
    deleteMin      = thpqDeleteMin
    tryDeleteMin   = \hhp -> (Just <$> deleteMin hhp) `orElse` return Nothing





