{-# LANGUAGE BangPatterns #-}

module PriorityQueue.Internals.HeapPQ(
    HeapPQ
) where

import Control.Concurrent.STM
import PriorityQueue.PriorityQueue


data Heap k v = Nil
              | Node {-# UNPACK #-} !Int -- rank
                     {-# UNPACK #-} !Int -- size
                     !k                   -- prio
                     v                   -- item
                     !(Heap k v)         -- left
                     !(Heap k v)         -- right


data HeapPQ k v = PQ (TVar (Heap k v))


empty :: Heap k v
empty = Nil


leaf :: k -> v -> Heap k v
leaf !k v = Node 1 1 k v empty empty


ins :: Ord k => k -> v -> Heap k v -> Heap k v
ins !k v !h = h `union` leaf k v


union :: Ord k => Heap k v -> Heap k v -> Heap k v
h `union` Nil = h
Nil `union` h = h
h1@(Node _ _ !k1 v1 l1 r1) `union` h2@(Node _ _ !k2 v2 l2 r2) =
  if k1 < k2
  then mk k1 v1 l1 $ r1 `union` h2
  else mk k2 v2 l2 $ r2 `union` h1


mk :: k -> v -> Heap k v -> Heap k v -> Heap k v
mk !k v h1 h2 =
  if rk h1 > rk h2 then Node (rk h1 + 1) ss k v h1 h2
                   else Node (rk h2 + 1) ss k v h2 h1
  where
    rk Nil = 0
    rk (Node !r _ _ _ _ _) = r
    sz Nil = 0
    sz (Node _ !s _ _ _ _) = s
    !ss = sz h1 + sz h2 + 1


pk :: Heap t a -> Maybe a
pk Nil = Nothing
pk (Node _ _ _ v _ _) = Just v


rm :: Ord k => Heap k v -> Maybe (v, Heap k v)
rm Nil = Nothing
rm (Node _ _ _ v l r) = Just (v, l `union` r)


dm :: Ord k => HeapPQ k b -> STM b
dm (PQ hp) = do
  h <- readTVar hp
  case rm h of
    Nothing -> retry
    Just (v, h') -> do
      writeTVar hp h'
      return v


instance PriorityQueue HeapPQ where
    new       = PQ `fmap` newTVar Nil
    insert    (PQ hp) k v = modifyTVar hp $ ins k v
    peekMin   (PQ hp) = pk `fmap` readTVar hp >>= maybe retry return
    deleteMin = dm
