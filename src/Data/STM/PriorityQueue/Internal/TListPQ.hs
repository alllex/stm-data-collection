
module Data.STM.PriorityQueue.Internal.TListPQ(
    TListPQ
) where

import Control.Concurrent.STM
import Data.STM.PriorityQueue.Class
import Control.Applicative

data TNode k v = TNil
               | TCons k v (TVar (TNode k v))

data TListPQ k v = PQ (TVar (TNode k v))


pqNew :: STM (TListPQ k v)
pqNew = PQ <$> newTVar TNil


pqInsert :: (Ord k) => TListPQ k v -> k -> v -> STM ()
pqInsert (PQ hd) k v =
    do
        xs <- readTVar hd
        xs' <- push xs
        writeTVar hd xs'
    where
        push TNil                 = TCons k v <$> newTVar TNil
        push cur@(TCons k' _ nxt) = do
          if k' > k then TCons k v <$> newTVar cur
          else do
            next <- readTVar nxt
            nnext <- push next
            writeTVar nxt $ nnext
            return cur


pqPeekMin :: TListPQ k v -> STM v
pqPeekMin (PQ hd) = do
    xs <- readTVar hd
    case xs of
      TNil  -> retry
      (TCons _ v _) -> return v


pqDeleteMin :: TListPQ k v -> STM v
pqDeleteMin (PQ hd) = do
    xs <- readTVar hd
    case xs of
      TNil            -> retry
      (TCons _ v nxt) -> do
        next <- readTVar nxt
        writeTVar hd next
        return v


instance PriorityQueue TListPQ where
  new            = pqNew
  insert         = pqInsert
  peekMin        = pqPeekMin
  deleteMin      = pqDeleteMin
