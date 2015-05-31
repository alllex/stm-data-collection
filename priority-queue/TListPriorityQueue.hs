{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module TListPriorityQueue(
    TListPriorityQueue
) where

import Control.Concurrent.STM
import PriorityQueue

data TNode k v = TNil
               | TCons (TVar (k, v, TNode k v))

data TListPriorityQueue k v = TLPQ (TVar (TNode k v))

tlpqNew :: STM (TListPriorityQueue k v)
tlpqNew = do
  hd <- newTVar TNil
  return $ TLPQ hd


tlpqInsert :: (Ord k) => TListPriorityQueue k v -> k -> v -> STM ()
tlpqInsert (TLPQ hd) k v =
    do
        xs <- readTVar hd
        xs' <- push xs
        writeTVar hd xs'
    where
        push TNil        = do
          nvar <- newTVar (k, v, TNil)
          return $ TCons nvar
        push (TCons var) = do
          (k', _, next) <- readTVar var
          if k' <= k
             then do
               nnext <- push next
               modifyTVar var $ \(kk, vv, _) -> (kk, vv, nnext)
               return $ TCons var
             else do
               nvar <- newTVar (k, v, TCons var)
               return $ TCons nvar


tlpqPeekMin :: TListPriorityQueue k v -> STM v
tlpqPeekMin (TLPQ hd) = do
    xs <- readTVar hd
    case xs of
      TNil  -> retry
      (TCons var) -> do
        (_, v, _) <- readTVar var
        return v


tlpqDeleteMin :: TListPriorityQueue k v -> STM v
tlpqDeleteMin (TLPQ hd) = do
    xs <- readTVar hd
    case xs of
      TNil  -> retry
      (TCons var) -> do
        (_, v, next) <- readTVar var
        writeTVar hd next
        return v


tlpqTryDeleteMin :: TListPriorityQueue k a -> STM (Maybe a)
tlpqTryDeleteMin lpq = (Just `fmap` tlpqDeleteMin lpq) `orElse` return Nothing


instance Ord k => PriorityQueue TListPriorityQueue k v where
  new            = tlpqNew
  insert         = tlpqInsert
  peekMin        = tlpqPeekMin
  deleteMin      = tlpqDeleteMin
  tryDeleteMin   = tlpqTryDeleteMin


