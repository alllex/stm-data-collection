{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module ListPriorityQueue(
    ListPriorityQueue
) where

import Control.Concurrent.STM
import PriorityQueue

data ListPriorityQueue k v = LPQ (TVar [(k, v)])


lpqNew :: STM (ListPriorityQueue k v)
lpqNew = do l <- newTVar []; return $ LPQ l


lpqInsert :: (Ord k) => ListPriorityQueue k v -> k -> v -> STM ()
lpqInsert (LPQ xsv) k v =
    do
        xs <- readTVar xsv
        writeTVar xsv $ push xs
    where
        push [] = [(k, v)]
        push (p@(k', _):xs) | k >= k' = p : push xs
                            | otherwise = (k, v):p:xs


lpqPeekMin :: ListPriorityQueue k v -> STM v
lpqPeekMin (LPQ xsv) = do
    xs <- readTVar xsv
    case xs of
      [] -> retry
      ((_, v):_) -> return v


lpqDeleteMin :: ListPriorityQueue k v -> STM v
lpqDeleteMin (LPQ xsv) = do
    xs <- readTVar xsv
    case xs of
      [] -> retry
      ((_, v):xs') -> do
        writeTVar xsv xs'
        return v


lpqTryDeleteMin :: ListPriorityQueue k v -> STM (Maybe v)
lpqTryDeleteMin lpq = (Just `fmap` lpqDeleteMin lpq) `orElse` return Nothing


instance Ord k => PriorityQueue ListPriorityQueue k v where
    new            = lpqNew
    insert         = lpqInsert
    peekMin        = lpqPeekMin
    deleteMin      = lpqDeleteMin
    tryDeleteMin   = lpqTryDeleteMin



