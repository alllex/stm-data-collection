{-|
Module      : Data.STM.PriorityQueue.Internal.ListPQ
Description : STM-based Concurrent Priority Queue data structure class implementation
Copyright   : (c) Alex Semin, 2015
License     : BSD3
Maintainer  : alllex.semin@gmail.com
Stability   : experimental
Portability : portable

An implementation of 'Data.STM.PriorityQueue.Class' based on
__coarse-grained__ list of key-value pairs.
__WARNING__: this implementation has specific time complexity of operations.
Insertion is slower: /O(n)/. Deletion is faster: /O(1)/.
-}

module Data.STM.PriorityQueue.Internal.ListPQ(
    ListPQ
) where

import Control.Concurrent.STM
import Data.STM.PriorityQueue.Class

data ListPQ k v = PQ (TVar [(k, v)])

pqNew :: STM (ListPQ k v)
pqNew = PQ `fmap` newTVar []

pqInsert :: (Ord k) => ListPQ k v -> k -> v -> STM ()
pqInsert (PQ xsv) k v =
    do
        xs <- readTVar xsv
        writeTVar xsv $ push xs
    where
        push [] = [(k, v)]
        push (p@(k', _):xs) | k >= k' = p : push xs
                            | otherwise = (k, v):p:xs

pqPeekMin :: ListPQ k v -> STM v
pqPeekMin (PQ xsv) = do
    xs <- readTVar xsv
    case xs of
      [] -> retry
      ((_, v):_) -> return v

pqDeleteMin :: ListPQ k v -> STM v
pqDeleteMin (PQ xsv) = do
    xs <- readTVar xsv
    case xs of
      [] -> retry
      ((_, v):xs') -> do
        writeTVar xsv xs'
        return v

instance PriorityQueue ListPQ where
    new            = pqNew
    insert         = pqInsert
    peekMin        = pqPeekMin
    deleteMin      = pqDeleteMin
