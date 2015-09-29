{-|
Module      : Data.STM.PriorityQueue.Class
Description : STM-based Concurrent Priority Queue data structure class
Copyright   : (c) Alex Semin, 2015
License     : BSD3
Maintainer  : alllex.semin@gmail.com
Stability   : experimental
Portability : portable

Concurrent Priority Queue implemented over Haskell STM.
This container allows to store items supplied with key and
provides efficient way to retrive item with the smallest key.

@
import Control.Concurrent.STM
import qualified Data.STM.PriorityQueue as PQ
import Control.Monad ( forM_ )

main :: IO ()
main = do
    pq <- atomically $ (PQ.new :: STM (PQ.Impl Int Int))
    let kvs = [(2, 1), (5, 3), (1, 2), (4, 5)]
    forM_ kvs $ \(k, v) -> atomically $ PQ.insert pq k v
    x <- atomically $ PQ.deleteMin pq
    putStrLn $ "x = " ++ show x -- prints 2
@
-}

module Data.STM.PriorityQueue.Class (
    PriorityQueue(..)
) where

import Control.Concurrent.STM

class PriorityQueue q where

    -- | /O(1)/. Creates empty priority queue.
    new          :: (Ord k) => STM (q k v)

    -- | /O(log(n))/. Performs insertion of an item with key.
    -- | Note: depending on the implementation time complexity may be better.
    insert       :: (Ord k) => q k v -> k -> v -> STM ()

    -- | /O(1)/. Returns an item with the smallest key without removing it.
    peekMin      :: (Ord k) => q k v -> STM v

    -- | /O(log(n))/. Returns an item with the smallest key and removes it.
    -- | Note: depending on the implementation time complexity may be better.
    deleteMin    :: (Ord k) => q k v -> STM v

    -- | /O(log(n))/. Tries to delete item and returns if transaction fails.
    -- | Note: depending on the implementation time complexity may be better.
    tryDeleteMin :: (Ord k) => q k v -> STM (Maybe v)
    tryDeleteMin pq = (Just `fmap` deleteMin pq) `orElse` return Nothing
