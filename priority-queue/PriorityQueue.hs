
module PriorityQueue (PriorityQueue(..)) where

import Control.Concurrent.STM

class PriorityQueue q where
    new          :: (Ord k) => STM (q k v)
    insert       :: (Ord k) => q k v -> k -> v -> STM ()
    peekMin      :: (Ord k) => q k v -> STM v
    deleteMin    :: (Ord k) => q k v -> STM v
    tryDeleteMin :: (Ord k) => q k v -> STM (Maybe v)

