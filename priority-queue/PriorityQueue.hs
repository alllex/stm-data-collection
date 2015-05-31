{-# LANGUAGE MultiParamTypeClasses  #-}

module PriorityQueue (PriorityQueue(..)) where

import Control.Concurrent.STM

class Ord k => PriorityQueue q k v where
    new          :: STM (q k v)
    insert       :: q k v -> k -> v -> STM ()
    peekMin      :: q k v -> STM v
    deleteMin    :: q k v -> STM v
    tryDeleteMin :: q k v -> STM (Maybe v)

