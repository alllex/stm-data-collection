{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module PriorityQueue (PriorityQueue(..)) where

import Control.Concurrent.STM

class Ord k => PriorityQueue q k v where
    new     :: STM (q k v)
    write   :: q k v -> k -> v -> STM ()
    read    :: q k v -> STM v
    tryRead :: q k v -> STM (Maybe v)
    peek    :: q k v -> STM v



