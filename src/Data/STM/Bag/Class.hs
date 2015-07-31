
module Data.STM.Bag.Class (
    Bag(..)
) where

import Control.Concurrent.STM

class Bag b where
    new  :: STM (b v)
    add  :: b v -> v -> STM ()
    take :: b v -> STM v
    isEmpty :: b v -> STM Bool
