{-|
Module      : Data.STM.Bag.Class
Description : STM-based Concurrent Bag data structure class
Copyright   : (c) Alex Semin, 2015
License     : BSD3
Maintainer  : alllex.semin@gmail.com
Stability   : experimental
Portability : portable

Concurrent Bag data structure which is relying on STM
for consistency in multi-threading environment. Bag (or Multi Set)
is a data container which provides very efficient adding and removing
but guarantees __no order__.

@
import Control.Concurrent.STM
import qualified Data.STM.Bag as Bag

main :: IO ()
main = do
    bag <- atomically $ (Bag.new :: STM (Bag.Impl Int))
    atomically $ Bag.add bag 7
    atomically $ Bag.add bag 5
    x <- atomically $ Bag.take bag
    putStrLn $ show x -- x may be either 5 or 7
@

-}

module Data.STM.Bag.Class (
    Bag(..)
) where

import Control.Concurrent.STM

class Bag b where
    -- | /O(1)/. Creates an empty bag.
    new  :: STM (b v)
    -- | /O(1)/. Adds given value to the bag.
    add  :: b v -> v -> STM ()
    -- | /O(1)/. Returns any item from bag removing it from data structure.
    take :: b v -> STM v
    -- | /O(1)/. Checks whether the bag is empty.
    isEmpty :: b v -> STM Bool
