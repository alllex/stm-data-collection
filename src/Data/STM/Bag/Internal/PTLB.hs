{-|
Module      : Data.STM.Bag.Internal.PTLB
Description : STM-based Concurrent Bag data structure implementation
Copyright   : (c) Alex Semin, 2015
License     : BSD3
Maintainer  : alllex.semin@gmail.com
Stability   : experimental
Portability : portable

Implementation of the 'Data.STM.Bag.Class' using 'Control.Concurrent.STM.TArray' of
'Data.STM.Bag.Internal.ListBag' (coarse-grained lists) which are used in thread-local manner in the first place and
perform work-stealing otherwise.
-}

module Data.STM.Bag.Internal.PTLB(
    PTLB
) where

import Control.Concurrent.STM

import Data.STM.Bag.Class
import qualified Data.STM.Bag.Internal.RoundRobinBag as RRB
import Data.STM.Bag.Internal.ListBag

-- | Abbreviation stands for Per Thread List Bag
data PTLB v = B (RRB.RoundRobinBag v)

bNew :: STM (PTLB v)
bNew = B `fmap` RRB.build (new :: STM (ListBag v))

bAdd :: PTLB v -> v -> STM ()
bAdd (B rrb) = RRB.add rrb

bTake :: PTLB v -> STM v
bTake (B rrb) = RRB.take rrb

bIsEmpty :: PTLB v -> STM Bool
bIsEmpty (B rrb) = RRB.isEmpty rrb

instance Bag PTLB where
    new  = bNew
    add  = bAdd
    take = bTake
    isEmpty = bIsEmpty
