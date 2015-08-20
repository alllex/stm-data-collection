{-|
Module      : Data.STM.Bag.Internal.PTTLB
Description : STM-based Concurrent Bag data structure implementation
Copyright   : (c) Alex Semin, 2015
License     : BSD3
Maintainer  : alllex.semin@gmail.com
Stability   : experimental
Portability : portable

Implementation of the 'Data.STM.Bag.Class' using 'Control.Concurrent.STM.TArray' of
'Data.STM.Bag.Internal.TListBag' (find-grained lists)
which are used in thread-local manner in the first place
and perform work-stealing otherwise.
-}

module Data.STM.Bag.Internal.PTTLB(
    PTTLB
) where

import Control.Concurrent.STM

import Data.STM.Bag.Class
import qualified Data.STM.Bag.Internal.RoundRobinBag as RRB
import Data.STM.Bag.Internal.TListBag

-- | Abbreviation stands for Per Thread TList Bag
-- where TList means fine-grained list.
data PTTLB v = B (RRB.RoundRobinBag v)

bNew :: STM (PTTLB v)
bNew = B `fmap` RRB.build (new :: STM (TListBag v))

bAdd :: PTTLB v -> v -> STM ()
bAdd (B rrb) = RRB.add rrb

bTake :: PTTLB v -> STM v
bTake (B rrb) = RRB.take rrb

bIsEmpty :: PTTLB v -> STM Bool
bIsEmpty (B rrb) = RRB.isEmpty rrb

instance Bag PTTLB where
    new  = bNew
    add  = bAdd
    take = bTake
    isEmpty = bIsEmpty
