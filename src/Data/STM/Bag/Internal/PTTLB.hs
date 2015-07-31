
module Data.STM.Bag.Internal.PTTLB(
    PTTLB
) where

import Control.Concurrent.STM

import Data.STM.Bag.Class
import qualified Data.STM.Bag.Internal.RoundRobinBag as RRB
import Data.STM.Bag.Internal.TListBag

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
