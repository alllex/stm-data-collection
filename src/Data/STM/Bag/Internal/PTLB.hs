
module Data.STM.Bag.Internal.PTLB(
    PTLB
) where

import Control.Concurrent.STM

import Data.STM.Bag.Class
import qualified Data.STM.Bag.Internal.RoundRobinBag as RRB
import Data.STM.Bag.Internal.ListBag

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
