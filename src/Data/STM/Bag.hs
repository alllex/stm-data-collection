
module Data.STM.Bag (
    Impl,
    module Data.STM.Bag.Class
) where

import Data.STM.Bag.Class
import Data.STM.Bag.Internal.PTLB

type Impl = PTLB
