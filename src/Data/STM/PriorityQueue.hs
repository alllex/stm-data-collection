
module Data.STM.PriorityQueue (
    Impl,
    module Data.STM.PriorityQueue.Class
) where

import Data.STM.PriorityQueue.Class
import Data.STM.PriorityQueue.Internal.PTSTASLPQ

type Impl = PTSTASLPQ
