
module Data.STM.Bag.Internal.ListBag(
    ListBag
) where

import Control.Concurrent.STM
import Data.STM.Bag.Class

data ListBag v = B (TVar [v])

bNew :: STM (ListBag v)
bNew = B `fmap` newTVar []

bAdd :: ListBag v -> v -> STM ()
bAdd (B b') v = readTVar b' >>= return . (v:) >>= writeTVar b'

bTake :: ListBag v -> STM v
bTake (B b') = do
    b <- readTVar b'
    case b of
        [] -> retry
        (v:vs) -> writeTVar b' vs >> return v

instance Bag ListBag where
    new  = bNew
    add  = bAdd
    take = bTake
