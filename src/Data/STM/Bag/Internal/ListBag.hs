
module Data.STM.Bag.Internal.ListBag(
    ListBag
) where

import Control.Concurrent.STM
import Data.STM.Bag.Class

data ListBag v = B (TVar [v]) -- stack behavior

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

bIsEmpty :: ListBag v -> STM Bool
bIsEmpty (B b') = do
    b <- readTVar b'
    case b of
        [] -> return True
        _ -> return False

instance Bag ListBag where
    new  = bNew
    add  = bAdd
    take = bTake
    isEmpty = bIsEmpty
