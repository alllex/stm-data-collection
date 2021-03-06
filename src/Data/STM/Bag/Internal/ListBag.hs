{-|
Module      : Data.STM.Bag.Internal.ListBag
Description : STM-based Concurrent Bag data structure implementation
Copyright   : (c) Alex Semin, 2015
License     : BSD3
Maintainer  : alllex.semin@gmail.com
Stability   : experimental
Portability : portable

Implementation of the 'Data.STM.Bag.Class' using coarse-grained list.
It is efficient only if there are not many threads.
-}

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
