{-|
Module      : Data.STM.Bag.Internal.TListBag
Description : STM-based Concurrent Bag data structure implementation
Copyright   : (c) Alex Semin, 2015
License     : BSD3
Maintainer  : alllex.semin@gmail.com
Stability   : experimental
Portability : portable

Implementation of the 'Data.STM.Bag.Class' using fine-grained list.
It is efficient only if there are not many threads.
-}

module Data.STM.Bag.Internal.TListBag(
    TListBag
) where

import Control.Concurrent.STM
import Data.STM.Bag.Class

-- | Fine-grained list upon 'Control.Concurrent.STM.TVar's
data TList v = Nil | TNode v (TVar (TList v))

data TListBag v = B
    { _getHead :: TVar (TList v)
    , _getTail :: TVar (TVar (TList v))
    }

bNew :: STM (TListBag v)
bNew = do
    h <- newTVar Nil
    t <- newTVar h
    return $ B h t

bAdd :: TListBag v -> v -> STM ()
bAdd (B _ t'') v = do
    nt' <- newTVar Nil
    t' <- readTVar t''
    writeTVar t' (TNode v nt')
    writeTVar t'' nt'

bTake :: TListBag v -> STM v
bTake (B h' t'') = do
    h <- readTVar h'
    case h of
        Nil -> retry
        TNode v i' -> do
            i <- readTVar i'
            case i of
                Nil -> writeTVar h' i >> writeTVar t'' h'
                _ ->   writeTVar h' i
            return v

bIsEmpty :: TListBag v -> STM Bool
bIsEmpty (B h' _) = do
    h <- readTVar h'
    case h of
        Nil -> return True
        _ -> return False

instance Bag TListBag where
    new  = bNew
    add  = bAdd
    take = bTake
    isEmpty = bIsEmpty
