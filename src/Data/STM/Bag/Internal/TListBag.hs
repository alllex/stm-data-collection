

module Data.STM.Bag.Internal.TListBag(
    TListBag
) where

import Control.Concurrent.STM
import Data.STM.Bag.Class

data TList v = Nil | TNode v (TVar (TList v))
data TListBag v = B
    { getHead :: TVar (TList v)
    , getTail :: TVar (TVar (TList v))
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
