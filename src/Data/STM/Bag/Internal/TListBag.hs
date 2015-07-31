

module Data.STM.Bag.Internal.TListBag(
    TListBag
) where

import Control.Concurrent.STM
import Data.STM.Bag.Class

data TList a = Nil | TNode a (TVar (TList a))
data TListBag a = B
    { getHead :: TVar (TList a)
    , getTail :: TVar (TVar (TList a))
    }

bNew :: STM (TListBag a)
bNew = do
    h <- newTVar Nil
    t <- newTVar h
    return $ B h t

bAdd :: TListBag a -> a -> STM ()
bAdd (B _ t'') v = do
    nt' <- newTVar Nil
    t' <- readTVar t''
    writeTVar t' (TNode v nt')
    writeTVar t'' nt'

bTake :: TListBag a -> STM a
bTake (B h' t'') = do
    h <- readTVar h'
    case h of
        Nil -> retry
        TNode v i' -> do
            i <- readTVar i'
            case i of
                Nil -> writeTVar i' i >> writeTVar t'' h'
                _ -> writeTVar i' i
            return v

instance Bag TListBag where
    new  = bNew
    add  = bAdd
    take = bTake
