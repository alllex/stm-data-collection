{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module ListPriorityQueue(
    ListPriorityQueue(LPQ)
) where

import Control.Concurrent.STM
import PriorityQueue

data ListPriorityQueue k v = LPQ (TVar [(k, v)])

values :: ListPriorityQueue k v -> STM [v]
values (LPQ xsv) = do
    xs <- readTVar xsv
    return $ map snd xs

newListPQ :: STM (ListPriorityQueue k v)
newListPQ = do l <- newTVar []; return $ LPQ l

writeListPQ :: (Ord k) => ListPriorityQueue k v -> k -> v -> STM ()
writeListPQ (LPQ xsv) k v = 
    do 
        xs <- readTVar xsv
        writeTVar xsv $ push xs 
    where
        push [] = [(k, v)]
        push (p@(k', v'):xs) | k' <  k = p : push xs
                             | k' >= k = (k, v):p:xs

lst :: [(k, v)] -> [(k, v)] -> STM (v, [(k, v)])
lst _ [] = retry
lst xs' [(_, v)] = return (v, reverse xs')
lst xs' (p:xs) = lst (p:xs') xs

readListPQ :: ListPriorityQueue k v -> STM v
readListPQ (LPQ xsv) = do
    xs <- readTVar xsv
    (v, xs') <- lst [] xs
    writeTVar xsv xs'
    return v

tryReadListPQ :: ListPriorityQueue k v -> STM (Maybe v)
tryReadListPQ lpq = (Just `fmap` readListPQ lpq) `orElse` return Nothing

peekListPQ :: ListPriorityQueue k v -> STM v
peekListPQ (LPQ xsv) = do
    xs <- readTVar xsv
    (v, _) <- lst [] xs
    return v

instance Ord k => PriorityQueue ListPriorityQueue k v where
    new     = newListPQ
    write   = writeListPQ
    read    = readListPQ
    tryRead = tryReadListPQ
    peek    = peekListPQ
