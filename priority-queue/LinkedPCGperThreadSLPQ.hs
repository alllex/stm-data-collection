{-# LANGUAGE FlexibleContexts #-}

module LinkedPCGperThreadSLPQ(
  LinkedPCGperThreadSLPQ
) where

import Control.Monad.STM
import Control.Monad
import Control.Concurrent.STM
import System.IO.Unsafe
import System.Random.PCG.Fast (createSystemRandom, uniform, GenIO)
import Data.Array.MArray
import Control.Concurrent

import PriorityQueue

data Node k v
  = Nil
  | Node
  { getKey   :: k
  , getVal   :: TVar v
  , getUp    :: TNode k v
  , getNext  :: TNode k v
  , getDown  :: TNode k v
  }

type TNode k v = TVar (Node k v)

{-
            [ v ]     [ v ]     [ v ]     [ v ]
  -------------------------------------------------------
  [top] --------------------------------> [ k ] --> [nil]
  [   ] ------------> [ k ] ------------> [ k ] --> [nil]
  [   ] ------------> [ k ] --- [ k ] --> [ k ] --> [nil]
  [bot] --> [ k ] --> [ k ] --> [ k ] --> [ k ] --> [nil]
  [nil]     [nil]     [nil]     [nil]

  The first layout is the main layout that is never deleted
  and used for data access and control.
-}
data LinkedPCGperThreadSLPQ k v
  = PQ
  { getTop       :: TNode k v  -- top-node of the main layout
  , getBottom    :: TNode k v  -- bottom-node of the main layout
  , getHeight    :: TVar Int   -- height of the main layout
  , getNil       :: TNode k v  -- pointer to Nil shared by all nodes
  , getGenIO     :: TArray Int GenIO -- RNG
  }


buildHeads
  :: Ord k
  => TNode k v -- ancestor in vertical linked list
  -> Int       -- remained height
  -> STM (TNode k v, TNode k v) -- built node and bottom node
buildHeads up' 0 = do
  nil' <- newTVar Nil
  return (nil', up')
buildHeads up' h = do
  curr' <- newTVar Nil
  next' <- newTVar Nil
  (down', bottom') <- buildHeads curr' (h-1)
  -- Algorithm never accesses the first layout of keys and values
  writeTVar curr' $ Node undefined undefined up' next' down'
  return (curr', bottom')

pqNew' :: Ord k => Int -> STM (LinkedPCGperThreadSLPQ k v)
pqNew' height = do
  nil' <- newTVar Nil
  (top', bottom') <- buildHeads nil' height
  height' <- newTVar height
  let cn = unsafePerformIO getNumCapabilities
  gios' <- newArray (1, cn) $ unsafePerformIO createSystemRandom
  return $ PQ top' bottom' height' nil' gios'


pqNew :: Ord k => STM (LinkedPCGperThreadSLPQ k v)
pqNew = pqNew' 16

logHalf :: Float
logHalf = log 0.5

chooseLvl :: GenIO -> Int -> Int
chooseLvl g h =
  min h $ 1 + truncate (log x / logHalf)
    where x = unsafePerformIO (uniform g :: IO Float)

pqInsert :: Ord k => LinkedPCGperThreadSLPQ k v -> k -> v -> STM ()
pqInsert (PQ top' _ height' nil' gios') k v = do
  top <- readTVar top'
  case top of
    Nil -> error "Illegal state: top must not be Nil"
    _ -> do
      prevs <- buildPrevs top' []
      height <- readTVar height'
      v' <- newTVar v
      let getCapNum = do
            tid <- myThreadId
            fst `fmap` threadCapability tid
          cn = 1 + unsafePerformIO getCapNum
      gio <- readArray gios' cn
      let lvl = chooseLvl gio height
      insertNode v' nil' prevs lvl
  where
    buildPrevs curr' prevs = do
      curr <- readTVar curr'
      case curr of
        Nil -> return prevs
        (Node _ _ _ next' down') -> do
          next <- readTVar next'
          let goDown = buildPrevs down' $ curr':prevs -- put only Node (not Nil) in prevs
              goNext = buildPrevs next' prevs
          case next of
            Nil -> goDown
            (Node kk _ _ _ _) -> if kk > k then goDown else goNext
    insertNode _ _ _ 0 = return ()
    insertNode v' down' (prev':prevs) h = do
      curr' <- newTVar Nil
      up' <- newTVar Nil
      (Node _ _ _ next' _) <- readTVar prev' -- no Nils were put in prevs
      next <- readTVar next'
      nextnext' <- newTVar next
      let curr = Node k v' up' nextnext' down'
      down <- readTVar down'
      case down of
        Nil -> return ()
        (Node _ _ downUp' _ _) -> writeTVar downUp' curr
      writeTVar next' curr
      writeTVar curr' curr
      insertNode v' curr' prevs (h-1)
    insertNode _ _ [] _ = error "Illegal state: main layout must be not lower than new column"

pqPeekMin :: Ord k => LinkedPCGperThreadSLPQ k v -> STM v
pqPeekMin (PQ _ bottom' _ _ _) = do
  bottom <- readTVar bottom'
  case bottom of
    Nil -> error "Illegal state: bottom must not be Nil"
    (Node _ _ _ next' _) -> do
      next <- readTVar next'
      case next of
        Nil -> retry
        (Node _ v' _ _ _) -> readTVar v'


pqDeleteMin :: Ord k => LinkedPCGperThreadSLPQ k v -> STM v
pqDeleteMin (PQ _ bottom' _ _ _) = do
  bottom <- readTVar bottom'
  case bottom of
    Nil -> error "Illegal state: bottom must not be Nil"
    (Node _ _ _ next' _) -> do
      next <- readTVar next'
      case next of
        Nil -> retry
        (Node _ v' _ _ _) -> do
          recDel bottom next
          readTVar v'
      where
        recDel _ Nil = return ()
        recDel (Node _ _ headUp' next' _)
               (Node _ _ up' nextnext' _) = do
                 nextnext <- readTVar nextnext'
                 up <- readTVar up'
                 headUp <- readTVar headUp'
                 writeTVar next' nextnext
                 recDel headUp up


pqTryDeleteMin:: Ord k => LinkedPCGperThreadSLPQ k v -> STM (Maybe v)
pqTryDeleteMin pq = (Just `fmap` pqDeleteMin pq) `orElse` return Nothing

instance PriorityQueue LinkedPCGperThreadSLPQ where
    new            = pqNew
    insert         = pqInsert
    peekMin        = pqPeekMin
    deleteMin      = pqDeleteMin
    tryDeleteMin   = pqTryDeleteMin






