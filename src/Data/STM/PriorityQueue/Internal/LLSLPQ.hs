{-|
Module      : Data.STM.PriorityQueue.Internal.LLSLPQ
Description : STM-based Concurrent Priority Queue data structure class implementation
Copyright   : (c) Alex Semin, 2015
License     : BSD3
Maintainer  : alllex.semin@gmail.com
Stability   : experimental
Portability : portable

An implementation of 'Data.STM.PriorityQueue.Class' based on skip-list.

Expected time complexity of deletion is /O(1)/, while insertion still
normally has logarithmic complexity.

__Default maximum height of skip-list node is 16.__ Use explicit constructor in case
the height needs to be changed.

The skip-list's nodes are implemented via fine-grained Linked List.
-}

{-# LANGUAGE FlexibleContexts #-}

module Data.STM.PriorityQueue.Internal.LLSLPQ(
  LLSLPQ,
  new'
) where

import Control.Monad.STM
import Control.Concurrent.STM
import System.IO.Unsafe
import System.Random.PCG.Fast (createSystemRandom, uniform, GenIO)

import Data.STM.PriorityQueue.Class

data Node k v
  = Nil
  | Node
  { _getKey   :: k
  , _getVal   :: TVar v
  , _getUp    :: TNode k v
  , _getNext  :: TNode k v
  , _getDown  :: TNode k v
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

-- | Abbreviation stands for Linked List (-based) Skip-List Priority Queue
data LLSLPQ k v
  = PQ
  { _getTop       :: TNode k v  -- top-node of the main layout
  , _getBottom    :: TNode k v  -- bottom-node of the main layout
  , _getHeight    :: TVar Int   -- height of the main layout
  , _getNil       :: TNode k v  -- pointer to Nil shared by all nodes
  , _getGenIO     :: TVar GenIO -- RNG
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

-- | Parameterizing constructor which determines
-- maximum height of skip-list node.
new' :: Ord k => Int -> STM (LLSLPQ k v)
new' height = do
  nil' <- newTVar Nil
  (top', bottom') <- buildHeads nil' height
  height' <- newTVar height
  gio' <- newTVar $ unsafePerformIO createSystemRandom
  return $ PQ top' bottom' height' nil' gio'

pqNew :: Ord k => STM (LLSLPQ k v)
pqNew = new' 16

logHalf :: Float
logHalf = log 0.5

chooseLvl :: GenIO -> Int -> Int
chooseLvl g h =
  min h $ 1 + truncate (log x / logHalf)
    where x = unsafePerformIO (uniform g :: IO Float)

pqInsert :: Ord k => LLSLPQ k v -> k -> v -> STM ()
pqInsert (PQ top' _ height' nil' gio') k v = do
  top <- readTVar top'
  case top of
    Nil -> error "Illegal state: top must not be Nil"
    _ -> do
      prevs <- buildPrevs top' []
      height <- readTVar height'
      v' <- newTVar v
      gio <- readTVar gio'
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

pqPeekMin :: Ord k => LLSLPQ k v -> STM v
pqPeekMin (PQ _ bottom' _ _ _) = do
  bottom <- readTVar bottom'
  case bottom of
    Nil -> error "Illegal state: bottom must not be Nil"
    (Node _ _ _ next' _) -> do
      next <- readTVar next'
      case next of
        Nil -> retry
        (Node _ v' _ _ _) -> readTVar v'

pqDeleteMin :: Ord k => LLSLPQ k v -> STM v
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
        recDel (Node _ _ headUp' next'' _)
               (Node _ _ up' nextnext' _) = do
                 nextnext <- readTVar nextnext'
                 up <- readTVar up'
                 headUp <- readTVar headUp'
                 writeTVar next'' nextnext
                 recDel headUp up
        recDel _ _ = error "Illegal state"

instance PriorityQueue LLSLPQ where
    new            = pqNew
    insert         = pqInsert
    peekMin        = pqPeekMin
    deleteMin      = pqDeleteMin
