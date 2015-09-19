{-|
Module      : Data.STM.PriorityQueue.Internal.TASLPQ
Description : STM-based Concurrent Priority Queue data structure class implementation
Copyright   : (c) Alex Semin, 2015
License     : BSD3
Maintainer  : alllex.semin@gmail.com
Stability   : experimental
Portability : portable

An implementation of 'Data.STM.PriorityQueue.Class' based on skip-list.
| Expected time complexity of deletion is /O(1)/, while insertion still
normally has logarithmic complexity.
| The skip-list's nodes are implemented via 'Control.Concurrent.STM.TArray'.
-}

{-# LANGUAGE FlexibleContexts #-}

module Data.STM.PriorityQueue.Internal.TASLPQ(
    TASLPQ
) where

import Data.Array.MArray
import Control.Monad.STM
import Control.Monad
import Control.Concurrent.STM
import System.Random.PCG.Fast (createSystemRandom, uniform, GenIO)
import System.IO.Unsafe

import Data.STM.PriorityQueue.Class

type Nodes k v = TArray Int (Node k v)

data Node k v = Nil
              | Node
              { getKey   :: k
              , getVal   :: TVar v
              , getNodes :: Nodes k v
              }

-- | Abbreviation stands for TArray (-based) Skip-List Priority Queue
data TASLPQ k v = PQ
  { getHeadNodes :: Nodes k v
  , getHeight    :: TVar Int
  , getGen       :: TVar GenIO
  }

pqNew' :: Ord k => Int -> STM (TASLPQ k v)
pqNew' height = do
  headNodes <- newArray (1, height) Nil
  vHeight <- newTVar $ height
  gio' <- newTVar $ unsafePerformIO createSystemRandom
  return $ PQ headNodes vHeight gio'

pqNew :: Ord k => STM (TASLPQ k v)
pqNew = pqNew' 16

logHalf :: Float
logHalf = log 0.5

chooseLvl :: GenIO -> Int -> Int
chooseLvl g h =
  min h $ 1 + truncate (log x / logHalf)
    where x = unsafePerformIO (uniform g :: IO Float)

pqInsert :: Ord k => TASLPQ k v -> k -> v -> STM ()
pqInsert (PQ headNodes vHeight gio') k v = do
  height <- readTVar vHeight
  prevs <- buildPrevs headNodes height []
  gio <- readTVar gio'
  let lvl = chooseLvl gio height
  insertNode lvl prevs
    where
      buildPrevs _ 0 prevs = return prevs
      buildPrevs nodes lvl prevs = do
        next <- readArray nodes lvl
        case next of
          Nil -> buildPrevs nodes (lvl-1) (nodes:prevs)
          (Node k' _ nodes') ->
            if k' > k then buildPrevs nodes (lvl-1) (nodes:prevs)
            else buildPrevs nodes' lvl prevs

      insertNode nodesHeight prevs = do
        nodes <- newArray_ (1, nodesHeight)
        vv <- newTVar v
        let newNode = Node k vv nodes
            updatePtrs lvl _ | lvl > nodesHeight = return ()
            updatePtrs lvl (p:ps) = do
                nextNode <- readArray p lvl
                writeArray p lvl newNode
                writeArray nodes lvl nextNode
                updatePtrs (lvl+1) ps
            updatePtrs _ [] = error "TASLPQ: main layout must be not lower than new one"

        updatePtrs 1 prevs

pqPeekMin :: Ord k => TASLPQ k v -> STM v
pqPeekMin (PQ headNodes _ _) = do
  bottom <- readArray headNodes 1
  case bottom of
    Nil -> retry
    (Node _ vv _) -> readTVar vv

pqDeleteMin :: Ord k => TASLPQ k v -> STM v
pqDeleteMin (PQ headNodes _ _) = do
  bottom <- readArray headNodes 1
  case bottom of
    Nil -> retry
    (Node _ vv nodes) -> do
      fstHeight <- snd `fmap` getBounds nodes
      forM_ [1..fstHeight] $ \i -> writeArray headNodes i =<< readArray nodes i
      readTVar vv


instance PriorityQueue TASLPQ where
    new            = pqNew
    insert         = pqInsert
    peekMin        = pqPeekMin
    deleteMin      = pqDeleteMin
