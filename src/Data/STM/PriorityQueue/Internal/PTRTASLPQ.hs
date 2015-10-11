{-|
Module      : Data.STM.PriorityQueue.Internal.PTRTASLPQ
Description : STM-based Concurrent Priority Queue data structure class implementation
Copyright   : (c) Alex Semin, 2015
License     : BSD3
Maintainer  : alllex.semin@gmail.com
Stability   : experimental
Portability : portable

An implementation of 'Data.STM.PriorityQueue.Class' based on skip-list.

Expected time complexity of deletion is /O(1)/, while insertion still
normally has logarithmic complexity.

The skip-list's nodes are implemented via 'Control.Concurrent.STM.TArray'.
In addition, RNG are distributed among capabilities which reduces contention.

__Default maximum height of skip-list node is 16.__ Use explicit constructor in case
the height needs to be changed.

Note: number of capabilities is not supposed to be changed during execution.
-}

{-# LANGUAGE FlexibleContexts #-}

module Data.STM.PriorityQueue.Internal.PTRTASLPQ(
    PTRTASLPQ,
    new'
) where

import Data.Array.MArray
import Control.Monad.STM
import Control.Monad
import Control.Concurrent.STM
import System.Random.PCG.Fast (createSystemRandom, uniform, GenIO)
import System.IO.Unsafe
import Control.Concurrent

import Data.STM.PriorityQueue.Class

type Nodes k v = TArray Int (Node k v)

data Node k v = Nil
              | Node
              { _getKey   :: k
              , _getVal   :: TVar v
              , _getNodes :: Nodes k v
              }

-- | Abbreviation stands for Per Thread TArray (-based) Skip-List Priority Queue
data PTRTASLPQ k v = PQ
  { _getHeadNodes :: Nodes k v
  , _getHeight    :: TVar Int
  , _getGen       :: TArray Int GenIO
  }

-- | Parameterizing constructor which determines
-- maximum height of skip-list node.
new' :: Ord k => Int -> STM (PTRTASLPQ k v)
new' height = do
  headNodes <- newArray (1, height) Nil
  vHeight <- newTVar $ height
  let cn = unsafePerformIO getNumCapabilities
  gios' <- newArray (1, cn) $ unsafePerformIO createSystemRandom
  return $ PQ headNodes vHeight gios'

pqNew :: Ord k => STM (PTRTASLPQ k v)
pqNew = new' 16

logHalf :: Float
logHalf = log 0.5

chooseLvl :: GenIO -> Int -> Int
chooseLvl g h =
  min h $ 1 + truncate (log x / logHalf)
    where x = unsafePerformIO (uniform g :: IO Float)

pqInsert :: Ord k => PTRTASLPQ k v -> k -> v -> STM ()
pqInsert (PQ headNodes vHeight gios') k v = do
  height <- readTVar vHeight
  prevs <- buildPrevs headNodes height []
  let getCapNum = do
        tid <- myThreadId
        fst `fmap` threadCapability tid
      cn = 1 + unsafePerformIO getCapNum
  gio <- readArray gios' cn
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
            updatePtrs _ [] = error "PTRTASLPQ: main layout must be not lower than new one"

        updatePtrs 1 prevs

pqPeekMin :: Ord k => PTRTASLPQ k v -> STM v
pqPeekMin (PQ headNodes _ _) = do
  bottom <- readArray headNodes 1
  case bottom of
    Nil -> retry
    (Node _ vv _) -> readTVar vv

pqDeleteMin :: Ord k => PTRTASLPQ k v -> STM v
pqDeleteMin (PQ headNodes _ _) = do
  bottom <- readArray headNodes 1
  case bottom of
    Nil -> retry
    (Node _ vv nodes) -> do
      fstHeight <- snd `fmap` getBounds nodes
      forM_ [1..fstHeight] $ \i -> writeArray headNodes i =<< readArray nodes i
      readTVar vv


instance PriorityQueue PTRTASLPQ where
    new            = pqNew
    insert         = pqInsert
    peekMin        = pqPeekMin
    deleteMin      = pqDeleteMin
