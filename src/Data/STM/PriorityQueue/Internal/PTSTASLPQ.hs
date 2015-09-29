{-|
Module      : Data.STM.PriorityQueue.Internal.PTSTASLPQ
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
In addition, unboxed RNG seeds are distributed among capabilities
which reduces contention and also accelerates internal random-number generation.

__Default maximum height of skip-list node is 16.__ Use explicit constructor in case
the height needs to be changed.

Note: number of capabilities is not supposed to be changed during execution.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Data.STM.PriorityQueue.Internal.PTSTASLPQ(
    PTSTASLPQ,
    new'
) where

import Data.Array.MArray
import Control.Monad.STM
import Control.Monad
import Control.Concurrent.STM
import qualified System.Random.PCG.Fast.Pure as R
import qualified Data.Vector.Unboxed.Mutable as U
import System.Random.PCG.Class (sysRandom)
import Data.Word (Word64, Word32)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Concurrent

import GHC.Conc

import Data.STM.PriorityQueue.Class

type Nodes k v = TArray Int (Node k v)

data Node k v = Nil
              | Node
              { getKey   :: k
              , getVal   :: TVar v
              , getNodes :: Nodes k v
              }

-- | Abbreviation stands for Per Thread Seed TArray Skip-List Priority Queue
data PTSTASLPQ k v = PQ
  { getHeadNodes :: Nodes k v
  , getHeight    :: TVar Int
  , getStates    :: U.IOVector Word64
  }

-- | Constant for aligning RNG seed to cache-line
-- which usually is 64 Kb long (while seed only is 'Data.Word64').
cacheFactor :: Int
cacheFactor = 8

-- | Parameterizing constructor which determines
-- maximum height of skip-list node.
new' :: Ord k => Int -> STM (PTSTASLPQ k v)
new' height = do
  headNodes <- newArray (1, height) Nil
  vHeight <- newTVar height
  let states = unsafeDupablePerformIO $ do
        cn <- getNumCapabilities
        statev <- U.new (cn * cacheFactor)
        forM_ [0..cn-1] $ \i -> do
            seed <- sysRandom
            U.write statev (i * cacheFactor) seed
        return statev
  return $ PQ headNodes vHeight states

pqNew :: Ord k => STM (PTSTASLPQ k v)
pqNew = new' 16

mbw32f :: Float
mbw32f = fromIntegral (maxBound :: Word32)

logHalf :: Float
logHalf = log 0.5

-- Obtains PCG state, generate random value and store new state
gen :: U.IOVector Word64 -> Int -> Word32
gen v !i = unsafeDupablePerformIO $ do
  let i' = i * cacheFactor
  st <- U.read v i'
  let (R.P st' x) = R.pair st
  U.write v i' st'
  return x

chooseLvl :: U.IOVector Word64 -> Int -> Int -> Int
chooseLvl v !i !h = min h $ (+1) $ truncate $ log x / logHalf
    where x = fromIntegral (gen v i) / mbw32f

pqInsert :: Ord k => PTSTASLPQ k v -> k -> v -> STM ()
pqInsert (PQ headNodes vHeight states) k v = do
  height <- readTVar vHeight
  prevs <- buildPrevs headNodes height []
  cn <- unsafeIOToSTM $ do
        tid <- myThreadId
        fst `fmap` threadCapability tid
  let lvl = chooseLvl states cn height
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
            updatePtrs _ [] = error "PTSTASLPQ: main layout must be not lower than new one"

        updatePtrs 1 prevs

pqPeekMin :: Ord k => PTSTASLPQ k v -> STM v
pqPeekMin (PQ headNodes _ _) = do
  bottom <- readArray headNodes 1
  case bottom of
    Nil -> retry
    (Node _ vv _) -> readTVar vv

pqDeleteMin :: Ord k => PTSTASLPQ k v -> STM v
pqDeleteMin (PQ headNodes _ _) = do
  bottom <- readArray headNodes 1
  case bottom of
    Nil -> retry
    (Node _ vv nodes) -> do
      fstHeight <- snd `fmap` getBounds nodes
      forM_ [1..fstHeight] $ \i -> writeArray headNodes i =<< readArray nodes i
      readTVar vv


instance PriorityQueue PTSTASLPQ where
    new            = pqNew
    insert         = pqInsert
    peekMin        = pqPeekMin
    deleteMin      = pqDeleteMin
