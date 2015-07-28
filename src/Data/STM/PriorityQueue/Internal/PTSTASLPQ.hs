{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.STM.PriorityQueue.Internal.PTSTASLPQ(
    PTSTASLPQ
) where

import Data.Array.MArray
import Control.Monad.STM
import Control.Monad
import Control.Concurrent.STM
import qualified System.Random.PCG.Fast as R
import qualified Data.Vector.Unboxed.Mutable as U
import Data.Vector.Unboxed.Deriving
import System.Random.PCG.Class (sysRandom)
import Data.Word (Word64)
import System.IO.Unsafe
import Control.Concurrent

import Data.STM.PriorityQueue.Class

type Nodes k v = TArray Int (Node k v)

data Node k v = Nil
              | Node
              { getKey   :: k
              , getVal   :: TVar v
              , getNodes :: Nodes k v
              }

data PTSTASLPQ k v = PQ
  { getHeadNodes :: Nodes k v
  , getHeight    :: TVar Int
  , getStates    :: U.IOVector R.FrozenGen
  }

derivingUnbox "FrozenGen"
    [t| R.FrozenGen -> Word64 |]
    [| \fr -> (fst $ R.withFrozen fr R.uniform) :: Word64 |]
    [| R.initFrozen |]

pqNew' :: Ord k => Int -> STM (PTSTASLPQ k v)
pqNew' height = do
  headNodes <- newArray (1, height) Nil
  vHeight <- newTVar height
  let states = unsafeDupablePerformIO $ do
        cn <- getNumCapabilities
        statev <- U.new cn
        forM_ [0..cn-1] $ \i -> do
            s <- sysRandom
            U.write statev i $ R.initFrozen s
        return statev
  return $ PQ headNodes vHeight states

pqNew :: Ord k => STM (PTSTASLPQ k v)
pqNew = pqNew' 16

logHalf :: Float
logHalf = log 0.5

-- Obtains PCG state, generate random Float and store new state
gen :: U.IOVector R.FrozenGen -> Int -> Float
gen v i = unsafeDupablePerformIO $ do
  fr <- U.read v i
  let (r, fr') = R.withFrozen fr R.uniform
  U.write v i fr'
  return r

chooseLvl :: U.IOVector R.FrozenGen -> Int -> Int -> Int
chooseLvl v i h = min h $ 1 + truncate (log (gen v i) / logHalf)

pqInsert :: Ord k => PTSTASLPQ k v -> k -> v -> STM ()
pqInsert (PQ headNodes vHeight states) k v = do
  height <- readTVar vHeight
  prevs <- buildPrevs headNodes height []
  let getCapNum = do
        tid <- myThreadId
        fst `fmap` threadCapability tid
      cn = unsafeDupablePerformIO getCapNum
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
