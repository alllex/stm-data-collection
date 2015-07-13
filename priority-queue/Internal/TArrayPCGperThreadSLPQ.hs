{-# LANGUAGE FlexibleContexts #-}

module Internal.TArrayPCGperThreadSLPQ(
    TArrayPCGperThreadSLPQ
) where

import Data.Array.MArray
import Control.Monad.STM
import Control.Monad
import Control.Concurrent.STM
import System.Random.PCG.Fast (createSystemRandom, uniform, GenIO)
import System.IO.Unsafe
import Control.Concurrent

import PriorityQueue

type Nodes k v = TArray Int (Node k v)

data Node k v = Nil
              | Node
              { getKey   :: k
              , getVal   :: TVar v
              , getNodes :: Nodes k v
              }

data TArrayPCGperThreadSLPQ k v = PQ
  { getHeadNodes :: Nodes k v
  , getHeight    :: TVar Int
  , getGen       :: TArray Int GenIO
  }


pqNew' :: Ord k => Int -> STM (TArrayPCGperThreadSLPQ k v)
pqNew' height = do
  headNodes <- newArray (1, height) Nil
  vHeight <- newTVar $ height
  let cn = unsafePerformIO getNumCapabilities
  gios' <- newArray (1, cn) $ unsafePerformIO createSystemRandom
  return $ PQ headNodes vHeight gios'


pqNew :: Ord k => STM (TArrayPCGperThreadSLPQ k v)
pqNew = pqNew' 16

logHalf :: Float
logHalf = log 0.5

chooseLvl :: GenIO -> Int -> Int
chooseLvl g h =
  min h $ 1 + truncate (log x / logHalf)
    where x = unsafePerformIO (uniform g :: IO Float)

pqInsert :: Ord k => TArrayPCGperThreadSLPQ k v -> k -> v -> STM ()
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
            updatePtrs _ [] = error "TArrayPCGperThreadSLPQ: main layout must be not lower than new one"

        updatePtrs 1 prevs


pqPeekMin :: Ord k => TArrayPCGperThreadSLPQ k v -> STM v
pqPeekMin (PQ headNodes _ _) = do
  bottom <- readArray headNodes 1
  case bottom of
    Nil -> retry
    (Node _ vv _) -> readTVar vv


pqDeleteMin :: Ord k => TArrayPCGperThreadSLPQ k v -> STM v
pqDeleteMin (PQ headNodes _ _) = do
  bottom <- readArray headNodes 1
  case bottom of
    Nil -> retry
    (Node _ vv nodes) -> do
      fstHeight <- snd `fmap` getBounds nodes
      forM_ [1..fstHeight] $ \i -> writeArray headNodes i =<< readArray nodes i
      readTVar vv


pqTryDeleteMin:: Ord k => TArrayPCGperThreadSLPQ k v -> STM (Maybe v)
pqTryDeleteMin pq = (Just `fmap` pqDeleteMin pq) `orElse` return Nothing

instance PriorityQueue TArrayPCGperThreadSLPQ where
    new            = pqNew
    insert         = pqInsert
    peekMin        = pqPeekMin
    deleteMin      = pqDeleteMin
    tryDeleteMin   = pqTryDeleteMin

