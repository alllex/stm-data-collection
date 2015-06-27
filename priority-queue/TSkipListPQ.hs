{-# LANGUAGE FlexibleContexts #-}

module TSkipListPQ(
    TSkipListPQ
) where

import Data.Array.MArray
import Control.Monad.STM
import Control.Monad
import Control.Concurrent.STM
import System.IO.Unsafe
import System.Random(randomR, newStdGen)

import PriorityQueue

type Nodes k v = TArray Int (Node k v)

data Node k v = Nil
              | Node
              { getKey   :: k
              , getVal   :: TVar v
              , getNodes :: Nodes k v
              }

data TSkipListPQ k v = PQ
  { getHeadNodes :: Nodes k v
  , getHeight    :: TVar Int
  }


pqNew' :: Ord k => Int -> STM (TSkipListPQ k v)
pqNew' height = do
  headNodes <- newArray (1, height) Nil
  vHeight <- newTVar $ height
  return $ PQ headNodes vHeight


pqNew :: Ord k => STM (TSkipListPQ k v)
pqNew = pqNew' 16

logHalf :: Float
logHalf = log 0.5

chooseLvl :: Int -> Int
chooseLvl h =
  min h $ 1 + truncate (log x / logHalf)
    where x = fst $ randomR (0.0, 1.0) (unsafePerformIO newStdGen)


pqInsert :: Ord k => TSkipListPQ k v -> k -> v -> STM ()
pqInsert (PQ headNodes vHeight) k v = do
  height <- readTVar vHeight
  prevs <- buildPrevs headNodes height []
  insertNode (chooseLvl height) prevs
    where
      -- buildPrevs :: Nodes k v -> Int -> [Nodes k v]
      buildPrevs _ 0 prevs = return prevs
      buildPrevs nodes lvl prevs = do
        next <- readArray nodes lvl
        case next of
          Nil -> buildPrevs nodes (lvl-1) (nodes:prevs)
          (Node k' _ nodes') ->
            if k' > k then buildPrevs nodes (lvl-1) (nodes:prevs)
            else buildPrevs nodes' lvl prevs

      -- insertNode :: Int -> [Nodes k v]
      insertNode nodesHeight prevs = do
        nodes <- newArray_ (1, nodesHeight)
        vv <- newTVar v
        let newNode = Node k vv nodes
            -- updatePtrs :: Int -> [Nodes k v]
            updatePtrs lvl (p:ps) =
              if lvl > nodesHeight then return ()
              else do
                nextNode <- readArray p lvl
                writeArray p lvl newNode
                writeArray nodes lvl nextNode
                updatePtrs (lvl+1) ps
            updatePtrs _ [] = error "TSkipList: head-nodes height not less than height of new node"

        updatePtrs 1 prevs


pqPeekMin :: Ord k => TSkipListPQ k v -> STM v
pqPeekMin (PQ headNodes _) = do
  bottom <- readArray headNodes 1
  case bottom of
    Nil -> retry
    (Node _ vv _) -> readTVar vv


pqDeleteMin :: Ord k => TSkipListPQ k v -> STM v
pqDeleteMin (PQ headNodes _) = do
  bottom <- readArray headNodes 1
  case bottom of
    Nil -> retry
    (Node _ vv nodes) -> do
      fstHeight <- snd `fmap` getBounds nodes
      forM_ [1..fstHeight] $ \i -> writeArray headNodes i =<< readArray nodes i
      readTVar vv


pqTryDeleteMin:: Ord k => TSkipListPQ k v -> STM (Maybe v)
pqTryDeleteMin pq = (Just `fmap` pqDeleteMin pq) `orElse` return Nothing

instance PriorityQueue TSkipListPQ where
    new            = pqNew
    insert         = pqInsert
    peekMin        = pqPeekMin
    deleteMin      = pqDeleteMin
    tryDeleteMin   = pqTryDeleteMin

