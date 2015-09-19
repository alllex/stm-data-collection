{-|
Module      : Data.STM.Bag.Internal.RoundRobinBag
Description : Helper STM-based Concurrent Bag data structure
Copyright   : (c) Alex Semin, 2015
License     : BSD3
Maintainer  : alllex.semin@gmail.com
Stability   : experimental
Portability : portable

Data structure which is used as a helper to build more advanced
'Data.STM.Bag.Class' implementations. It employs work-stealing in
a <https://en.wikipedia.org/wiki/Round-robin Round Robin> manner.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

module Data.STM.Bag.Internal.RoundRobinBag(
    RoundRobinBag,
    build,
    add,
    take,
    isEmpty
) where

import Control.Concurrent.STM
import Prelude hiding (take)
import Control.Concurrent
import Data.Array.MArray
import Control.Monad

import GHC.Conc

import qualified Data.STM.Bag.Class as Bag

data RoundRobinBag v where
    B :: Bag.Bag b
      => Int                -- Number of capabilities
      -> TArray Int (b v)   -- Array of thread-local bags
      -> RoundRobinBag v

myCap :: IO Int
myCap = do
    tid <- myThreadId
    c <- fst `fmap` threadCapability tid
--     putStrLn $ "cap " ++ show c
    return c

-- | Given an 'Data.STM.Bag.Class' implementation
-- creates an array of capability-local data structures
-- of length matching the amount of available capabilities.
build :: Bag.Bag b => STM (b v) -> STM (RoundRobinBag v)
build bcons = do
    cn <- unsafeIOToSTM getNumCapabilities
    bags <- newArray_ (0, cn-1)
    forM_ [0..cn-1] $ \i -> bcons >>= writeArray bags i
    return $ B cn bags

-- | /O(1)/. Adding always to capability-local bag.
add :: RoundRobinBag v -> v -> STM ()
add (B _ bags) v = do
    ci <- unsafeIOToSTM myCap
    bag <- readArray bags ci
    Bag.add bag v

-- | /O(1)/. Removing an item from capability-local bag.
-- In case the local bag is empty tries to remove item from
-- another bag.
take :: RoundRobinBag v -> STM v
take (B cn bags) = do
    ci <- unsafeIOToSTM myCap
    go ci
  where
    go ci = take' ci
      where
        take' cj = do
            bag <- readArray bags cj
            isEmp <- Bag.isEmpty bag
            if isEmp then next $ cj + 1
            else Bag.take bag
        next cj = let cj' = cj `mod` cn in
            if cj' == ci then retry
            else take' cj'

-- | /O(1)/. Checks whether the bag is empty
-- by traversing through capability-local bags.
isEmpty :: RoundRobinBag v -> STM Bool
isEmpty (B cn bags) = do
    ci <- unsafeIOToSTM myCap
    go ci
  where
    go ci = isEmpty' ci
     where
      isEmpty' cj = do
          bag <- readArray bags cj
          isEmp <- Bag.isEmpty bag
          if isEmp then next $ cj + 1
          else return False
      next cj = let cj' = cj `mod` cn in
          if cj' == ci then return True
          else isEmpty' cj'
