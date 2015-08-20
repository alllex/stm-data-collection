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
import System.IO.Unsafe (unsafeDupablePerformIO)
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

capNum :: Int
capNum = unsafeDupablePerformIO getNumCapabilities

build :: Bag.Bag b => STM (b v) -> STM (RoundRobinBag v)
build bcons = do
    cn <- unsafeIOToSTM getNumCapabilities
    bags <- newArray_ (0, cn-1)
    forM_ [0..cn-1] $ \i -> bcons >>= writeArray bags i
    return $ B cn bags

add :: RoundRobinBag v -> v -> STM ()
add (B _ bags) v = do
    ci <- unsafeIOToSTM myCap
    bag <- readArray bags ci
    Bag.add bag v

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
