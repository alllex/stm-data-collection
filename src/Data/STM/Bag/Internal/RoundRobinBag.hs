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

import qualified Data.STM.Bag.Class as Bag

data RoundRobinBag v where
    B :: Bag.Bag b
      => Int                -- Number of capabilities
      -> TArray Int (b v)   -- Array of thread-local bags
      -> RoundRobinBag v

myCap :: Int
myCap = unsafeDupablePerformIO $ do
    tid <- myThreadId
    fst `fmap` threadCapability tid

capNum :: Int
capNum = unsafeDupablePerformIO getNumCapabilities

build :: Bag.Bag b => STM (b v) -> STM (RoundRobinBag v)
build bcons = do
    let cn = capNum
    bags <- newArray_ (0, cn-1)
    forM_ [0..cn-1] $ \i -> bcons >>= writeArray bags i
    return $ B cn bags

add :: RoundRobinBag v -> v -> STM ()
add (B _ bags) v = do
    let !ci = myCap
    bag <- readArray bags ci
    Bag.add bag v

take :: RoundRobinBag v -> STM v
take (B cn bags) = take' ci where
    !ci = myCap
    take' cj = do
        bag <- readArray bags cj
        isEmp <- Bag.isEmpty bag
        if isEmp then next $ cj + 1
        else Bag.take bag
    next cj = let cj' = cj `mod` cn in
        if cj' == ci then retry
        else take' cj'

isEmpty :: RoundRobinBag v -> STM Bool
isEmpty (B cn bags) = isEmpty' ci where
    !ci = myCap
    isEmpty' cj = do
        bag <- readArray bags cj
        isEmp <- Bag.isEmpty bag
        if isEmp then next $ cj + 1
        else return False
    next cj = let cj' = cj `mod` cn in
        if cj' == ci then return True
        else isEmpty' cj'
