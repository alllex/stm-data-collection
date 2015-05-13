{-# LANGUAGE TupleSections #-}

import Control.Concurrent.STM
import Control.Monad
import qualified PriorityQueue as PQ
import ListPriorityQueue

assertEq expected actual err_msg = 
    if expected == actual then return ()
    else do
        putStrLn "Assertion on equals failed:"
        putStrLn $ "Expected: " ++ show expected
        putStrLn $ "Actual: " ++ show actual
        putStrLn $ "Error message: " ++ err_msg

test1 = do
    putStrLn ">> Test1"
    pq <- atomically $ PQ.new :: IO (ListPriorityQueue Int Int)
    forM_ (map (1,) [1..8]) $ \(k, v) -> atomically $ PQ.write pq k v
    v <- atomically $ PQ.peek pq
    assertEq 1 v "FIFO property violated on equals priorities"
    atomically $ PQ.write pq 2 42
    v <- atomically $ PQ.read pq
    assertEq 42 v "Higher priority data didn't propagate to queue's head"
    putStrLn "<< Test1"

main = do
    test1