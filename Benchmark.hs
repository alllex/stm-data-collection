{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

module Benchmark (
    module BenchData,
    execBenchmark
) where

import Control.Monad
import Control.Exception (finally, bracket_)
import Control.Concurrent
import System.Random.PCG.Fast (createSystemRandom, uniform)
import System.Timeout
import Data.IORef
import Data.Maybe (catMaybes)

import BenchData

import Debug.Trace

event :: String -> IO a -> IO a
event label =
  bracket_ (traceMarkerIO $ "START " ++ label)
           (traceMarkerIO $ "STOP "  ++ label)

execBenchmark :: BenchStruct a Int -> BenchProc -> IO (BenchReport a Int)
execBenchmark strt defProc = do
    capsNum <- getNumCapabilities
    let env = BenchEnv capsNum capsNum
    setting <- buildBenchSetting strt env defProc
    benchmark setting

timing :: Int -> Int -> Int -> IO b -> IO Int
timing !opCount !numCap !numWork op = do

  let !perWorker = opCount `div` numWork
      !fstWorker = perWorker + (opCount `mod` numWork)
      work' n = forM_ [1..n] $ const op
      work 1 = work' fstWorker
      work _ = work' perWorker
      wi = [1..numWork]

  (wis, vs) <- fmap unzip . forM wi $ \i -> do
    v <- newEmptyMVar
    return ((i, work i >> putMVar v ()), v)

  startTime <- getTime
  forM_ wis $ \(i, w) -> forkOn (i `mod` numCap) w
  mapM_ takeMVar vs
  stopTime <- getTime

  let dt = stopTime - startTime
  return $! round $ dt * 1000


throughput :: Int -> Int -> Int -> IO a -> IO Int
throughput !period !numCap !numWork qop = do

  cs <- replicateM numWork $ newIORef 0
  (ts, vs) <- fmap unzip . forM (zip cs [0..numWork]) $ \(c, i) -> do
    v <- newEmptyMVar
    let work = forever $ qop >> modifyIORef' c (+1)
    t <- forkOn (i `mod` numCap) $ finally work (putMVar v ())
    return (t, v)

  threadDelay $ period * 1000
  mapM_ killThread ts
  mapM_ takeMVar vs

  sum `fmap` mapM readIORef cs

res2disp :: [Int] -> (Int, Int)
res2disp !rs = (mn + d, d)
    where (mn, mx) = (minimum rs, maximum rs)
          !d = (mx - mn) `div` 2

opInsDel
    :: IO Int -- random key generator
    -> IO Int -- random percent generator
    -> Int    -- insertion rate
    -> (a -> Int -> IO ()) -- insertion operation
    -> (a -> IO ())  -- deletion operation
    -> a      -- struct
    -> IO ()
opInsDel rndKey rndPer !insRate ins del struct = do
    percent <- rndPer
    if percent < insRate
        then rndKey >>= ins struct
        else del struct

fill :: Int -> Int -> IO Int -> (a -> Int -> IO ()) -> a -> IO ()
fill !numCap !initSize rndKey ins struct = do
    -- parallel data structure filling
    timing initSize numCap numCap $ rndKey >>= ins struct
    return () -- discarding result

benchmark :: BenchSetting a Int -> IO (BenchReport a Int)
benchmark setting@(
    BenchSetting (BenchStruct name cons insOp delOp)
                 (BenchEnv workersNum capsNum)
                 (BenchProc !initSize !insRate !runsNum)
                 benchCase
  ) = event ("series of runs with: " ++ name) $ do
    g <- createSystemRandom
    let rndInt = uniform g :: IO Int
        rndPerc = (`mod` 101) `fmap` rndInt
        buildOp = opInsDel rndInt rndPerc insRate insOp delOp
        oneBench bencher = event ("benchmark with: " ++ name) $ do
            struct <- cons
            event "filling structure" $ fill capsNum initSize rndInt insOp struct
            event "bench itself" $ bencher workersNum capsNum $ buildOp struct

        bench (ThroughputCase period) = do
            let bench' = oneBench $ throughput period
            (r, d) <- res2disp `fmap` replicateM runsNum bench'
            return $! ThroughputRes r d

        bench (TimingCase opCount timelimit) = do
            let abortOnTimeout = timeout (timelimit * 1000)
                bench' = abortOnTimeout $ oneBench $ timing opCount
                abortedMsg = AbortedRes $ ">" ++ show timelimit ++ "ms"
            rs <- catMaybes `fmap` replicateM runsNum bench'
            return $! case rs of
                [] -> abortedMsg
                _ -> uncurry TimingRes $ res2disp rs

    BenchReport setting `fmap` bench benchCase


foreign import ccall unsafe "gettime" getTime :: IO Double
