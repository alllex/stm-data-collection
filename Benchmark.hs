{-# LANGUAGE ForeignFunctionInterface #-}

module Benchmark (
    module BenchData,
    execBenchmark
) where

import Control.Monad
import Control.Exception (finally)
import Control.Concurrent
import System.Random.PCG.Fast (createSystemRandom, uniform)
import System.Timeout
import Data.IORef
import Data.Maybe (catMaybes)

import BenchData

execBenchmark :: BenchStruct Int -> BenchProc -> IO (BenchReport Int)
execBenchmark strt defProc = do
    capsNum <- getNumCapabilities
    let env = BenchEnv capsNum capsNum
    setting <- buildBenchSetting strt env defProc
    benchmark setting

timing :: Int -> Int -> Int -> IO b -> IO Int
timing opCount numCap numWork op = do

  let perWorker = opCount `div` numWork
      fstWorker = perWorker + (opCount `mod` numWork)
      work' n = forM_ [1..n] $ const op
      work 1 = work' fstWorker
      work _ = work' perWorker

  (ws, vs) <- fmap unzip . forM [1..numWork] $ \i -> do
    v <- newEmptyMVar
    return (work i >> putMVar v (), v)

  startTime <- getTime
  forM_ (zip [1..] ws) $ \(i, w) -> forkOn (i `mod` numCap) w
  mapM_ takeMVar vs
  stopTime <- getTime

  let dt = stopTime - startTime
  return $! round $ dt * 1000


throughput :: Int -> Int -> Int -> IO a -> IO Int
throughput period numCap numWork qop = do

  cs <- replicateM numWork $ newIORef 0
  (ts, vs) <- fmap unzip . forM (zip cs [1..]) $ \(c, i) -> do
    v <- newEmptyMVar
    let work = forever $ qop >> modifyIORef' c (+1)
    t <- forkOn (i `mod` numCap) $ finally work (putMVar v ())
    return (t, v)

  threadDelay $ period * 1000
  mapM_ killThread ts
  mapM_ takeMVar vs

  sum `fmap` mapM readIORef cs

res2disp :: [Int] -> (Int, Int)
res2disp rs = (mn + d, d)
    where (mn, mx) = (minimum rs, maximum rs)
          d = (mx - mn) `div` 2

opInsDel
    :: IO Int -- random key generator
    -> IO Int -- random percent generator
    -> Int    -- insertion rate
    -> (Int -> IO ()) -- insertion operation
    -> IO ()  -- deletion operation
    -> IO ()
opInsDel rndKey rndPer insRate ins del = do
    percent <- rndPer
    if percent < insRate
        then rndKey >>= ins
        else del

fill :: Int -> IO Int -> (Int -> IO ()) -> IO ()
fill initSize rndKey insOp =
    replicateM_ initSize $ rndKey >>= insOp

benchmark :: BenchSetting Int -> IO (BenchReport Int)
benchmark setting@(
    BenchSetting (BenchStruct name insOp delOp)
                 (BenchEnv workersNum capsNum)
                 (BenchProc initSize insRate runsNum)
                 benchCase
  ) = do
    g <- createSystemRandom
    let randomInt = uniform g :: IO Int
        randomPercent = (`mod` 101) `fmap` randomInt
        op = opInsDel randomInt randomPercent insRate insOp delOp
        oneBench bencher = do
            fill initSize randomInt insOp
            bencher workersNum capsNum op

        bench (ThroughputCase period) = do
            let bench' = oneBench $ throughput period
            (r, d) <- res2disp `fmap` replicateM runsNum bench'
            return [(name, ThroughputRes r d)]

        bench (TimingCase opCount timelimit) = do
            let abortOnTimeout = timeout (timelimit * 1000)
                bench' = abortOnTimeout $ oneBench $ timing opCount
                abortedMsg = AbortedRes $ ">" ++ show timelimit ++ "ms"
            rs <- catMaybes `fmap` replicateM runsNum bench'
            let res = case rs of [] -> abortedMsg
                                 _ -> uncurry TimingRes $ res2disp rs
            return [(name, res)]

    BenchReport setting `fmap` bench benchCase


foreign import ccall unsafe "gettime" getTime :: IO Double
