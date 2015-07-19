{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Maybe (mapMaybe)
import Data.List (find)
import System.Mem

import BenchData

import Debug.Trace

-- for profiling
event :: String -> IO a -> IO a
event label =
  bracket_ (traceMarkerIO $ "START " ++ label)
           (traceMarkerIO $ "STOP "  ++ label)

data OneBenchStatus
    = PrepFail          -- preparation for benchmark was aborted
    | BenchFail         -- benchmark was aborted
    | BenchSucc Int     -- benchmark finished successfully

execBenchmark :: BenchStruct a Int -> BenchProc -> IO (BenchReport a Int)
execBenchmark strt defProc = do
    capsNum <- getNumCapabilities
    let env = BenchEnv capsNum capsNum
    setting <- buildBenchSetting strt env defProc
    benchmark setting

type BenchFunc a = Int -> Int -> IO a -> IO (Maybe Int)

timing :: (IO () -> IO (Maybe ())) -> Int -> BenchFunc a
timing abort !opCount !numCap !numWork op = do

  let !perWorker = opCount `div` numWork
      !fstWorker = perWorker + (opCount `mod` numWork)
      work' n = forM_ [1..n] $ const op
      work 1 = work' fstWorker
      work _ = work' perWorker
      wi = [1..numWork]

  (cws, vs) <- fmap unzip . forM wi $ \i -> do
    v <- newEmptyMVar
    return ((i `mod` numCap, work i >> putMVar v ()), v)

  performMajorGC
  traceMarkerIO "Right before timing benchmark start"

  startTime <- getTime
  tle <- abort $ do
    forM_ cws $ uncurry forkOn
    mapM_ takeMVar vs
  stopTime <- getTime

  let dt = stopTime - startTime
  return $ case tle of
      Nothing -> Nothing
      _ -> Just $! round $ dt * 1000


throughput :: Int -> BenchFunc a
throughput !period !numCap !numWork qop = do

  cs <- replicateM numWork $ newIORef 0
  (cws, vs) <- fmap unzip . forM (zip cs [0..numWork]) $ \(c, i) -> do
    v <- newEmptyMVar
    let work = forever $ qop >> modifyIORef' c (+1)
        wrappedWork = finally work (putMVar v ())
        cap = i `mod` numCap
    return ((cap, wrappedWork), v)

  performMajorGC
  traceMarkerIO "Right before throughput benchmark start"

  ts <- forM cws $ uncurry forkOn
  threadDelay $ period * 1000
  mapM_ killThread ts
  mapM_ takeMVar vs

  (Just . sum) `fmap` mapM readIORef cs

res2disp :: [Int] -> (Int, Int)
res2disp !rs = (mean, d)
    where mean = sum rs `div` length rs
          (mn, mx) = (minimum rs, maximum rs)
          !d = (mx - mean) `min` (mean - mn)

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

fill
    :: Int -> Int -> Int
    -> IO Int -> (a -> Int -> IO ()) -> a
    -> IO (Maybe Int)
fill !prepTL !numCap !initSize rndKey ins struct = do
    let abort = timeout $ prepTL * 1000
    -- parallel data structure filling
    timing abort initSize numCap numCap $ rndKey >>= ins struct

benchmark :: BenchSetting a Int -> IO (BenchReport a Int)
benchmark setting@(
    BenchSetting (BenchStruct name cons insOp delOp)
                 (BenchEnv workersNum capsNum)
                 (BenchProc !initSize !insRate !runsNum !prepTL)
                 benchCase
  ) = event ("series of runs with: " ++ name) $ do
    g <- createSystemRandom
    let rndInt = uniform g :: IO Int
        rndPerc = (`mod` 101) `fmap` rndInt

        buildOp = opInsDel rndInt rndPerc insRate insOp delOp

        oneRun :: BenchFunc () -> IO OneBenchStatus
        oneRun bencher = event ("benchmark with: " ++ name) $ do
            struct <- cons
            let fillAct = fill prepTL capsNum initSize rndInt insOp struct
                benchAct = bencher workersNum capsNum $ buildOp struct
                fillAct' = event "filling structure" fillAct
                benchAct' = event "bench itself" benchAct

            tle <- fillAct'
            performMajorGC
            case tle of
                Nothing -> return PrepFail
                _ -> benchAct' >>= \case
                        Nothing -> return BenchFail
                        (Just res) -> return $ BenchSucc res

        manyRuns
            :: String
            -> BenchFunc ()
            -> IO (Either String (Int, Int))
        manyRuns benchFailMsg bencher = do
            rowRes <- replicateM runsNum $ oneRun bencher
            let prepFailed = find (\case PrepFail -> True; _ -> False) rowRes
                prepFailMsg = "prep >" ++ show prepTL ++ "ms"
            case prepFailed of
              (Just PrepFail) -> return $ Left prepFailMsg
              _ -> do
                let succRes = filter (\case BenchSucc _ -> True; _ -> False) rowRes
                if 100 * length succRes < 75 * length rowRes -- if more than 25% failures
                then return $ Left benchFailMsg
                else return . Right . res2disp $ map (\(BenchSucc r) -> r) succRes

        manyBenchCases
            :: String
            -> [a]
            -> (a -> BenchFunc ())
            -> ([(Int, Int)] -> BenchResult)
            -> IO BenchResult
        manyBenchCases benchFailMsg params bencher' wrapper = do
            rowRes <- forM params $ \param ->
                manyRuns benchFailMsg (bencher' param)
            let mapper (Right r) = Just r
                mapper (Left _) = Nothing
                succRes = mapMaybe mapper rowRes
            return $! wrapper succRes

        bench :: BenchCase -> IO BenchResult
        bench (ThroughputCase periods) =
            manyBenchCases "" periods throughput ThroughputRes

        bench (TimingCase opCounts timelimit) = do
            let abortOnTimeout = timeout (timelimit * 1000)
                benchFailMsg = ">" ++ show timelimit ++ "ms"
                bencher = timing abortOnTimeout
            manyBenchCases benchFailMsg opCounts bencher TimingRes

    performMajorGC
    BenchReport setting `fmap` bench benchCase


foreign import ccall unsafe "gettime" getTime :: IO Double
