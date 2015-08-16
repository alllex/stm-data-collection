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

type BenchFunc a = Int -> Int -> [IO a] -> IO (Maybe Int)

timing :: (IO () -> IO (Maybe ())) -> Int -> BenchFunc a
timing abort !opCount !numCap !numWork ops = do

    let iops = [ (i, op) | op <- ops, i <- [0..numWork-1] ]
        !perWorker = opCount `div` numWork
        !fstWorker = perWorker + (opCount `mod` numWork)
        work 0 = replicateM_ fstWorker
        work _ = replicateM_ perWorker

    (cws, flags) <- fmap unzip . forM iops $ \(i, op) -> do
        flag <- newEmptyMVar
        let cap = i `mod` numCap
            wrappedWork = work i op >> putMVar flag ()
        return ((cap, wrappedWork), flag)

    performMajorGC
    traceMarkerIO "Right before timing benchmark start"

    startTime <- getTime
    tle <- abort $ do
        forM_ cws $ uncurry forkOn
        mapM_ takeMVar flags
    stopTime <- getTime

    let dt = stopTime - startTime
    return $ case tle of
        Nothing -> Nothing
        _ -> Just $! round $ dt * 1000



throughput :: Int -> BenchFunc a
throughput !period !numCap !numWork ops = do

    let iops = [ (i, op) | op <- ops, i <- [0..numWork-1] ]
    (cs, cws, flags) <- fmap unzip3 $ forM iops $ \(i, op) -> do
        counter <- newIORef 0
        flag <- newEmptyMVar
        let work = forever $ op >> modifyIORef' counter (+1)
            wrappedWork = finally work (putMVar flag ())
            cap = i `mod` numCap
        return (counter, (cap, wrappedWork), flag)

    performMajorGC
    traceMarkerIO "Right before throughput benchmark start"

    ts <- forM cws $ uncurry forkOn
    threadDelay $ period * 1000
    mapM_ killThread ts
    mapM_ takeMVar flags

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
    -> IO Int -> (a -> Int -> IO ()) -> [a]
    -> IO (Maybe Int)
fill !prepTL !numCap !initSize rndKey ins structs = do
    let abort = timeout $ prepTL * 1000
        insOps = [rndKey >>= ins s | s <- structs]
    -- parallel data structure filling
    timing abort initSize numCap numCap insOps

benchmark :: BenchSetting a Int -> IO (BenchReport a Int)
benchmark setting@(
    BenchSetting (BenchStruct name cons insOp delOp)
                 (BenchEnv workersNum capsNum)
                 (BenchProc !initSize !insRate !runsNum !prepTL _)
                 benchCase
  ) = event ("series of runs with: " ++ name) $ do
    g <- createSystemRandom
    let rndInt = uniform g :: IO Int
        rndPerc = (`mod` 101) `fmap` rndInt

        buildOp = opInsDel rndInt rndPerc insRate insOp delOp

        oneRun :: BenchFunc () -> Int -> IO OneBenchStatus
        oneRun bencher scale = event ("benchmark with: " ++ name) $ do
            structs <- replicateM scale cons
            let fillAct = fill prepTL capsNum initSize rndInt insOp structs
                benchAct = bencher workersNum capsNum $ map buildOp structs
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
            -> Int -- scale
            -> IO (Either String (Int, Int))
        manyRuns benchFailMsg bencher scale = do
            rowRes <- replicateM runsNum $ oneRun bencher scale
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
            -> Int -- scale
            -> IO BenchResult
        manyBenchCases benchFailMsg params bencher' wrapper scale = do
            rowRes <- forM params $ \param ->
                manyRuns benchFailMsg (bencher' param) scale
            let mapper (Right r) = Just r
                mapper (Left _) = Nothing
                succRes = mapMaybe mapper rowRes
            return $! wrapper succRes

        bench :: BenchCase -> IO BenchResult
        bench (ThroughputCase scale periods) =
            manyBenchCases "" periods throughput ThroughputRes scale

        bench (TimingCase opCounts timelimit) = do
            let abortOnTimeout = timeout (timelimit * 1000)
                benchFailMsg = ">" ++ show timelimit ++ "ms"
                bencher = timing abortOnTimeout
            manyBenchCases benchFailMsg opCounts bencher TimingRes 1

    performMajorGC
    BenchReport setting `fmap` bench benchCase


foreign import ccall unsafe "gettime" getTime :: IO Double
