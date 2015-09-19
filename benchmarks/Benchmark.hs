{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Benchmark (
    module BenchData,
    execBenchmark
) where

import Control.Monad
import Control.Applicative
import Data.Foldable (for_)
import Control.Exception (finally, bracket_)
import Control.Concurrent
import System.Random.PCG.Fast (createSystemRandom, uniform)
import System.Timeout
import Data.IORef
import Data.Maybe (mapMaybe)
import Data.List (find)
import System.Mem
import Data.Array

import BenchData

import Debug.Trace

-- for profiling
event :: String -> IO a -> IO a
event label =
  bracket_ (traceMarkerIO $ "START " ++ label)
           (traceMarkerIO $ "STOP "  ++ label)

data OneBenchStatus
    = PrepFail  -- ^ Preparation for benchmark was aborted
    | BenchFail -- ^ Benchmark was aborted
    | BenchSucc (Int, Double)  -- ^ Benchmark finished successfully

-- | Main function to execute benchmarks using this framework.
-- | Note: Number of capabilities isn't supposed to be changed during execution .
execBenchmark
    :: BenchStruct a Int        -- ^ Interface for working with data structure.
    -> BenchProc                -- ^ Dafault parameters for benchmark procedure.
    -> IO (BenchReport a Int)   -- ^ Benchmark execution action.
execBenchmark strt defProc = do
    capsNum <- getNumCapabilities
    let env = BenchEnv capsNum capsNum
    setting <- buildBenchSetting strt env defProc
    benchmark setting

-- | Common parameters for benchmark cases.
type BenchFunc a
    =  Int  -- ^ Number of capabilities.
    -> Int  -- ^ Number of worker threads.
    -> IO a -- ^ Single operation for worker.
    -> IO (Maybe (Int, Double)) -- ^ Result and estimated time of execution

-- | Benchmark case for measuring amount of time needed to complete
--   given amount of operations upon data structure.
timing
    :: (IO () -> IO (Maybe ())) -- ^ Abortion wrapper in case benchmark executing too long.
    -> Int                      -- ^ Amount of operations to perform.
    -> BenchFunc a              -- ^ Common benchmark parameters (see 'BenchFunc').
timing abort !opCount !numCap !numWork op = do

    let !perWorker = opCount `div` numWork
        !fstWorker = perWorker + (opCount `mod` numWork)
        work 0 = replicateM_ fstWorker
        work _ = replicateM_ perWorker

    (cws, flags) <- fmap unzip . forM [0..numWork-1] $ \i -> do
        flag <- newEmptyMVar -- empty holder for the flag of finished execution
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

    let !dt = stopTime - startTime :: Double -- measured time in seconds
        !dtms = round $ dt * 1000 :: Int -- measured time in milliseconds
    return $ case tle of -- Time Limit Exceeded (abort has triggered)
        Nothing -> Nothing
        _ -> Just (dtms, dt) -- pair here for consistency with BenchFunc


-- | Benchmark case for measuring amount of succeeded operations upon
--   data structure for given period of time.
throughput
    :: Int          -- ^ Period of time in milliseconds.
    -> BenchFunc a  -- ^ Common benchmark parameters (see 'BenchFunc').
throughput !period !numCap !numWork op = do

    (cs, cws, flags) <- fmap unzip3 $ forM [0..numWork-1] $ \i -> do
        counter <- newIORef (0 :: Int)-- counter for completed operations
        flag <- newEmptyMVar  -- empty holder for the flag of finished execution
        let work = forever $ op >> modifyIORef' counter (+1)
            -- work is wrapped in 'finally' because it is stopped by throwing exception
            wrappedWork = finally work (putMVar flag ())
            cap = i `mod` numCap
        return (counter, (cap, wrappedWork), flag)

    performMajorGC
    traceMarkerIO "Right before throughput benchmark start"

    startTime <- getTime
    ts <- forM cws $ uncurry forkOn
    threadDelay $ period * 1000 -- threadDelay takes microseconds
    mapM_ killThread ts
    mapM_ takeMVar flags -- waiting for threads to handle exceptions
    stopTime <- getTime

    count' <- sum <$> mapM readIORef cs -- summed up result of all counters
    let !dt = stopTime - startTime :: Double -- actual time period in seconds
        !dtms = dt * 1000 :: Double -- actual time period in milliseconds
        !ratio = fromIntegral period / dtms :: Double
        !count = round $! (fromIntegral count' :: Double) * ratio :: Int
    return $ Just (count, dt) -- estimated lower bound of throughput and actual time

-- | Converting results of benchmark to `avg +- err` form.
-- | [<min>---------------<avg>-----<max>]
-- |                        |<---err---->|
res2disp
    :: [Int]        -- ^ Abstract numeric results.
    -> (Int, Int)   -- ^ Arithmetic average and closest bound.
res2disp !rs = (mean, d)
    where mean = sum rs `div` length rs
          (mn, mx) = (minimum rs, maximum rs)
          !d = (mx - mean) `min` (mean - mn)

-- | Builder for simple operation upon data structure.
-- Corresponding to insertion rate operation will either
-- insert (add) a random value or delete (take) one.
-- | Note: as data structure parameter an IO action may be supplied
-- which may yield random data structure each time.
opInsDel
    :: IO Int -- ^ Random key generator
    -> IO Int -- ^ Random percent generator
    -> Int    -- ^ Insertion rate
    -> (a -> Int -> IO ()) -- ^ Insertion operation
    -> (a -> IO ())  -- ^ Deletion operation
    -> IO a      -- ^ Data structure
    -> IO ()
opInsDel rndKey rndPerc !insRate ins del getStruct = do
    struct <- getStruct
    percent <- rndPerc
    if percent < insRate
        then rndKey >>= ins struct
        else del struct


-- | Filling each data structure with needed amount of items before benchamrk.
-- | Uses all avaliable capabilities for efficiency.
fill
    :: Int -- ^ Time limit for the whole filling procedure (in milliseconds)
    -> Int -- ^ Number of capabilities
    -> Int -- ^ Needed size of each structure
    -> IO Int -- ^ Random value generator
    -> (a -> Int -> IO ()) -- ^ Insertion operation.
    -> Array Int a -- ^ List of (empty) data structures
    -> IO (Maybe (Int, Double)) -- ^ TLE indicator
fill !prepTL !numCap !initSize rndKey ins structs = do
    let abort = timeout $ prepTL * 1000
        insOpsAct = for_ structs $ \s -> rndKey >>= ins s
    -- parallel data structure filling
    timing abort initSize numCap numCap insOpsAct

-- | Processes 'BenchSetting' and reports results.
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
            let structsCount = scale * capsNum -- `scale` data structures per capability
                rndStructNum = (`mod` structsCount) `fmap` rndInt
            structs <- replicateM structsCount cons
            let structsArr = listArray (0, structsCount-1) structs
                -- getting random structure from the array on each invocation
                getStructAct = liftM (structsArr !) rndStructNum
                fillAct = fill prepTL capsNum initSize rndInt insOp structsArr
                singleOp = buildOp getStructAct
                benchAct = bencher workersNum capsNum singleOp
                fillAct' = event "filling structure" fillAct
                benchAct' = event "bench itself" benchAct

            tle <- fillAct'
            performMajorGC
            case tle of -- Time Limit Exceeded flag
                Nothing -> return PrepFail -- fail on structure filling step
                _ -> benchAct' >>= \case
                        Nothing -> return BenchFail -- fail on excecution step (due to TLE)
                        (Just res) -> return $ BenchSucc res

        manyRuns
            :: String
            -> BenchFunc ()
            -> Int -- scale
            -> IO (Either String ((Int, Int), Double))
        manyRuns benchFailMsg bencher scale = do
            rowRes <- replicateM runsNum $ oneRun bencher scale
            let prepFailed = find (\case PrepFail -> True; _ -> False) rowRes
                prepFailMsg = "prep >" ++ show prepTL ++ "ms"
            case prepFailed of
              (Just PrepFail) -> return $ Left prepFailMsg
              _ -> do
                let fltr acc (BenchSucc r) = r : acc
                    fltr acc _ = acc
                    succRes = foldl fltr [] rowRes
                if 100 * length succRes < 75 * length rowRes -- if more than 25% failures
                then return $ Left benchFailMsg
                else do
                    let (rs, dts) = unzip succRes
                        !rd = res2disp rs :: (Int, Int)
                        !dt = sum dts / (fromIntegral . length) dts :: Double
                    return $ Right (rd, dt)

        manyBenchCases
            :: String
            -> [a]
            -> (a -> BenchFunc ())
            -> ([((Int, Int), Double)] -> BenchResult)
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
                wrapper = TimingRes . map fst
            manyBenchCases benchFailMsg opCounts bencher wrapper 1

    performMajorGC
    BenchReport setting `fmap` bench benchCase


-- | Returns time in seconds
foreign import ccall unsafe "gettime" getTime :: IO Double
