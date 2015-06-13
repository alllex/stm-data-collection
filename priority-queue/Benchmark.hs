
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ExistentialQuantification #-}


import Control.Monad
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.STM
import System.Random.PCG.Fast (createSystemRandom, uniform)

import Data.IORef

import PriorityQueue
import ListPriorityQueue
import TListPriorityQueue
import HeapPriorityQueue
import THeapPriorityQueue


data BenchmarkCase = Throughput Int -- run period in ms
                   | Timing Int     -- amount of operations

instance Show BenchmarkCase where
  show (Throughput t) = "throughput(" ++ show t ++ "ms)"
  show (Timing c) = "timing("++ show c ++ "ops)"

data BenchmarkResult = ThroughputResult Int
                     | TimingResult Int
                     | Aborted String

instance Show BenchmarkResult where
  show (ThroughputResult c) = "throughput(\t" ++ show c ++ "ops)"
  show (TimingResult t) = "timing(\t" ++ show t ++ "ms)"
  show (Aborted s) = "aborted(" ++ s ++ ")"

best :: [BenchmarkResult] -> Int
best [] = error "There's no best result in empty set"
best ((Aborted _):rs) = best rs
best ((ThroughputResult r):rs) = fst $ foldl best' (0, r) $ zip [1..] rs where
  best' acc@(_, bestRes) (i, (ThroughputResult c)) =
    if c > bestRes then (i, c) else acc
  best' acc (_, (Aborted _)) = acc
  best' _ _ = error "Cannot find best in different cases of benchmark!"
best ((TimingResult r):rs) = fst $ foldl best' (0, r) $ zip [1..] rs where
  best' acc@(_, bestRes) (i, (TimingResult c)) =
    if c < bestRes then (i, c) else acc
  best' acc (_, (Aborted _)) = acc
  best' _ _ = error "Cannot find best in different cases of benchmark!"


data BenchmarkSetting = BenchmarkSetting {
          numCapabilities :: Int,
          initialSize :: Int,
          insertionRate :: Int,
          numWorkers :: Int,
          benchmarkCase :: BenchmarkCase
}

instance Show BenchmarkSetting where
  show (BenchmarkSetting numCap initSize insRate numWork bCase) =
    "Benchmark[" ++
      show numCap ++ " cores, on queue with " ++
      show initSize ++ " items, " ++
      show insRate ++ "% insertions, " ++
      show numWork ++ " workers in " ++
      show bCase ++ "]"

data BenchmarkResults = BenchmarkResults {
          setting :: BenchmarkSetting,
          results :: [(String, BenchmarkResult)]
}

instance Show BenchmarkResults where
  show (BenchmarkResults bs rs) =
    show bs ++ "\n" ++
    unlines (map show' $ zip [0..] rs)
   where
     bestNum = best $ map snd rs
     show' (i, (name, r)) = name ++ ":\t" ++ show r ++ postfix i
     postfix i = if i == bestNum then " <---" else ""


data PQBox = forall q. PriorityQueue q => PQB (String, STM (q Int ()))

impls :: [PQBox]
impls =
  [ PQB ("List",  new :: STM (ListPriorityQueue  Int ()))
  , PQB ("TList", new :: STM (TListPriorityQueue Int ()))
  , PQB ("Heap",  new :: STM (HeapPriorityQueue  Int ()))
  , PQB ("THeap", new :: STM (THeapPriorityQueue Int ()))
  ]


fill :: PriorityQueue q => IO Int -> q Int () -> Int -> IO ()
fill randomInt q n = do
  forM_ [1..n] $ \_ -> do
    k <- randomInt
    atomically $ insert q k ()


singleOp :: PriorityQueue q => IO Int -> IO Int -> Int -> q Int () -> IO ()
singleOp rndKey rndPer insRate q = do
  percent <- rndPer
  if (percent < insRate)
  then do
    k <- rndKey
    atomically $ insert q k ()
  else do
    atomically $ deleteMin q


timing :: Int -> Int -> Int -> Int -> IO b -> IO Int
timing opCount timeout numCap numWork qop = do

  let perWorker = opCount `div` numWork
      fstWorker = perWorker + (opCount `mod` numWork)
      work' n = forM_ [1..n] $ \_ -> qop
      work 1 = work' fstWorker
      work _ = work' perWorker

  (ws, vs) <- fmap unzip . forM [1..numWork] $ \i -> do
    v <- newEmptyMVar
    return (work i >> putMVar v (), v)

  startTime <- getTime
  forM_ (zip [1..] ws) $ \(i, w) ->
    forkOn (i `mod` numCap) w
  mapM_ takeMVar vs
  stopTime <- getTime

  let dt = stopTime - startTime
  return $! round $ dt * 1000


throughput :: Int -> Int -> Int -> IO a -> IO Int
throughput timeout numCap numWork qop = do

  c <- newIORef 0
  (ts, vs) <- fmap unzip . forM [1..numWork] $ \i -> do
    v <- newEmptyMVar
    let work = forever $ qop >> (modifyIORef' c (+1))
    t <- forkOn (i `mod` numCap) $ finally work (putMVar v ())
    return (t, v)

  threadDelay $ timeout * 1000
  mapM_ killThread ts
  mapM_ takeMVar vs

  readIORef c


benchmark :: BenchmarkSetting -> IO BenchmarkResults
benchmark bs@(BenchmarkSetting numCap initSize insRate numWork bCase) = do
  g <- createSystemRandom
  let randomInt = uniform g :: IO Int
      randomPercent = (`mod` 101) `fmap` randomInt
      op :: PriorityQueue q => q Int () -> IO ()
      op = singleOp randomInt randomPercent insRate
      bench' bencher wrapper =
        forM impls $ \(PQB (implName, create)) -> do
          q <- atomically $ create
          fill randomInt q initSize
          r <- bencher numCap numWork $ op q
          return (implName, wrapper r)
      bench (Throughput timeout) = bench' (throughput timeout) ThroughputResult
      bench (Timing opCount)     = bench' (timing opCount) TimingResult

  BenchmarkResults bs `fmap`  bench bCase


main :: IO ()
main = do
  let bss =
        [ BenchmarkSetting 1 0 100 4 $ Throughput 500
        , BenchmarkSetting 2 0 100 8 $ Throughput 500
        , BenchmarkSetting 4 0 100 20 $ Throughput 500
        , BenchmarkSetting 1 0 100 4 $ Timing 5000
        , BenchmarkSetting 2 0 100 8 $ Timing 5000
        , BenchmarkSetting 4 0 100 20 $ Timing 5000
        , BenchmarkSetting 1 3000 50 4 $ Throughput 500
        , BenchmarkSetting 2 3000 50 8 $ Throughput 500
        , BenchmarkSetting 4 3000 50 20 $ Throughput 500
        , BenchmarkSetting 1 2000 50 4 $ Timing 5000
        , BenchmarkSetting 2 2000 50 8 $ Timing 5000
        , BenchmarkSetting 4 2000 50 20 $ Timing 5000
        ]
  forM_ bss $ \bs -> do
    rs <- benchmark bs
    print rs


foreign import ccall unsafe "gettime" getTime :: IO Double









