
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ExistentialQuantification #-}


import Control.Monad
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.STM
import System.Random.PCG.Fast (createSystemRandom, uniform)
import System.Timeout
import Options.Applicative
import Data.IORef
import Data.Maybe (catMaybes)

import PriorityQueue
import ListPriorityQueue
import TListPriorityQueue
import HeapPriorityQueue
import THeapPriorityQueue
import TArraySkipListPQ
import LinkedSkipListPQ
import TArrayPCGSkipListPQ
import LinkedPCGSkipListPQ
import TArrayPCGperThreadSLPQ
import LinkedPCGperThreadSLPQ

{-   Utils   -}

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

{-   Data    -}

data BenchmarkCase = Throughput Int -- run period in ms
                   | Timing Int Int -- amount of operations and abortion timeout

parseTiming :: Parser BenchmarkCase
parseTiming = Timing
  <$> (argument auto) (metavar "N")
  <*> (argument auto) (metavar "TIMELIMIT")

parseThroughput :: Parser BenchmarkCase
parseThroughput = Throughput <$> (argument auto) (metavar "TIMEOUT")

parseBenchmarkCase :: Parser BenchmarkCase
parseBenchmarkCase = subparser $
  command "timing"
    (parseTiming
     `withInfo` "Measuring time needed to complete a number of operations"
    ) <>
  command "throughput"
    (parseThroughput
     `withInfo` "Measuring a number of complete operations before timeout"
    )

instance Show BenchmarkCase where
  show (Throughput t) = "throughput(" ++ show t ++ "ms)"
  show (Timing c tl) = "timing("++ show c ++ "ops with " ++ show tl ++ "ms time limit)"

data BenchmarkResult = ThroughputResult Int Int
                     | TimingResult Int Int
                     | Aborted String

instance Show BenchmarkResult where
  show (ThroughputResult c d) = "throughput(\t" ++ show c ++ "+-" ++ show d ++ " ops)"
  show (TimingResult t d) = "timing(\t" ++ show t ++ "+-" ++ show d ++ "ms)"
  show (Aborted s) = "aborted(" ++ s ++ ")"

-- does not check consistency of benchmark cases
best :: [BenchmarkResult] -> Maybe Int
best brs = foldl fld Nothing brs where
  fld acc (Aborted _) = acc
  fld Nothing (TimingResult t _) = Just t
  fld acc@(Just r) (TimingResult t d) = if t < r then Just t else acc
  fld Nothing (ThroughputResult n _) = Just n
  fld acc@(Just r) (ThroughputResult n _) = if n > r then Just n else acc

data BenchmarkSetting = BenchmarkSetting {
          numCaps :: Int,
          numWorkers :: Int,
          initialSize :: Int,
          insertionRate :: Int,
          numRuns :: Int,
          benchmarkCase :: BenchmarkCase
}

benchmarkSetting :: Int -> Int -> Parser BenchmarkSetting
benchmarkSetting numCap numWork = BenchmarkSetting numCap numWork
  <$> (option auto)
    (value 1000 <> long "initial-size" <> short 's' <> help "Initial size")
  <*> (option auto)
    (value 50 <> long "insersion-rate" <> short 'r' <> help "Percentage of insertions during one run")
  <*> (option auto)
    (value 3 <> long "runs" <> short 'n' <> help "Number of runs for each implementation")
  <*> parseBenchmarkCase

instance Show BenchmarkSetting where
  show (BenchmarkSetting numCap numWork initSize insRate numRuns bCase) =
    "Benchmark[" ++
      show numCap ++ " cores, initially " ++
      show initSize ++ " items, " ++
      show insRate ++ "% insertions, " ++
      show numWork ++ " workers, " ++
      show numRuns ++ " repeats, " ++
      show bCase ++ "]"

data BenchmarkResults = BenchmarkResults {
          setting :: BenchmarkSetting,
          results :: [(String, BenchmarkResult)]
}

instance Show BenchmarkResults where
  show (BenchmarkResults bs rs) =
    show bs ++ "\n" ++
    unlines (map show' rs)
   where
     bestRes = best $ map snd rs
     show' (name, r) = name ++ ":\t" ++ show r ++ postfix r
     postfix (TimingResult r _) = bestMark r
     postfix (ThroughputResult r _) = bestMark r
     postfix (Aborted r) = ""
     bestMark r = if (Just r) == bestRes then " <---" else ""


data PQBox = forall q. PriorityQueue q => PQB (String, STM (q Int ()))

impls :: [PQBox]
impls =
  [ PQB ("coarse-list-pq",  new :: STM (ListPriorityQueue  Int ()))
  , PQB ("fine-list-pq", new :: STM (TListPriorityQueue Int ()))
  , PQB ("coarse-heap-pq",  new :: STM (HeapPriorityQueue  Int ()))
  , PQB ("fine-heap-pq", new :: STM (THeapPriorityQueue Int ()))
  , PQB ("tarray-skiplist-pq", new :: STM (TArraySkipListPQ Int ()))
  , PQB ("linkedlist-skiplist-pq", new :: STM (LinkedSkipListPQ Int ()))
  , PQB ("tarray-pcg-skiplist-pq", new :: STM (TArrayPCGSkipListPQ Int ()))
  , PQB ("linkedlist-pcg-skiplist-pq", new :: STM (LinkedPCGSkipListPQ Int ()))
  , PQB ("tarray-pcg-perthread-skiplist-pq", new :: STM (TArrayPCGperThreadSLPQ Int ()))
  , PQB ("linkedlist-pcg-perthread-skiplist-pq", new :: STM (LinkedPCGperThreadSLPQ Int ()))
  ]

{-   Benchmark internals   -}

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


timing :: Int -> Int -> Int -> IO b -> IO Int
timing opCount numCap numWork qop = do

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

  cs <- replicateM numWork $ newIORef 0
  (ts, vs) <- fmap unzip . forM (zip cs [1..]) $ \(c, i) -> do
    v <- newEmptyMVar
    let work = forever $ qop >> (modifyIORef' c (+1))
    t <- forkOn (i `mod` numCap) $ finally work (putMVar v ())
    return (t, v)

  threadDelay $ timeout * 1000
  mapM_ killThread ts
  mapM_ takeMVar vs

  sum `fmap` mapM readIORef cs


benchmark :: BenchmarkSetting -> IO BenchmarkResults
benchmark bs@(BenchmarkSetting numCap numWork initSize insRate numRuns bCase) = do
  g <- createSystemRandom
  let randomInt = uniform g :: IO Int
      randomPercent = (`mod` 101) `fmap` randomInt
      op :: PriorityQueue q => q Int () -> IO ()
      op = singleOp randomInt randomPercent insRate

      oneBench qcons bencher = do
          q <- atomically $ qcons
          fill randomInt q initSize
          bencher numCap numWork $ op q

      res2disp rs = (mn + d, d) where
          (mn, mx) = (minimum rs, maximum rs)
          d = (mx - mn) `div` 2
          r = mn + d

      bench (Throughput timeout) =
        forM impls $ \(PQB (implName, qcons)) -> do
          let bench' = oneBench qcons $ throughput timeout
          (r, d) <- res2disp `fmap` replicateM numRuns bench'
          return (implName, ThroughputResult r d)

      bench (Timing opCount timelimit) = do
        forM impls $ \(PQB (implName, qcons)) -> do
          let bench' = timeout (timelimit * 1000) $ oneBench qcons $ timing opCount
          rs <- catMaybes `fmap` replicateM numRuns bench'
          case rs of
            [] -> return $ (,) implName $ Aborted $ ">" ++ show timelimit ++ "ms"
            _ -> return (implName, uncurry TimingResult $ res2disp rs)

  BenchmarkResults bs `fmap` bench bCase


main :: IO ()
main = do
  numCap <- getNumCapabilities
  let p = benchmarkSetting numCap numCap `withInfo` "Concurrent Data Structures Benchmark"
  opts <- execParser p
  rs <- benchmark opts
  print rs


foreign import ccall unsafe "gettime" getTime :: IO Double









