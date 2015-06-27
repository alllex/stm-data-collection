
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

import PriorityQueue
import ListPriorityQueue
import TListPriorityQueue
import HeapPriorityQueue
import THeapPriorityQueue
import TSkipListPQ

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

data BenchmarkResult = ThroughputResult Int
                     | TimingResult Int
                     | Aborted String

instance Show BenchmarkResult where
  show (ThroughputResult c) = "throughput(\t" ++ show c ++ "ops)"
  show (TimingResult t) = "timing(\t" ++ show t ++ "ms)"
  show (Aborted s) = "aborted(" ++ s ++ ")"

-- does not check consistency of benchmark cases
best :: [BenchmarkResult] -> Maybe Int
best brs = foldl fld Nothing brs where
  fld acc (Aborted _) = acc
  fld Nothing (TimingResult t) = Just t
  fld acc@(Just r) (TimingResult t) = if t < r then Just t else acc
  fld Nothing (ThroughputResult n) = Just n
  fld acc@(Just r) (ThroughputResult n) = if n > r then Just n else acc

data BenchmarkSetting = BenchmarkSetting {
          numCaps :: Int,
          numWorkers :: Int,
          initialSize :: Int,
          insertionRate :: Int,
          benchmarkCase :: BenchmarkCase
}

benchmarkSetting :: Int -> Int -> Parser BenchmarkSetting
benchmarkSetting numCap numWork = BenchmarkSetting numCap numWork
  <$> (option auto)
    (value 1000 <> long "initial-size" <> short 's' <> help "Initial size")
  <*> (option auto)
    (value 50 <> long "insersion-rate" <> short 'r' <> help "Percentage of insertions during one run")
  <*> parseBenchmarkCase

instance Show BenchmarkSetting where
  show (BenchmarkSetting numCap numWork initSize insRate bCase) =
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
    unlines (map show' rs)
   where
     bestRes = best $ map snd rs
     show' (name, r) = name ++ ":\t" ++ show r ++ postfix r
     postfix (TimingResult r) = bestMark r
     postfix (ThroughputResult r) = bestMark r
     postfix (Aborted r) = ""
     bestMark r = if (Just r) == bestRes then " <---" else ""


data PQBox = forall q. PriorityQueue q => PQB (String, STM (q Int ()))

impls :: [PQBox]
impls =
  [ PQB ("List",  new :: STM (ListPriorityQueue  Int ()))
  , PQB ("TList", new :: STM (TListPriorityQueue Int ()))
  , PQB ("Heap",  new :: STM (HeapPriorityQueue  Int ()))
  , PQB ("THeap", new :: STM (THeapPriorityQueue Int ()))
  , PQB ("TSkipList", new :: STM (TSkipListPQ Int ()))
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
benchmark bs@(BenchmarkSetting numCap numWork initSize insRate bCase) = do
  g <- createSystemRandom
  let randomInt = uniform g :: IO Int
      randomPercent = (`mod` 101) `fmap` randomInt
      op :: PriorityQueue q => q Int () -> IO ()
      op = singleOp randomInt randomPercent insRate
      bench (Throughput timeout) =
        forM impls $ \(PQB (implName, create)) -> do
          q <- atomically $ create
          fill randomInt q initSize
          r <- throughput timeout numCap numWork $ op q
          return (implName, ThroughputResult r)
      bench (Timing opCount timelimit) = do
        forM impls $ \(PQB (implName, create)) -> do
          res <- timeout (timelimit * 1000) $ do
            q <- atomically $ create
            fill randomInt q initSize
            r <- timing opCount numCap numWork $ op q
            return (implName, TimingResult r)
          case res of
            Nothing -> return $ (,) implName $ Aborted $ ">" ++ show timelimit ++ "ms"
            Just r -> return r

  BenchmarkResults bs `fmap`  bench bCase


main :: IO ()
main = do
  numCap <- getNumCapabilities
  let p = benchmarkSetting numCap numCap `withInfo` "Concurrent Data Structures Benchmark"
  opts <- execParser p
  rs <- benchmark opts
  print rs


foreign import ccall unsafe "gettime" getTime :: IO Double









