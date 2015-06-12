
{-# LANGUAGE ForeignFunctionInterface #-}


import Control.Concurrent



data BenchmarkCase = Throughput Int -- run period in ms
                   | Timing Int     -- amount of operations

data BenchmarkResult = ThroughputResult Double
                     | TimingResult Int

data BenchmarkSetting = BenchmarkSetting {
          numCapabilities :: Int,
          initialSize :: Int,
          insertionRate :: Int,
          workersAmount :: Int,
          benchmarkCase :: BenchmarkCase
}

data BenckmarkResult = BenchmarkResult {
          description :: String,
          setting :: BenchmarkSetting,
          result :: BenchmarkResult
}



main :: IO ()
main = do
  t1 <- getTime
  threadDelay 500
  t2 <- getTime
  let t = round $ (t2-t1) * 10.0^6
  print $ t




foreign import ccall unsafe "gettime" getTime :: IO Double


