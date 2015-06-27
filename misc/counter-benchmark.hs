{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Control.Monad
import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent
import Options.Applicative
import Data.Array.MArray
import Data.Array.IO
import Data.Array.Unboxed
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as U
import Data.Primitive.ByteArray

type IOByteArray = MutableByteArray RealWorld

data BenchmarkOptions =
  BenchmarkOptions { runs :: Int
                   , timeout :: Int
                   }

benchmarkOptions :: Parser BenchmarkOptions
benchmarkOptions = BenchmarkOptions
  <$> (option auto)
    (value 3   <> long "runs"    <> short 'r' <> help "Best in number of runs")
  <*> (option auto)
    (value 100 <> long "timeout" <> short 't' <> help "Throughput timeout (ms)")

counterIORef :: IORef Int -> IO ()
counterIORef c = forever $ modifyIORef' c (+1)

countIORef :: Int -> IO Int
countIORef = count (newIORef 0) counterIORef readIORef

counterMVar :: MVar Int -> IO ()
counterMVar c = forever $ modifyMVar_ c $ return . (+1)

countMVar :: Int -> IO Int
countMVar = count (newMVar 0) counterMVar takeMVar

counterTVar :: TVar Int -> IO ()
counterTVar c = forever $ atomically $ modifyTVar c (+1)

countTVar :: Int -> IO Int
countTVar = count (atomically $ newTVar 0) counterTVar $ atomically . readTVar

counterMArray :: (MArray a Int IO) => a Int Int -> IO ()
counterMArray c = forever $ do
  v <- readArray c 0
  writeArray c 0 $ v+1

countMArray :: (MArray a Int IO) => IO (a Int Int) -> Int -> IO Int
countMArray zero = count zero counterMArray $ \c -> readArray c 0

countIOArray :: Int -> IO Int
countIOArray = countMArray (newArray (0, 0) 0 :: IO (IOArray Int Int))

countIOUArray :: Int -> IO Int
countIOUArray = countMArray (newArray (0, 0) 0 :: IO (IOUArray Int Int))

counterMVector :: U.IOVector Int -> IO ()
counterMVector c = forever $ do
  v <- U.unsafeRead c 0
  U.unsafeWrite c 0 $ v+1

countMVector :: Int -> IO Int
countMVector = do
  let zero = do
        vec <- U.new 1
        U.write vec 0 0
        return vec
      result = \c -> U.read c 0
  count zero counterMVector result

counterMByteArr :: IOByteArray -> IO ()
counterMByteArr c = forever $ do
  v <- readByteArray c 0 :: IO Int
  writeByteArray c 0 $ v+1

countMByteArr :: Int -> IO Int
countMByteArr = do
  let zero = do
        arr <- newByteArray (8 :: Int)
        writeByteArray arr 0 (0 :: Int)
        return arr
      result = \c -> readByteArray c 0
  count zero counterMByteArr result

counters :: [(String, Int -> IO Int)]
counters =
  [ ("IORef",    countIORef)
  , ("MVar",     countMVar)
  , ("TVar",     countTVar)
  , ("IOArr",    countIOArray)
  , ("IOUArr",   countIOUArray)
  , ("MVector",  countMVector)
  , ("ByteArr",  countMByteArr)
  ]

count :: (IO a) -> (a -> IO ()) -> (a -> IO Int) -> Int -> IO Int
count zero counter result timeout = do
  c <- zero
  sync <- newEmptyMVar
  tid <- forkFinally (counter c) $ \_ -> putMVar sync ()
  threadDelay timeout
  killThread tid
  takeMVar sync
  result c

benchmark :: BenchmarkOptions -> IO [(Int, String)]
benchmark (BenchmarkOptions runs timeout) = do
  let cs = counters
  rs <- forM cs $ \(name, cnt) -> do
    res <- maximum `fmap` replicate runs `fmap` cnt (1000 * timeout)
    return (res, name)
  return $ sort rs

writeResults :: Show a => FilePath -> [(a, String)] -> IO ()
writeResults filename rs = do
  writeFile filename "counter, result\n"
  forM_ rs $ \(r, name) -> do
    appendFile filename $ name ++ ", " ++ show r ++ "\n"

main :: IO ()
main = do
  let p = info (helper <*> benchmarkOptions)
               (fullDesc <> progDesc "Counters benchmark.")
  opts <- execParser p
  rs <- benchmark opts
  putStrLn $ "Counter benchmark [ best of = " ++
             show (runs opts) ++ ", timeout = " ++
             show (timeout opts) ++ "ms ]"
  forM_ rs $ \(r, name) -> do
    putStrLn $ name ++ "\t" ++ show r
  writeResults "counter-results.csv" rs





