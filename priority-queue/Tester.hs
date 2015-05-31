{-# LANGUAGE FlexibleContexts #-}

module PriorityQueueTester where

import Test.Hspec
import Test.QuickCheck
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import PriorityQueue
import ListPriorityQueue
import TListPriorityQueue
import Data.List(sort)

{-   Debug setting   -}

modeDebug :: Bool
modeDebug = True

deepDebug :: Bool
deepDebug = True

printDebug :: String -> IO ()
printDebug msg = if modeDebug || deepDebug then putStrLn msg else return ()

printDeepDebug :: String -> IO ()
printDeepDebug msg = if deepDebug then putStrLn msg else return ()

{-   Helpers   -}

at :: STM a -> IO a
at = atomically

{-   Structural checkers and properties   -}

addManyRemOne :: PriorityQueue q Int Int => STM (q Int Int) -> [Int] -> Int -> IO ()
addManyRemOne _ [] _ = return ()
addManyRemOne cons vals ans = do
  pq <- at $ cons
  forM_ vals $ \x -> at $ insert pq x x
  x <- at $ deleteMin pq
  x `shouldBe` (ans :: Int)


addManyRemOneProp :: PriorityQueue q Int Int => STM (q Int Int) -> [Int] -> IO ()
addManyRemOneProp _ []    = return ()
addManyRemOneProp cons vs = addManyRemOne cons vs $ head $ sort vs


addManyRemAll :: PriorityQueue q Int Int => STM (q Int Int) -> [Int] -> IO ()
addManyRemAll cons vals = do
  pq <- at $ cons
  forM_ vals $ \x -> at $ insert pq x x
  let vals' = sort vals
  forM_ vals' $ \ans -> do
    y <- at $ peekMin pq
    y `shouldBe` (ans :: Int)
    x <- at $ deleteMin pq
    x `shouldBe` (ans :: Int)


addManyRemAllProp :: PriorityQueue q Int Int => STM (q Int Int) -> [Int] -> IO ()
addManyRemAllProp = addManyRemAll


addRemEach :: PriorityQueue q Int Int => STM (q Int Int) -> [Int] -> IO ()
addRemEach cons vals = do
  pq <- at $ cons
  forM_ vals $ \v -> do
    at $ insert pq v v
    y <- at $ peekMin pq
    y `shouldBe` (v :: Int)
    x <- at $ deleteMin pq
    x `shouldBe` (v :: Int)


addRemEachProp :: PriorityQueue q Int Int => STM (q Int Int) -> [Int] -> IO ()
addRemEachProp = addRemEach

{-   Producer/Consumer checkers and properties   -}

fork'n'join :: [IO a] -> IO ()
fork'n'join ios = do
  children <- newMVar []
  forM_ ios $ \io -> do
    child <- newEmptyMVar
    modifyMVar_ children $ return . (child:)
    forkFinally io $ \_ -> putMVar child ()
  children' <- readMVar children
  forM_ children' $ \child -> do
    takeMVar child


prod'n'cons :: (Show k, PriorityQueue q k k) => Int -> Int -> q k k -> [k] -> IO ()
prod'n'cons pcount ccount pq vals = do
  prodVals <- newMVar vals
  consVals <- newMVar vals
  let prods = replicate pcount $ prodRole prodVals
  let conss = replicate ccount $ consRole prodVals consVals
  fork'n'join $ prods ++ conss
  where
      msg mark name m = name ++ "\t" ++ mark ++ "\t" ++ m
      pmsg = msg "---->"
      cmsg = msg "<<<<<"

      prodRole prodVals = do
        tid <- myThreadId
        let nmsg = pmsg $ show tid
        printDeepDebug $ nmsg "is waiting on values"
        vs <- takeMVar prodVals
        printDeepDebug $ nmsg "got values"
        case vs of
          [] -> do
            putMVar prodVals []
            printDebug $ nmsg "DONE"
          (v:vs') -> do
            printDeepDebug $ nmsg "puts back rest values"
            putMVar prodVals vs'
            at $ insert pq v v
            printDebug $ nmsg $ show v
            printDeepDebug $ nmsg "continues producing"
            prodRole prodVals

      consRole prodVals consVals = do
        tid <- myThreadId
        let nmsg = cmsg $ show tid
        printDeepDebug $ nmsg "is about to consume value"
        mx <- at $ tryDeleteMin pq
        case mx of
          Nothing -> do
            vs <- takeMVar prodVals
            putMVar prodVals vs
            printDeepDebug $ nmsg "got values"
            case vs of
              [] -> do
                printDebug $ nmsg "is starving..."
                return ()
              _  -> do
                consRole prodVals consVals
          (Just x) -> do
            printDeepDebug $ nmsg "is waiting on values"
            vs <- takeMVar consVals
            x `shouldSatisfy` (`elem` vs)
            printDebug $ nmsg $ show x
            case drop1 x vs of
              [] -> do
                putMVar consVals []
                printDebug $ nmsg "DONE"
              vs' -> do
                putMVar consVals vs'
                printDeepDebug $ nmsg "continues consuming"
                consRole prodVals consVals
        where
            drop1 _ []                   = []
            drop1 x (x':xs') | x' == x   = xs'
                             | otherwise = x' : drop1 x xs'


prodNconsK :: (Show k, PriorityQueue q k k) => STM (q k k) -> Int -> Int -> [k] -> IO ()
prodNconsK pqcons n k vals = do
  let description = "Producer/Consumer test with " ++
                    show n ++ "/" ++ show k ++ " capacities" ++
                    "and " ++ show (length vals) ++ " items"
  printDebug $ ">>> Start " ++ description
  pq <- at $ pqcons
  prod'n'cons n k pq vals
  printDebug $ "<<< Finish " ++ description


prodNconsKprop :: (Show k, PriorityQueue q k k) => STM (q k k) -> Int -> Int -> [k] -> IO ()
prodNconsKprop pqcons n k vals =
  if n > 0 && k > 0
  then prodNconsK pqcons n k vals
  else return ()


{-   Per implementation test runner   -}

testImpl :: PriorityQueue q Int Int => String -> STM (q Int Int) -> IO ()
testImpl base cons = hspec $ do

  describe (base ++ " implementation") $ do

    it "insert one item and delete it" $ do
      addManyRemOne cons [3] 3

    it "insert several items and minimum is first" $ do
      addManyRemOne cons [1..5] 1

    it "insert several items and minimum is second" $ do
      addManyRemOne cons (6:[2..5]) 2

    it "insert several items and minimum is last" $ do
      addManyRemOne cons [9,8..5] 5

    it "insert sequentially encreasing keys and delete all" $ do
      addManyRemAll cons [10..20]

    it "insert sequentially decreasing keys and delete all" $ do
      addManyRemAll cons [20,19..10]

    it "insert and delete several items" $ do
      addRemEach cons [8,2,7,1,9,0,3,5,4,6]

    it "property of keeing first minimum" $ do
      property $ addManyRemOneProp cons

    it "property of deleting items in sorted order" $ do
      property $ addManyRemAllProp cons

    it "property of one-item queue having this item as minimum" $ do
      property $ addRemEachProp cons

    it "run 1 producer and 1 consumer on several items" $ do
      prodNconsK cons 1 1 [1..5]

    it "run 2 producers and 1 consumer on several items" $ do
      prodNconsK cons 2 1 [1..5]

    it "run 1 producer and 2 consumers on several items" $ do
      prodNconsK cons 1 2 [1..5]

    it "run 5 producers and 2 consumers on several items" $ do
      prodNconsK cons 5 2 [25,24..1]

    it "run 2 producers and 5 consumers on several items" $ do
      prodNconsK cons 2 5 [1..25]

    it "property of consumers to consume all produced items" $ do
      property $ prodNconsKprop cons


main :: IO ()
main = do
  printDebug "\n{--------------------  NEW TEST SET  --------------------}\n"
  testImpl "Coarse-grained List" (new :: STM (ListPriorityQueue  Int Int))
  testImpl "Fine-grained List"   (new :: STM (TListPriorityQueue Int Int))





