
import Test.Hspec
import Test.QuickCheck
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.List(sort, delete)

import PriorityQueue.PriorityQueue
-- import PriorityQueue.Internals.ListPQ
-- import PriorityQueue.Internals.HeapPQ
-- import PriorityQueue.Internals.TListPQ
-- import PriorityQueue.Internals.THeapPQ
-- import PriorityQueue.Internals.TArraySkipListPQ
-- import PriorityQueue.Internals.LinkedSkipListPQ
-- import PriorityQueue.Internals.TArrayPCGSkipListPQ
-- import PriorityQueue.Internals.LinkedPCGSkipListPQ
-- import PriorityQueue.Internals.TArrayPCGperThreadSLPQ
-- import PriorityQueue.Internals.LinkedPCGperThreadSLPQ
import PriorityQueue.Internals.TArrayPCGSeedPerThreadSLPQ

{-   Debug setting   -}

modeDebug :: Bool
modeDebug = False

printDebug :: String -> IO ()
printDebug msg = when modeDebug $ putStrLn msg

infixl 3 <%
(<%) :: a -> (a -> b) -> b
a <% f = f a

{-   Structural checkers and properties   -}

addManyRemOne :: PriorityQueue q => STM (q Int Int) -> [Int] -> Int -> IO ()
addManyRemOne _ [] _ = return ()
addManyRemOne cons vals ans = do
  pq <- atomically cons
  forM_ vals $ \x -> atomically $ insert pq x x
  x <- atomically $ deleteMin pq
  x `shouldBe` (ans :: Int)


addManyRemOneProp :: PriorityQueue q => STM (q Int Int) -> [Int] -> IO ()
addManyRemOneProp _ []    = return ()
addManyRemOneProp cons vs = addManyRemOne cons vs $ minimum vs


addManyRemAll :: PriorityQueue q => STM (q Int Int) -> [Int] -> IO ()
addManyRemAll cons vals = do
  pq <- atomically cons
  forM_ vals $ \x -> atomically $ insert pq x x
  let vals' = sort vals
  forM_ vals' $ \ans -> do
    y <- atomically $ peekMin pq
    y `shouldBe` (ans :: Int)
    x <- atomically $ deleteMin pq
    x `shouldBe` (ans :: Int)


addRemEach :: PriorityQueue q => STM (q Int Int) -> [Int] -> IO ()
addRemEach cons vals = do
  pq <- atomically cons
  forM_ vals $ \v -> do
    atomically $ insert pq v v
    y <- atomically $ peekMin pq
    y `shouldBe` (v :: Int)
    x <- atomically $ deleteMin pq
    x `shouldBe` (v :: Int)


{-   Producer/Consumer checkers and properties   -}

forkNJoin :: [IO a] -> IO ()
forkNJoin ios = do
  children <- forM ios $ \io -> do
    child <- newEmptyMVar
    forkFinally io $ \_ -> putMVar child ()
    return child
  forM_ children $ \child -> takeMVar child


prodNCons :: (Ord a, Show a, PriorityQueue q) =>
    Int -> Int -> q a a -> [a] -> IO ()
prodNCons pcount ccount pq vals = do
  prodVals <- newMVar vals
  consVals <- newMVar vals
  let prods = replicate pcount $ prodRole prodVals
  let conss = replicate ccount $ consRole prodVals consVals
  forkNJoin $ prods ++ conss
  where
      msg mark name m = name ++ "\t" ++ mark ++ "\t" ++ m
      pmsg = msg "---->"
      cmsg = msg "<<<<<"

      prodRole prodVals = do
        tid <- myThreadId
        let dmsg = printDebug . pmsg (show tid)
        "is waiting on values" <% dmsg
        vs <- takeMVar prodVals
        "got values" <% dmsg
        case vs of
          [] -> do
            putMVar prodVals []
            "DONE" <% dmsg
          (v:vs') -> do
            "puts back rest values" <% dmsg
            putMVar prodVals vs'
            atomically $ insert pq v v
            "value = " ++ show v <% dmsg
            "continues producing" <% dmsg
            prodRole prodVals

      consRole prodVals consVals = do
        tid <- myThreadId
        let dmsg = printDebug . cmsg (show tid)
        "is about to consume value" <% dmsg
        mx <- atomically $ tryDeleteMin pq
        case mx of
          Nothing -> do
            vs <- takeMVar prodVals
            putMVar prodVals vs
            "got values" <% dmsg
            case vs of
              [] -> do
                "is starving..." <% dmsg
                return ()
              _  -> consRole prodVals consVals
          (Just x) -> do
            "is waiting on values" <% dmsg
            vs <- takeMVar consVals
            x `shouldSatisfy` (`elem` vs)
            "value = " ++ show x <% dmsg
            case delete x vs of
              [] -> do
                putMVar consVals []
                "DONE" <% dmsg
              vs' -> do
                putMVar consVals vs'
                "continues consuming" <% dmsg
                consRole prodVals consVals


prodNconsK :: (Ord a, Show a, PriorityQueue q) => STM (q a a) -> Int -> Int -> [a] -> IO ()
prodNconsK pqcons n k vals = do
  let description = "Producer/Consumer test with " ++
                    show n ++ "/" ++ show k ++ " capacities" ++
                    "and " ++ show (length vals) ++ " items"
  ">>> Start " ++ description <% printDebug
  pq <- atomically pqcons
  prodNCons n k pq vals
  "<<< Finish " ++ description <% printDebug


prodNconsKprop :: (Ord a, Show a, PriorityQueue q) => STM (q a a) -> Int -> Int -> [a] -> IO ()
prodNconsKprop pqcons n k vals =
    when (n > 0 && k > 0) $ prodNconsK pqcons n k vals


{-   Per implementation test runner   -}

testImpl :: PriorityQueue q => String -> STM (q Int Int) -> IO ()
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
      property $ addManyRemAll cons

    it "property of one-item queue having this item as minimum" $ do
      property $ addRemEach cons

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
  testImpl "unboxed seed" (new :: STM (TArrayPCGSeedPerThreadSLPQ Int Int))
