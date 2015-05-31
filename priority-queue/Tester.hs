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

at :: STM a -> IO a
at = atomically

{-   Structural checkers and properties -}

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

{-   Producer/Consumer checkers and properties -}

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
      -- msg mark name m = putStrLn $ name ++ "\t" ++ mark ++ "\t" ++ m
      -- pmsg = msg "---->"
      -- cmsg = msg "<<<<<"

      prodRole prodVals = do
        -- tid <- myThreadId
        -- let tname = show tid
        -- putStrLn $ "P(" ++ tname ++ ")\t\twait on values"
        vs <- takeMVar prodVals
        -- putStrLn $ "P(" ++ tname ++ ")\t\tgot values"
        case vs of
          [] -> do
            putMVar prodVals []
            -- pmsg tname "DONE"
            -- putStrLn $ "P(" ++ tname ++ ")\t\tDONE"
          (v:vs') -> do
            -- putStrLn $ "P(" ++ tname ++ ")\t\tput back rest values"
            putMVar prodVals vs'
            -- putStrLn $ "P(" ++ tname ++ ")\t\t >>> " ++ show v
            at $ insert pq v v
            -- pmsg tname $ show v
            -- putStrLn $ "P(" ++ tname ++ ")\t\tcontinue producing"
            prodRole prodVals

      consRole prodVals consVals = do
        -- tid <- myThreadId
        -- let tname = show tid
        -- putStrLn $ "C(" ++ tname ++ ")\t\tconsume value"
        mx <- at $ tryDeleteMin pq
        case mx of
          Nothing -> do
            vs <- takeMVar prodVals
            putMVar prodVals vs
            -- putStrLn $ "P(" ++ tname ++ ")\t\tgot values"
            case vs of
              [] -> do
                -- cmsg tname "starving..."
                return ()
              _  -> do
                consRole prodVals consVals
          (Just x) -> do
            -- putStrLn $ "C(" ++ tname ++ ")\t\twait on values"
            vs <- takeMVar consVals
            -- putStrLn $ "C(" ++ tname ++ ")\t\t <<< " ++ show x
            x `shouldSatisfy` (`elem` vs)
            -- cmsg tname $ show x
            case drop1 x vs of
              [] -> do
                putMVar consVals []
                -- cmsg tname "DONE"
                -- putStrLn $ "C(" ++ tname ++ ")\t\tDONE"
              vs' -> do
                putMVar consVals vs'
                -- putStrLn $ "C(" ++ tname ++ ")\t\tcontinue consuming"
                consRole prodVals consVals
        where
            drop1 _ []                   = []
            drop1 x (x':xs') | x' == x   = xs'
                             | otherwise = x' : drop1 x xs'


prodNconsK :: (Show k, PriorityQueue q k k) => Int -> Int -> STM (q k k) -> [k] -> IO ()
prodNconsK n k pqcons vals = do
  -- putStrLn $ ">>> Start Producer/Consumer test with " ++
  --              show n ++ "/" ++ show k ++
  --              " capacities and values: " ++ show vals
  pq <- at $ pqcons
  prod'n'cons n k pq vals
  -- putStrLn $ "<<< FINISH Producer/Consumer test with " ++
  --              show n ++ "/" ++ show k ++
  --              " capacities and values: " ++ show vals

{-
loudCounter :: MVar Int -> Int -> Int -> IO ()
loudCounter global name n = do
  putStrLn $ show name ++ " :: started"
  forM_ [1..n] $ \k -> do
    modifyMVar_ global $ return . (+1)
    putStrLn $ show name ++ " :: count " ++ show k
  putStrLn $ show name ++ " :: finished"


tmpTest :: IO ()
tmpTest = do
  global <- newMVar 0
  putStrLn "main :: let's fork"
  fork'n'join [ loudCounter global n n | n <- [1..5] ]
  gval <- takeMVar global
  putStrLn $ "Global counter = " ++ show gval
  putStrLn "main :: all joined"
-}


{- Per implementation test runner -}

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
      prodNconsK 1 1 cons [1..5]

    it "run 2 producers and 1 consumer on several items" $ do
      prodNconsK 2 1 cons [1..5]

    it "run 1 producer and 2 consumers on several items" $ do
      prodNconsK 1 2 cons [1..5]

    it "run 5 producers and 2 consumers on several items" $ do
      prodNconsK 5 2 cons [25,24..1]

    it "run 2 producers and 5 consumers on several items" $ do
      prodNconsK 2 5 cons [1..25]

main :: IO ()
main = do
  putStrLn "\n{--------------------  NEW TEST SET  --------------------}\n"
  testImpl "Coarse-grained List" (new :: STM (ListPriorityQueue  Int Int))
  testImpl "Fine-grained List"   (new :: STM (TListPriorityQueue Int Int))





