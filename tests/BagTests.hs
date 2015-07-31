
import Test.Hspec
import Test.QuickCheck
import Control.Concurrent.STM
import Control.Monad

import Data.STM.Bag.Class as Bag
import Data.STM.Bag.Internal.ListBag
import Data.STM.Bag.Internal.TListBag

addTakeOne :: Bag.Bag b => STM (b Int) -> [Int] -> IO ()
addTakeOne _ [] = return ()
addTakeOne bagCons vs =
    forM_ vs $ \v -> do
        bag <- atomically $ bagCons
        atomically $ Bag.add bag v
        v' <- atomically $ Bag.take bag
        v' `shouldBe` v

addTakeOneMany :: Bag.Bag b => STM (b Int) -> [Int] -> IO ()
addTakeOneMany _ [] = return ()
addTakeOneMany bagCons vs = do
    bag <- atomically $ bagCons
    forM_ vs $ \v -> do
        atomically $ Bag.add bag v
        v' <- atomically $ Bag.take bag
        v' `shouldBe` v

addManyTakeOne :: Bag.Bag b => STM (b Int) -> [Int] -> IO ()
addManyTakeOne _ [] = return ()
addManyTakeOne bagCons vs = do
    bag <- atomically $ bagCons
    forM_ vs $ atomically . Bag.add bag
    v' <- atomically $ Bag.take bag
    v' `shouldSatisfy` (`elem` vs)

addManyTakeAll :: Bag.Bag b => STM (b Int) -> [Int] -> IO ()
addManyTakeAll _ [] = return ()
addManyTakeAll bagCons vs = do
    bag <- atomically $ bagCons
    forM_ vs $ atomically . Bag.add bag
    forM_ vs $ \_ -> do
        v' <- atomically $ Bag.take bag
        v' `shouldSatisfy` (`elem` vs)


test :: Bag.Bag b => String -> STM (b Int) -> IO ()
test name bagCons = hspec $
  describe (name ++ " bag implementation") $ do
    it "add the only item and take it" $
        addTakeOne bagCons [1..25]
    it "add one item and take it and again (inc)" $
        addTakeOneMany bagCons [1..25]
    it "add one item and take it and again (dec)" $
        addTakeOneMany bagCons [25,24..1]
    it "add many items and take only one (inc)" $
        addManyTakeOne bagCons [1..25]
    it "add many items and take only one (dec)" $
        addManyTakeOne bagCons [25,24..1]
    it "add many items and take all (inc)" $
        addManyTakeAll bagCons [1..25]
    it "add many items and take all (dec)" $
        addManyTakeAll bagCons [25,24..1]

    it "property: add the only item and take it" $
        property $ addTakeOne bagCons
    it "property: add one item and take it and again" $
        property $ addTakeOneMany bagCons
    it "property: add many items and take only one" $
        property $ addManyTakeOne bagCons
    it "property: add many items and take all (inc)" $
        property $ addManyTakeAll bagCons


main :: IO ()
main = do
    test "list" (new :: STM (ListBag Int))
    test "tlist" (new :: STM (TListBag Int))
