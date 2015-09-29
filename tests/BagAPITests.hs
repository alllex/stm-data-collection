
import Control.Concurrent.STM
import qualified Data.STM.Bag as Bag

main :: IO ()
main = do
    bag <- atomically $ (Bag.new :: STM (Bag.Impl Int))
    atomically $ Bag.add bag 7
    atomically $ Bag.add bag 5
    x <- atomically $ Bag.take bag
    putStrLn $ show x -- x may be either 5 or 7
