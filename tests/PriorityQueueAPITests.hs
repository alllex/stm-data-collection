
import Control.Concurrent.STM
import qualified Data.STM.PriorityQueue as PQ
import Control.Monad ( forM_ )

main :: IO ()
main = do
    pq <- atomically $ (PQ.new :: STM (PQ.Impl Int Int))
    let kvs = [(2, 1), (5, 3), (1, 2), (4, 5)]
    forM_ kvs $ \(k, v) -> atomically $ PQ.insert pq k v
    x <- atomically $ PQ.deleteMin pq
    putStrLn $ "x = " ++ show x -- prints 2
