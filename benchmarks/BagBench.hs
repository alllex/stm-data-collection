
{-# LANGUAGE ExistentialQuantification #-}

import Control.Concurrent.STM

import Benchmark

import Data.STM.Bag.Class as Bag
import Data.STM.Bag.Internal.ListBag
import Data.STM.Bag.Internal.TListBag
import Data.STM.Bag.Internal.PTLB
import Data.STM.Bag.Internal.PTTLB

data Box = forall b. Bag.Bag b => Box (STM (b Int))

findImpl :: String -> Maybe Box
findImpl name = find impls
    where find [] = Nothing
          find ((name', impl):is) =
            if name == name' then Just impl
            else find is

benchOne' :: (String, Box) -> IO ()
benchOne' (name, Box bcons) = do
    let cons = atomically bcons
        insOp b v = atomically $ Bag.add b v
        delOp b = do
            _ <- atomically $ Bag.take b
            return ()
        struct = BenchStruct name cons insOp delOp
        defProc = BenchProc 1000 50 3 1000
    report <- execBenchmark struct defProc
    let shortRep = makeShortReport report
    print shortRep

benchOne :: String -> IO ()
benchOne name =
    case findImpl name of
        Nothing -> error $ "Unknown implementation: " ++ name
        Just box -> benchOne' (name, box)

impls :: [(String, Box)]
impls =
  [ ("coarse-list-bag", Box (new :: STM (ListBag Int)))
  , ("fine-list-bag",   Box (new :: STM (TListBag Int)))
  , ("per-thread-list-bag",     Box (new :: STM (PTLB Int)))
  , ("per-thread-tlist-bag",    Box (new :: STM (PTTLB Int)))
  ]

main :: IO ()
main = do
    benchOne "coarse-list-bag"
    benchOne "fine-list-bag"
    benchOne "per-thread-list-bag"
    benchOne "per-thread-tlist-bag"
