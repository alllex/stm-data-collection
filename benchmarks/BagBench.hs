
{-# LANGUAGE ExistentialQuantification #-}

import Control.Concurrent.STM

import Benchmark

import Data.STM.Bag.Class as Bag
import Data.STM.Bag.Internal.ListBag
import Data.STM.Bag.Internal.TListBag
import Data.STM.Bag.Internal.PTLB
import Data.STM.Bag.Internal.PTTLB

data Box = forall b. Bag.Bag b => Box (STM (b Int))

impls :: [(String, Box)]
impls =
  [ ("coarse-list-bag", Box (new :: STM (ListBag Int)))
  , ("fine-list-bag",   Box (new :: STM (TListBag Int)))
  , ("per-thread-list-bag",     Box (new :: STM (PTLB Int)))
  , ("per-thread-tlist-bag",    Box (new :: STM (PTTLB Int)))
  ]

findImpl :: String -> Maybe Box
findImpl name = find impls
    where find [] = Nothing
          find ((name', impl):is) =
            if name == name' then Just impl
            else find is

benchOne' :: (String, Box) -> IO ShortBenchReport
benchOne' (name, Box bcons) = do
    let cons = atomically bcons
        insOp b item = atomically $ Bag.add b item
        delOp b = (atomically $ Bag.take b) >> return ()
        struct = BenchStruct name cons insOp delOp
        defProc = BenchProc 1000 50 3 1000 False
    report <- execBenchmark struct defProc
    return $ makeShortReport report

benchOne :: String -> IO ShortBenchReport
benchOne name =
    case findImpl name of
        Nothing -> error "Bag Bench: unknown implementation"
        Just pqb -> benchOne' (name, pqb)

composedBench :: [String] -> IO ComposedReport
composedBench names = fmap makeComposedReport $ sequence $ map benchOne names

main :: IO ()
main = do
    let toBenchNames = map fst impls
    composedRep <- composedBench toBenchNames
    let (toFile, stamp, text) = printedTable "bag-bench" composedRep
        filename = stamp ++ ".log"
    if toFile then
        writeFile filename text
    else
        putStrLn text
