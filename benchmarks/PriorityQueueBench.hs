{-# LANGUAGE ExistentialQuantification #-}

import Control.Concurrent.STM

import Benchmark

import Data.STM.PriorityQueue.Class
import Data.STM.PriorityQueue.Internal.ListPQ
import Data.STM.PriorityQueue.Internal.TListPQ
import Data.STM.PriorityQueue.Internal.HeapPQ
import Data.STM.PriorityQueue.Internal.THeapPQ
import Data.STM.PriorityQueue.Internal.TASLPQ
import Data.STM.PriorityQueue.Internal.LLSLPQ
import Data.STM.PriorityQueue.Internal.PTRTASLPQ
import Data.STM.PriorityQueue.Internal.PTRLLSLPQ
import Data.STM.PriorityQueue.Internal.PTSTASLPQ


data PQBox = forall q. PriorityQueue q => PQB (STM (q Int ()))

impls :: [(String, PQBox)]
impls =
  [ ("coarse-list-pq",  PQB (new :: STM (ListPQ Int ())))
  , ("fine-list-pq", PQB (new :: STM (TListPQ Int ())))
  , ("coarse-heap-pq",  PQB (new :: STM (HeapPQ Int ())))
  , ("fine-heap-pq", PQB (new :: STM (THeapPQ Int ())))
  , ("tarray-pcg-skiplist-pq", PQB (new :: STM (TASLPQ Int ())))
  , ("linkedlist-pcg-skiplist-pq", PQB (new :: STM (LLSLPQ Int ())))
  , ("tarray-pcg-perthread-skiplist-pq", PQB (new :: STM (PTRTASLPQ Int ())))
  , ("linkedlist-pcg-perthread-skiplist-pq", PQB (new :: STM (PTRLLSLPQ Int ())))
  , ("tarray-pcg-seed-perthread-skiplist-pq", PQB (new :: STM (PTSTASLPQ Int ())))
  ]

implNames :: [String]
implNames = map fst impls

findImpl :: String -> Maybe PQBox
findImpl name = find impls
    where find [] = Nothing
          find ((name', impl):is) =
            if name == name' then Just impl
            else find is

benchOne' :: (String, PQBox) -> IO ()
benchOne' (name, PQB qcons) = do
    let cons = atomically qcons
        insOp q key = atomically $ insert q key ()
        delOp q = atomically $ deleteMin q
        struct = BenchStruct name cons insOp delOp
        defProc = BenchProc 1000 50 3 1000
    report <- execBenchmark struct defProc
    let shortRep = makeShortReport report
    print shortRep


benchOne :: String -> IO ()
benchOne name =
    case findImpl name of
        Nothing -> error "Unknown implementation"
        Just pqb -> benchOne' (name, pqb)

main :: IO ()
main = do
    benchOne "coarse-heap-pq"
    benchOne "tarray-pcg-perthread-skiplist-pq"
    benchOne "tarray-pcg-skiplist-pq"
    benchOne "tarray-pcg-seed-perthread-skiplist-pq"
