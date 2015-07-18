{-# LANGUAGE ExistentialQuantification #-}

import Control.Concurrent.STM
import Control.Monad

import Benchmark

import PriorityQueue
import Internal.ListPQ
import Internal.TListPQ
import Internal.HeapPQ
import Internal.THeapPQ
import Internal.TArraySkipListPQ
import Internal.LinkedSkipListPQ
import Internal.TArrayPCGSkipListPQ
import Internal.LinkedPCGSkipListPQ
import Internal.TArrayPCGperThreadSLPQ
import Internal.LinkedPCGperThreadSLPQ


data PQBox = forall q. PriorityQueue q => PQB (STM (q Int ()))

impls :: [(String, PQBox)]
impls =
  [ ("coarse-list-pq",  PQB (new :: STM (ListPQ Int ())))
  , ("fine-list-pq", PQB (new :: STM (TListPQ Int ())))
  , ("coarse-heap-pq",  PQB (new :: STM (HeapPQ Int ())))
  , ("fine-heap-pq", PQB (new :: STM (THeapPQ Int ())))
  , ("tarray-skiplist-pq", PQB (new :: STM (TArraySkipListPQ Int ())))
  , ("linkedlist-skiplist-pq", PQB (new :: STM (LinkedSkipListPQ Int ())))
  , ("tarray-pcg-skiplist-pq", PQB (new :: STM (TArrayPCGSkipListPQ Int ())))
  , ("linkedlist-pcg-skiplist-pq", PQB (new :: STM (LinkedPCGSkipListPQ Int ())))
  , ("tarray-pcg-perthread-skiplist-pq", PQB (new :: STM (TArrayPCGperThreadSLPQ Int ())))
  , ("linkedlist-pcg-perthread-skiplist-pq", PQB (new :: STM (LinkedPCGperThreadSLPQ Int ())))
  ]

implNames :: [String]
implNames = map fst impls

findImpl :: String -> Maybe PQBox
findImpl name = find impls
    where find [] = Nothing
          find ((name', impl):is) =
            if name == name' then Just impl
            else find is

benchOne' (name, PQB qcons) = do
    let cons = atomically qcons
        insOp q key = atomically $ insert q key ()
        delOp q = atomically $ deleteMin q
        struct = BenchStruct name cons insOp delOp
        defProc = BenchProc 1000 50 3
    report <- execBenchmark struct defProc
    print report


benchOne name =
    case findImpl name of
        Nothing -> error "Unknown implementation"
        Just pqb -> benchOne' (name, pqb)

main :: IO ()
-- main = benchOne "linkedlist-pcg-perthread-skiplist-pq"
-- main = mapM_ benchOne
main = do
    benchOne "tarray-pcg-perthread-skiplist-pq"
    benchOne "linkedlist-pcg-perthread-skiplist-pq"
    benchOne "tarray-pcg-skiplist-pq"
    benchOne "linkedlist-pcg-skiplist-pq"
    benchOne "fine-heap-pq"
    benchOne "coarse-heap-pq"
