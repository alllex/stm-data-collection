{-# LANGUAGE ExistentialQuantification #-}

import Control.Concurrent.STM

import Benchmark

import PriorityQueue.PriorityQueue
import PriorityQueue.Internals.ListPQ
import PriorityQueue.Internals.TListPQ
import PriorityQueue.Internals.HeapPQ
import PriorityQueue.Internals.THeapPQ
import PriorityQueue.Internals.TArraySkipListPQ
import PriorityQueue.Internals.LinkedSkipListPQ
import PriorityQueue.Internals.TArrayPCGSkipListPQ
import PriorityQueue.Internals.LinkedPCGSkipListPQ
import PriorityQueue.Internals.TArrayPCGperThreadSLPQ
import PriorityQueue.Internals.LinkedPCGperThreadSLPQ
import PriorityQueue.Internals.TArrayPCGSeedPerThreadSLPQ


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
  , ("tarray-pcg-seed-perthread-skiplist-pq", PQB (new :: STM (TArrayPCGSeedPerThreadSLPQ Int ())))
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
