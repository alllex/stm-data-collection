
module BenchData (
    BenchSetting(BenchSetting),
    BenchStruct(BenchStruct),
    BenchEnv(BenchEnv),
    BenchProc(BenchProc),
    BenchCase(ThroughputCase, TimingCase),
    BenchResult(ThroughputRes, TimingRes, AbortedRes),
    BenchReport(BenchReport),
    BenchCaseKind(Thrput, Timing),
    ShortBenchReport(ShortBenchReport),
    ComposedReport(ComposedReport),
    buildBenchSetting,
    makeShortReport,
    makeComposedReport,
    printableTable,
    benchStamp,
    printedTable
) where

import Options.Applicative
import Text.Printf
import Data.List

{- Data Structures -}

data BenchSetting a b = BenchSetting {
    getStruct :: BenchStruct a b,
    getEnv :: BenchEnv,
    getProc :: BenchProc,
    getCase :: BenchCase
} deriving Show

data BenchStruct a b = BenchStruct {
    getName  :: String,
    getCons  :: IO a,
    getInsOp :: a -> b -> IO (),
    getDelOp :: a -> IO ()
}

data BenchEnv = BenchEnv {
    getCountOfWorkers :: {-# UNPACK #-} !Int,
    getCountOpCaps    :: {-# UNPACK #-} !Int
} deriving Show

data BenchProc = BenchProc {
    getInitSize :: {-# UNPACK #-} !Int,
    getInsRate  :: {-# UNPACK #-} !Int,
    getCountOfRuns :: {-# UNPACK #-} !Int,
    getPrepTimeLimit :: {-# UNPACK #-} !Int,
    getFileOutputFlag :: Bool
} deriving Show

data BenchCase
    = ThroughputCase {
        getScale   :: Int,
        getPeriods :: [Int] -- periods in ms
    }
    | TimingCase {
        getCountsOfOps :: [Int], -- amount of operations to be performed
        getAbortionTimeout :: {-# UNPACK #-} !Int -- time before abort in ms
    } deriving Show

data BenchResult
    = ThroughputRes [((Int, Int), Double)]
    | TimingRes [(Int, Int)]
    | AbortedRes String
    deriving Show

data BenchReport a b = BenchReport {
    getSetting :: BenchSetting a b,
    getResult  :: BenchResult
} deriving Show

data BenchCaseKind = Thrput | Timing

data ShortBenchReport = ShortBenchReport {
    getStrName :: String,
    getKind :: BenchCaseKind,
    getInitSz :: Int,
    getInsRt :: Int,
    getCapNum :: Int,
    getMatches :: [(Int, Int, Int)], -- (param, result, error)
    getFileFlag :: Bool
}

data ComposedReport =
    ComposedReport  BenchCaseKind -- kind of benchmark
                    Int -- init size
                    Int -- insertion rate
                    Int -- number of capabilities
                    [String] -- implementation names
                    [(Int, [(Int, Int)])] -- results and errors for given param
                    Bool -- whether to output to file

{- Utils -}

printedTable :: String -> ComposedReport -> (Bool, String, String)
printedTable benchName composedRep = (toFile, stamp, text)
    where
        text = unlines $ map (intercalate "\t") table
        (toFile, stamp, table) = printableTable benchName composedRep

printableTable :: String -> ComposedReport -> (Bool, String, [[String]])
printableTable benchName (ComposedReport kind size rate caps names results toFile) =
    (toFile, benchStamp benchName kindName size rate caps, fstLine : table)
    where
        kindName = paramName kind
        fstLine = kindName : titles names
        titles [] = []
        titles (name:ns) = name : err name : titles ns
        paramName Thrput = "period"
        paramName Timing = "timing"
        err = (++"-err")
        unpair [] = []
        unpair ((a, b):ps) = a : b : unpair ps
        table = map (\(p, ps) -> map show $ p : unpair ps) results

benchStamp :: String -> String -> Int -> Int -> Int -> String
benchStamp benchName kindName size rate caps =
    intercalate "-" [benchName, kindName, size', rate', caps']
    where
        size' = "s" ++ show size
        rate' = "r" ++ show rate
        caps' = "c" ++ show caps

makeComposedReport :: [ShortBenchReport] -> ComposedReport
makeComposedReport [] = error "Cannot composed empty list of reports"
makeComposedReport rs@(ShortBenchReport _ kind initSize insRate capNum _ toFile :_) =
  ComposedReport kind initSize insRate capNum names results toFile
    where
        names = map getStrName rs
        results = map transform $ transpose $ map getMatches rs
        transform [] = error "makeComposedReport: empty lise of reports"
        transform xs@((p, _, _):_) = (p, map get23 xs)
        get23 (_, b, c) = (b, c)

makeShortReport :: BenchReport a b -> ShortBenchReport
makeShortReport (BenchReport setting result) =
    ShortBenchReport
        (getName $ getStruct setting)
        caseKind
        (getInitSize $ getProc setting)
        (getInsRate $ getProc setting)
        (getCountOpCaps $ getEnv setting)
        match
        (getFileOutputFlag $ getProc setting)
    where
        match = zipWith (\p (r, d) -> (p, r, d)) params results
        results = case result of
            AbortedRes msg -> error msg
            TimingRes rs -> rs
            ThroughputRes rs -> map fst rs
        (caseKind, params) = case getCase setting of
            TimingCase ps _ -> (Timing, ps)
            ThroughputCase _ ps -> (Thrput, ps)

{- Applicative parsers -}

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

buildBenchSetting
    :: BenchStruct a b
    -> BenchEnv
    -> BenchProc
    -> IO (BenchSetting a b)
buildBenchSetting strt env defProc =
    execParser $ parseBenchSetting strt env defProc
                 `withInfo` "Concurrent Data Structures Benchmark"

parseBenchSetting
    :: BenchStruct a b
    -> BenchEnv
    -> BenchProc
    -> Parser (BenchSetting a b)
parseBenchSetting strt env defProc = BenchSetting strt env
    <$> parseBenchProc defProc
    <*> parseBenchCase

parseBenchProc :: BenchProc -> Parser BenchProc
parseBenchProc defaultProc = BenchProc
    <$> option auto (value (getInitSize defaultProc)
                     <> long "initsize"
                     <> help "Initial size"
                    )
    <*> option auto (value (getInsRate defaultProc)
                     <> long "insrate"
                     <> help "Percentage of insertions during one run"
                    )
    <*> option auto (value (getCountOfRuns defaultProc)
                     <> long "runs"
                     <> help "Number of runs for each implementation"
                    )
    <*> option auto (value (getPrepTimeLimit defaultProc)
                     <> long "prep"
                     <> help "Time limit for preparation"
                    )
    <*> switch (long "file"
                <> help "Whether to output to file")

parseBenchCase :: Parser BenchCase
parseBenchCase = subparser $
  command "timing"
    (parseTiming
     `withInfo` "Measuring time needed to complete a number of operations"
    ) <>
  command "throughput"
    (parseThroughput
     `withInfo` "Measuring a number of complete operations before timeout"
    )

parseTiming :: Parser BenchCase
parseTiming = builder
    <$> argument auto (metavar "TIMELIMIT")
    <*> argument auto (metavar "N")
    <*> option auto (value 0 <> long "step")
    <*> option auto (value (-1) <> long "upto")
    where
        builder tl n step nn
            | nn < 0 = TimingCase [n] tl
            | otherwise = TimingCase [n, (n+step)..nn] tl

parseThroughput :: Parser BenchCase
parseThroughput = builder
    <$> argument auto (metavar "TIMEOUT")
    <*> option auto (value 0 <> long "step")
    <*> option auto (value (-1) <> long "upto")
    <*> option auto (value 1 <> long "scale")
    where
        builder n step nn scale = ThroughputCase scale $
            if nn < 0 then [n] else [n, (n+step)..nn]

{- Show instances -}

instance Show (BenchStruct a b) where
    show (BenchStruct name _ _ _) = name

instance Show ShortBenchReport where
    show (ShortBenchReport name kind isize irate cn matches _) =
        unlines $ benchHeader : map printMatch matches
        where
            (kindName, parUnits, resUnits) = case kind of
                Thrput -> ("Throughput", "ms", "ops")
                Timing -> ("Timing", "ops", "ms")
            benchHeader = printf "%s of %s (size=%d, ins. rate=%d, cpu units=%d)"
                        kindName name isize irate cn
            printMatch (p, r, d) = printf "%d%s\t->\t%d +- %d%s"
                                        p parUnits r d resUnits
