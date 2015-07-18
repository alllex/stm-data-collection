
module BenchData (
    BenchSetting(BenchSetting),
    BenchStruct(BenchStruct),
    BenchEnv(BenchEnv),
    BenchProc(BenchProc),
    BenchCase(ThroughputCase, TimingCase),
    BenchResult(ThroughputRes, TimingRes, AbortedRes),
    BenchReport(BenchReport),
    buildBenchSetting
) where

import Options.Applicative

{- Data Structures -}

data BenchSetting a = BenchSetting {
    getStruct :: BenchStruct a,
    getEnv :: BenchEnv,
    getProc :: BenchProc,
    getCase :: BenchCase
} deriving Show

data BenchStruct a = BenchStruct {
    getName  :: String,
    getInsOp :: a -> IO (),
    getDelOp :: IO ()
}

data BenchEnv = BenchEnv {
    getCountOfWorkers :: Int,
    getCountOpCaps    :: Int
} deriving Show

data BenchProc = BenchProc {
    getInitSize :: Int,
    getInsRate  :: Int,
    getCountOfRuns :: Int
} deriving Show

data BenchCase
    = ThroughputCase {
        getPeriod :: Int -- period in ms
    }
    | TimingCase {
        getCountOfOps :: Int, -- amount of operations to be performed
        getAbortionTimeout :: Int -- time before abort in ms
    } deriving Show

data BenchResult
    = ThroughputRes Int Int
    | TimingRes Int Int
    | AbortedRes String
    deriving Show

data BenchReport a = BenchReport {
        getSetting :: BenchSetting a,
        getResults :: [(String, BenchResult)]
} deriving Show

{- Applicative parsers -}

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

buildBenchSetting
    :: BenchStruct a
    -> BenchEnv
    -> BenchProc
    -> IO (BenchSetting a)
buildBenchSetting strt env defProc =
    execParser $ parseBenchSetting strt env defProc
                 `withInfo` "Concurrent Data Structures Benchmark"

parseBenchSetting
    :: BenchStruct a
    -> BenchEnv
    -> BenchProc
    -> Parser (BenchSetting a)
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
parseTiming = TimingCase
    <$> argument auto (metavar "N")
    <*> argument auto (metavar "TIMELIMIT")

parseThroughput :: Parser BenchCase
parseThroughput = ThroughputCase
    <$> argument auto (metavar "TIMEOUT")

{- Show instances -}

instance Show (BenchStruct a) where
    show (BenchStruct name _ _) = name
