
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
    getCountOfRuns :: {-# UNPACK #-} !Int
} deriving Show

data BenchCase
    = ThroughputCase {
        getPeriod :: {-# UNPACK #-} !Int -- period in ms
    }
    | TimingCase {
        getCountOfOps :: {-# UNPACK #-} !Int, -- amount of operations to be performed
        getAbortionTimeout :: {-# UNPACK #-} !Int -- time before abort in ms
    } deriving Show

data BenchResult
    = ThroughputRes {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    | TimingRes {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    | AbortedRes String
    deriving Show

data BenchReport a b = BenchReport {
    getSetting :: BenchSetting a b,
    getResult  :: BenchResult
} deriving Show

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

instance Show (BenchStruct a b) where
    show (BenchStruct name _ _ _) = name
