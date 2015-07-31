
import Control.Monad
import qualified System.Random as SysR
import qualified System.Random.PCG.Fast.Pure as R
import System.Random.PCG.Class (sysRandom)
import Data.Word (Word64, Word32)
import Data.List (foldl')
import qualified Data.Map as Map

{- Appropriate benchmark section for .cabal

benchmark random-bench
  type:                 exitcode-stdio-1.0
  main-is:              RandomBench.hs
  hs-source-dirs:       benchmarks, src
  default-language:     Haskell2010
  build-depends:        base >=4.8 && <4.9, stm >=2.4 && <2.5,
                        random >=1.1 && <1.2, pcg-random >=0.2 && <0.3,
                        containers >=0.5 && <0.6
  ghc-options:          -Wall -O2 -fno-omit-yields

-}

gen :: Word64
    -> Int
    -> ((Word64, [a]) -> Int -> (Word64, [a]))
    -> (a -> b)
    -> [b]
gen st0 n f m = res
    where
        (_, folded) = foldl' f (st0, []) [1..n]
        mapped = map m folded
        res = mapped

mbw32 :: Word32
mbw32 = maxBound

genPure :: Word64 -> Int -> [Int]
genPure st0 n = gen st0 n f m
    where
        f :: (Word64, [Word32]) -> Int -> (Word64, [Word32])
        f (st, xs) _ = case R.pair st of (R.P st' x) -> (st', x:xs)
        m' :: Word32 -> Float
        m' w = fromIntegral w / fromIntegral mbw32
        m :: Word32 -> Int -- generated number to height with logarithmic distribution
        m w = (+1) $ truncate $ logBase 0.5 (m' w)

genSys :: Word64 -> Int -> [Int]
genSys st0 n = gen st0 n folder mapper
    where
        folder :: (Word64, [Float]) -> Int -> (Word64, [Float])
        folder (st, xs) _ = (st', x:xs)
            where st' = R.state st
                  x = fst . SysR.random $ R.initFrozen st
        mapper :: Float -> Int
        mapper x = (+1) $ truncate $ logBase 0.5 x

count :: Int -> [Int] -> [(Int, Int)]
count h xs = res
    where
        mp0 = Map.fromList [(k, 0) | k <- [1..h+1]]
        norm x | x < 1 = error "Illegal height"
               | x > h = h + 1
               | otherwise = x
        f mp x = Map.update (Just . (+1)) (norm x) mp
        mpn :: Map.Map Int Int
        mpn = foldl' f mp0 xs
        res = Map.toList mpn

main :: IO ()
main = do
    let h = 20
        samplesn = 2^h
        perc :: Int -> Int
        perc c = (c * 100) `div` samplesn
        printRes (h', c) = putStrLn $ show h' ++ " -> " ++ show c ++ " (" ++ show (perc c) ++ "%)"
    putStrLn $ "h=" ++ show h ++ ", samples count=" ++ show samplesn
    st0 <- sysRandom
    let sxs = genSys st0 samplesn
        shs = count h sxs
        pxs = genPure st0 samplesn
        phs = count h pxs
    putStrLn "Pure random distribution"
    forM_ phs printRes
    putStrLn "System random distribution"
    forM_ shs printRes
