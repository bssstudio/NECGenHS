{-# LANGUAGE BangPatterns #-}

module SimParse where

import Data.List
import NEC
import System.Process
import System.Random
import Data.Complex



vswr :: Double -> (Double, Double) -> Double
vswr target = vswr' (target, 0)

vswr' :: (Double, Double) -> (Double, Double) -> Double
vswr' (reTar, imTar) (reLoad, imLoad) = swr where
  z0 = reTar :+ imTar
  zL = reLoad :+ imLoad
  refRatio = (zL - z0) / (zL + z0)
  (ro :+ _) = abs $ refRatio
  swr = (1 + ro)  / (1 - ro)


bandwidthInterval :: Double -> Double -> [[String]] -> (Double, Double)
bandwidthInterval targetImp swrThre lnss = (lo, hi) where
  swrs = map (\l -> (frequency l , vswr targetImp (impedance l))) lnss
  below = takeWhile (\s -> (snd s) <= swrThre) $ dropWhile (\s -> (snd s) > swrThre) swrs
  lo = fst $ head below
  hi = fst $ last below


bandwidth t s = (\(lo, hi) -> hi - lo) . bandwidthInterval t s


gain :: [String] -> Double
gain lns = read gn where
  gn = head $ drop 4 $ words gainline
  gainline =  head $ drop 5 $ dropWhile (not . isSubsequenceOf "RADIATION PATTERN") lns


impedance :: [String] -> (Double, Double)
impedance lns = (zr, zi) where
  zr = read $ head line
  zi = read $ head $ tail line
  line = drop 6 $ words paramsLine
  paramsLine = head $ drop 3 $ dropWhile (not . isSubsequenceOf "ANTENNA INPUT PARAMETERS") lns


frequency :: [String] -> Double
frequency lns = read freq where
  freq = head $ drop 2 $ words $ head $ dropWhile (not . isSubsequenceOf "FREQUENCY") $ lns


fitness targetImpedance lns = a * gn - b * (abs $ targetImpedance - reZ) - c * (abs imZ) where
  a = 40
  b = 2
  c = 2
  gn = gain lns
  (reZ, imZ) = impedance lns



fitnessImepd targetImpedance lns = - b * (abs $ targetImpedance - reZ) - c * (abs imZ) where
  b = 2
  c = 2
  gn = gain lns
  (reZ, imZ) = impedance lns



fitness' targetImpedance lns = a * gn - b * ((targetImpedance - reZ)*(targetImpedance - reZ)) - c * (imZ * imZ) where
  a = 40
  b = 2
  c = 2
  gn = gain lns
  (reZ, imZ) = impedance lns

fitnessImepd' targetImpedance lns =  - b * ((targetImpedance - reZ)*(targetImpedance - reZ)) - c * (imZ * imZ) where
  b = 2
  c = 2
  gn = gain lns
  (reZ, imZ) = impedance lns


runSim sim sfreq efreq steps = go where
  splitRuns [] = []
  splitRuns xs = first : splitRuns rest where
    first = takeWhile (not . isSubsequenceOf "- FREQUENCY -") $ rest
    rest = tail $ (dropWhile (not . isSubsequenceOf "- FREQUENCY -")) xs

  go = do
    rn <- randomIO :: IO Int
    let fl = "/tmp/evalnec"++(show rn)
    writeFile (fl++".nec") $ printSim sfreq efreq steps False sim
    system $ "nec2c -i "++fl++".nec"
    !lns <- fmap lines $ readFile (fl++".out")
    system $ "rm "++fl++"*"
    return $ init $ splitRuns $ lns ++ ["- FREQUENCY -"]


evalSim sim freq fitnessF = go where
  --freq = 447.625
  go = do  
    rn <- randomIO :: IO Int
    let fl = "/tmp/evalnec"++(show rn)
    writeFile (fl++".nec") $ printSim freq freq 1 False sim
    system $ "nec2c -i "++fl++".nec"
    !lns <- fmap lines $ readFile (fl++".out")
    system $ "rm "++fl++"*"
    --print $ gain lns
    --print $ impedance lns
    return $ fitnessF lns


  

