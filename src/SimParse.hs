
module SimParse where
import Data.List
import NEC
import System.Process

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



fitness lns = a * gn - b * (abs $ 50 - reZ) - c * (abs imZ) where
  a = 40
  b = 2
  c = 2
  gn = gain lns
  (reZ, imZ) = impedance lns


evalSim sim = go where
  freq = 447.625
  fl = "/tmp/test1"
  go = do  
    writeFile (fl++".nec") $ printSim freq freq 1 False sim
    system $ "nec2c -i "++fl++".nec"
    lns <- fmap lines $ readFile (fl++".out")
    print $ gain lns
    print $ impedance lns
    return $ fitness lns


  

