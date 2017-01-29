
module YagiOpt where

import Yagi
import SimParse
import PSO
import System.Random
import NEC


c = 299.792458
lambda f = c / f


freq = 447.625

pairs [] = []
pairs [x] = []
pairs (x:y:xs) = (x, y) : pairs xs

prepareYagi (rlen:hlen:hpos:elems) = sim where
  sim' = makeYagi 0.006 ([(rlen, 0), (hlen, hpos)]  ++ directors) False
  sim = setSimRP (90, 10, 1) (90, 10, 1) sim'
  f (len, pos) (lst, acc) = ((len, pos+acc):lst, acc+pos)
  (directors,_) = foldr f ([], hpos) (pairs elems)


evalYagiPso lst@[rlen, hlen, hpos, dlen, dpos] = go where
    targetImpedance = 50
    go = do
      r <- evalSim ygi freq (fitness' targetImpedance)
      r2 <- evalSim ygi (freq-3) (fitnessImepd targetImpedance)
      r3 <- evalSim ygi (freq+3) (fitnessImepd targetImpedance)
      return $ -r
      return $ -(r+(r2+r3)/2)
    ygi = prepareYagi lst

yagiOpt = go where
  lam = lambda freq
  opt = makeOpt [(lam/8, lam), (lam/8, lam), (0.02, lam), (lam/8, lam), (0.02, lam)] 200
  f = evalYagiPso
  go = do
    g <- getStdGen
    fmap optResult $ runOptIO g opt f 100


testYagi y = writeFile "/tmp/test1.nec" $ printSim 420 480 100 False $ setSimRP (90, 10, 37) (90, 10, 37) $  prepareYagi y


prepareFoldedYagi (rlen:hlen:hrad:hpos:elems) = sim where
  sim' = makeFoldedYagi 0.006 hlen hrad hpos ([(rlen, 0)] ++ directors) False
  sim = setSimRP (90, 10, 1) (90, 10, 1) sim'
  f (len, pos) (lst, acc) = ((len, pos+acc):lst, acc+pos)
  (directors,_) = foldr f ([], hpos) (pairs elems)

evalFoldedYagiPso lst@(rlen:hlen:hrad:hpos:elems) = go where
    targetImpedance = 50
    go = do
      r <- evalSim ygi freq (fitness' targetImpedance)
      r2 <- evalSim ygi (freq-10) (fitnessImepd' targetImpedance)
      r3 <- evalSim ygi (freq+10) (fitnessImepd' targetImpedance)
      
      --print r
      --putStrLn ""
      
      return $ -(r+(r2+r3))
      --return $ -(r)
    ygi = prepareFoldedYagi lst

foldedYagiOpt = go where
  lam = lambda freq
  --opt = makeOpt [(lam/8, lam), (lam/8, lam), (0.005, lam/16), (0.02, lam/4), (lam/8, lam), (0.02, lam/4)] 200
  opt = makeOpt [(lam/8, lam), (0.285231, 0.285232), (0.04000001/2, 0.04000002/2), (0.02, lam/4), (lam/8, lam), (0.02, lam/4)] 200
  f = evalFoldedYagiPso
  go = do
    g <- getStdGen
    fmap optResult $ runOptIO g opt f 100


testYagiFolded y = writeFile "/tmp/test1.nec" $ printSim 440 (447+7) 100 False $ setSimRP (90, 10, 37) (90, 10, 37) $  prepareFoldedYagi y
