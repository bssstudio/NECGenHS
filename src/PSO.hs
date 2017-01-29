
module PSO where

import System.Random
import Data.List
import Control.Monad
import Control.Concurrent.ParallelIO.Local

type PSOParam = Double

type PSOMetric = Double

type PSOParamDesc = (PSOParam, PSOParam) -- lower and upper bound

data Particle = Particle {
                           partParams :: [PSOParam]
                         , partVelocity :: [PSOParam]
                         , partPersonalBest :: [PSOParam]
                         , partPersonalBestMetric :: PSOMetric
                         } deriving (Show)


data Optimization = Optimization {
                                   optDesc :: [PSOParamDesc]
                                 , optParticles :: [Particle]
                                 , optBest :: [PSOParam]
                                 , optBestMetric :: PSOMetric
                                 , optOmega :: PSOParam
                                 , optPhiP :: PSOParam
                                 , optPhiG :: PSOParam
                                 } deriving (Show)


makeOpt :: [PSOParamDesc] -> Int -> Optimization
makeOpt descs numparts = opt where
  part = Particle [] [] [] 10e100
  parts = take numparts $ repeat part
  opt = Optimization { optDesc = descs
                     , optParticles = parts
                     , optBest = []
                     , optBestMetric = 10e100
                     , optOmega = 0.5
                     , optPhiP = 0.3
                     , optPhiG = 0.2
                     }

optResult :: (RandomGen g) => (Optimization, g) -> ([PSOParam], PSOMetric)
optResult (opt, _) = (optBest opt, optBestMetric opt)


initParticles :: (RandomGen g) => g -> Optimization -> (Optimization, g)
initParticles g opt = (newopt, ng) where
  f part (l, gen) = let (npart, ng) = initParticle gen (optDesc opt) part in (npart : l, ng)
  (nprts, ng) = foldr f ([],g) (optParticles opt)
  newopt = opt { optParticles = nprts }


initParticle :: (RandomGen g) => g -> [PSOParamDesc] -> Particle -> (Particle, g)
initParticle g desc part = (npart, ng) where
  -- init random parameter values
  fpar desc (l, gen) = let (p, ng) = randomR desc gen in (p:l, ng)
  (nparams, ng1) = foldr fpar ([], g) desc
  -- init random velocity
  fvel (lo, hi) (l, gen) = let (v, ng) = randomR (-(abs $ hi - lo), abs $ hi - lo) gen in (v:l, ng) 
  (nvel, ng2) = foldr fvel ([], ng1) desc 
  -- get it together
  npart = part { partParams = nparams, partPersonalBest = nparams, partVelocity = nvel }
  ng = ng2


randomRsr :: (Random a, RandomGen g) => (a, a) -> g -> Int -> ([a], g)
randomRsr hilo g 0 = ([], g)
randomRsr hilo g n = (rnd:lst, ng) where
  (lst, ng1) = randomRsr hilo g (n-1)
  (rnd, ng) = randomR hilo ng1


updateVelocity :: (RandomGen g) => g -> (PSOParam, PSOParam) -> (PSOParam, PSOParam) -> (PSOParam, PSOParam, PSOParam) -> (PSOParam, g)
updateVelocity g (vel, pos) (personalBest, swarmBest) (omega, phiP, phiG) = (nparam, ng) where
  ([rp, rg], ng) = randomRsr (0,1) g 2
  nparam = (omega * vel) + (phiP * rp * (personalBest - pos)) + (phiG * rg * (swarmBest - pos))


updatePos :: [PSOParam] -> [PSOParam] -> [PSOParamDesc] -> [PSOParam]
updatePos pos vel bounds = zipWith3 (\p v (lo, hi) -> let np = p + v in min hi $ max lo np) pos vel bounds


updateParticle :: (RandomGen g) => g -> (PSOParam, PSOParam, PSOParam) ->  Particle -> PSOMetric -> [PSOParam] -> [PSOParamDesc]-> (Particle, g)
updateParticle g (omega, phiP, phiG) part fvalue swarmBest descs = (npart, ng) where
  params = partParams part
  personalBest = partPersonalBest part
  personalBestMetric = partPersonalBestMetric part
  velocity = partVelocity part
  fvel (velpos, bests) (lst, gen) = (nvel:lst, ng) where  
    (nvel, ng) = updateVelocity gen velpos bests (omega, phiP, phiG)
  (nvel, ng) = foldr fvel ([], g) (zip (zip velocity params) (zip personalBest swarmBest)) 
  npart = part { partParams = updatePos params nvel descs
               , partVelocity = nvel
               , partPersonalBest = if (fvalue < personalBestMetric) then params else personalBest
               , partPersonalBestMetric = if (fvalue < personalBestMetric) then fvalue else personalBestMetric
               }



optEval :: Optimization -> ([PSOParam] -> PSOMetric) -> [(Particle, PSOMetric)]
optEval opt f = map (\part -> (part, f $ partParams part)) (optParticles opt)


optEvalIO :: Optimization -> ([PSOParam] -> IO PSOMetric) -> IO [(Particle, PSOMetric)]
optEvalIO opt f = mapM (\part -> fmap (\v -> (part, v)) $ f $ partParams part) (optParticles opt)


optEvalParIO :: Int -> Optimization -> ([PSOParam] -> IO PSOMetric) -> IO [(Particle, PSOMetric)]
optEvalParIO n opt f = withPool n $ \pool -> parallel pool $ map (\part -> fmap (\v -> (part, v)) $ f $ partParams part) (optParticles opt)


updateOpt :: (RandomGen g) => g -> Optimization -> [(Particle, PSOMetric)] -> (Optimization, g)
updateOpt g opt evals' = (nopt, ng) where
  evals = sortOn snd evals'
  (swarmBestCnd, swarmBestValCnd) = head evals 
  (swarmBest, swarmBestVal) = if (swarmBestValCnd < optBestMetric opt) then (partParams swarmBestCnd, swarmBestValCnd) else (optBest opt, optBestMetric opt)
  -- update particles
  omega = optOmega opt 
  phiG = optPhiG opt 
  phiP = optPhiP opt
  fpar (part, fval) (lst, gen) = (npart:lst, ng) where
    (npart, ng) = updateParticle gen (omega, phiP, phiG) part fval swarmBest (optDesc opt)
  (nparts, ng) = foldr fpar ([], g) evals
  -- update optimization
  nopt = opt { optBest = swarmBest
             , optBestMetric = swarmBestVal
             , optParticles = nparts
             }





initEvalUpdateParticles :: Optimization -> ([PSOParam] -> PSOMetric) -> Optimization
initEvalUpdateParticles opt f = nopt where
  evals = sortOn snd $ map (\part -> (part, f $ partParams part)) (optParticles opt)
  (swarmBest, swarmBestVal) = head evals 
  nparts = map (\(p, val) -> p { partPersonalBest = partParams p, partPersonalBestMetric = val}) evals
  nopt = opt { optBest = partParams swarmBest
             , optBestMetric = swarmBestVal
             , optParticles = nparts
             }

runOpt :: (RandomGen g) => g -> Optimization -> ([PSOParam] -> PSOMetric) -> Int -> (Optimization, g)
runOpt g opt f n = runOpt' initg initopt n where
  (initopt', initg) = initParticles g opt
  initopt = initEvalUpdateParticles initopt' f
  runOpt' g opt 0 = (opt, g)
  runOpt' g opt n = (nopt, ng) where
    (nsopt, nsg) = updateOpt g opt (optEval opt f)
    (nopt, ng) = runOpt' nsg nsopt (n-1)


    



initEvalUpdateParticlesIO :: Optimization -> ([PSOParam] -> IO PSOMetric) -> IO Optimization
initEvalUpdateParticlesIO opt f = do 
    evals' <- optEvalIO opt f
    let evals = sortOn snd evals' 
        (swarmBest, swarmBestVal) = head evals 
        nparts = map (\(p, val) -> p { partPersonalBest = partParams p, partPersonalBestMetric = val}) evals
    return $ opt { optBest = partParams swarmBest
               , optBestMetric = swarmBestVal
               , optParticles = nparts
               }

runOptIO :: (RandomGen g) => g -> Optimization -> ([PSOParam] -> IO PSOMetric) -> Int -> IO (Optimization, g)
runOptIO g opt f n = go where 
  (initopt', initg) = initParticles g opt

  go = do 
    initopt <- initEvalUpdateParticlesIO initopt' f
    runOpt' initg initopt n

  runOpt' g opt 0 = return (opt, g)
  runOpt' g opt i = do
    evals <- optEvalParIO 16 opt f
    let (nsopt, nsg) = updateOpt g opt evals
    --when ((optBestMetric nsopt) < (optBestMetric opt)) $ do 
    --  print ((optBestMetric nsopt), optBest nsopt, n-i)
    print ((optBestMetric nsopt), optBest nsopt, n-i)
    runOpt' nsg nsopt (i-1)



