
module NEC where

import Data.List

data Wire = Wire {
               tag :: Int
             , segments :: Int
             , x1 :: Double
             , y1 :: Double
             , z1 :: Double
             , x2 :: Double
             , y2 :: Double
             , z2 :: Double
             , rad :: Double
             } deriving (Show, Eq)

defaultRad = 0.0025 / 2.0

defaultWire = Wire {
                     tag = 0
                   , segments = 15
                   , x1 = 0
                   , y1 = 0
                   , z1 = 0
                   , x2 = 0
                   , y2 = 0
                   , z2 = 0
                   --, rad = 0.0005 -- d = 1mm
                   --, rad = 0.003 -- d = 6mm
                   --, rad = 0.006 / 2.0 -- d = 2.5mm
                   , rad = defaultRad -- d = 2.5mm
                   } 

defaultArch =  Arch {
                      archTag = 0
                    , archSegments = 15
                    , archRadius = 0
                    , archAngle1 = 0
                    , archAngle2 = 0
                    , archRad = defaultRad
                    }

data Arch = Arch {
                   archTag :: Int
                 , archSegments :: Int
                 , archRadius :: Double
                 , archAngle1 :: Double
                 , archAngle2 :: Double
                 , archRad :: Double
                 } deriving (Show, Eq)

data Move = Move {
               tagIncrement :: Int
             , newStructures :: Int
             , startFromTag :: Int
             , rX :: Double
             , rY :: Double
             , rZ :: Double
             , mX :: Double
             , mY :: Double
             , mZ :: Double
             } deriving (Show, Eq)

zeroMove = Move {
                 tagIncrement = 0
               , newStructures = 0
               , startFromTag = 0
               , rX = 0
               , rY = 0
               , rZ = 0
               , mX = 0 
               , mY = 0
               , mZ = 0
               }


data NEC = GW Wire | GA Arch | GM Move deriving Show

printNEC (GW w) = intercalate " " $ [ "GW"
                                , (show $ tag w) 
                                , (show $ segments w) 
                                , (show $ x1 w)
                                , (show $ y1 w)
                                , (show $ z1 w)
                                , (show $ x2 w)
                                , (show $ y2 w)
                                , (show $ z2 w)
                                , (show $ rad w)
                                ]
printNEC (GA a) = intercalate " " $ [ "GA"
                                    , show $ archTag a 
                                    , show $ archSegments a 
                                    , show $ archRadius a 
                                    , show $ archAngle1 a 
                                    , show $ archAngle2 a 
                                    , show $ archRad a 
                                    ]
printNEC (GM m) = intercalate " " $ [ "GM"
                                    , show $ tagIncrement m 
                                    , show $ newStructures m 
                                    , show $ rX m 
                                    , show $ rY m 
                                    , show $ rZ m 
                                    , show $ mX m  
                                    , show $ mY m 
                                    , show $ mZ m 
                                    , show $ startFromTag m 
                                    ]
                                

printGeometry sim = (intercalate "\n" $ map printNEC $ reverse $ geo sim) ++ "\nGE\n"

printSim sfreq efreq steps ground sim = out where
  stepsize = (efreq - sfreq) / (fromIntegral steps)
  (exTag, exSeg) = excitation sim
  out = ("CM -- hsnecgen --\nCE\n") ++ (printGeometry sim) ++ (intercalate "\n" [
      "EX     0     "++(show exTag)++"     "++(show exSeg)++"      0  1.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00"
    , "FR     0    "++ (show steps) ++"     0      0  "++(show sfreq)++"  "++(show stepsize)++"  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00"
    , "NH     0     0     0      0  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00"
    , "NE     0    10     1     10 -1.35000E+00  0.00000E+00 -1.35000E+00  3.00000E-01  0.00000E+00  3.00000E-01"
    , if ground then "GN     1     0     0      0  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00" else ""
    , "RP     0    "++ (if ground then "19" else "37") ++ "    37   1000 "++ (if ground then "-90" else "0") ++ "  0.00000E+00  1.00000E+01  1.00000E+01  0.00000E+00  0.00000E+00"
    , "EN     0     0     0      0  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00"
    , ""
    ])

data NECSim = NECSim { 
                       geo :: [NEC]
                     , nextTag :: Int
                     , excitation :: (Int, Int)
                     } deriving Show

emptySim = NECSim { geo = [], nextTag = 1 , excitation = (1, 8)}

addToSim :: NEC -> NECSim -> NECSim 
addToSim n sim = sim { geo = n : (geo sim) }

addToSimTag :: (Int -> NEC) -> NECSim -> NECSim 
addToSimTag n sim = sim { geo = (n (nextTag sim)) : (geo sim) }




incTag :: NECSim -> NECSim 
incTag sim = sim { nextTag = (nextTag sim) + 1 }

addWire :: Wire -> NECSim -> NECSim
addWire w sim = nsim where 
  nsim = incTag $ addToSim wire sim
  wire = GW $ w { tag = (nextTag sim) }


addArch :: Arch -> NECSim -> NECSim
addArch a sim = nsim where 
  nsim = incTag $ addToSim arch sim
  arch = GA $ a { archTag = (nextTag sim) }


