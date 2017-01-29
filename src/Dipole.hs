
module Dipole where

import NEC

makeDipole :: Double -> Double -> Bool -> NECSim
makeDipole wDia len horizontal = addToSim rot $ addWire wire $ emptySim where
  half = len / 2
  wire = defaultWire { z1 = - half, z2 = half }
  rot = GM $ zeroMove { rY = if horizontal then  90 else 0 }



makeFoldedDipole wDia l r horizontal = addToSim rot $ sim  where
  sim = addToSimTag m2 $ addArch a2 $ addToSimTag m1 $ addArch a1 $ addWire w2 $ addWire w1 $ emptySim
  w1 = defaultWire { z1 = -(l/2) , z2 = (l/2), x1 = r, x2 = r, rad = wDia / 2 }
  w2 = defaultWire { z1 = -(l/2) , z2 = (l/2), x1 = -r, x2 = -r, rad = wDia / 2}
  a1 = defaultArch { archRadius = r, archAngle1 = 0, archAngle2 = 180, archRad = wDia / 2 }  
  a2 = defaultArch { archRadius = r, archAngle1 = 180, archAngle2 = 360, archRad = wDia / 2}  
  m1 = \tg -> GM $ zeroMove { mZ = (l/2), startFromTag = (tg - 1) }
  m2 = \tg -> GM $ zeroMove { mZ = -(l/2), startFromTag = (tg - 1) }
  rot = GM $ zeroMove { rY = if horizontal then  90 else 0 }


