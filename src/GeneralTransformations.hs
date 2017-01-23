
module GeneralTransformations where

import NEC

aboveGround :: Double -> NECSim -> NECSim
aboveGround m nec = addToSim move nec where
  move = GM $ zeroMove { mZ = m }

tilt :: Double -> NECSim -> NECSim
tilt t = addToSim tlt where
  tlt = GM $ zeroMove { rX = t }
