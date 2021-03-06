
module Yagi where

import NEC
import Dipole


makeYagi :: Double -> [(Double, Double)] -> Bool -> NECSim
makeYagi wDia elms horizontal = yagi where 
  startSim = emptySim { excitation = (2, 8) }
  yagi = addToSim rot $ foldl f startSim elms 
  f sim (l, pos) = wire l pos sim
  wire l pos sim = addWire defaultWire { z1 = - (l/2), z2 = (l/2), y1 = pos, y2 = pos, rad = wDia / 2} sim
  rot = GM $ zeroMove { rY = if horizontal then  90 else 0 }




makeFoldedYagi :: Double -> Double -> Double -> Double -> [(Double, Double)] -> Bool -> NECSim
makeFoldedYagi wDia l r pos elms horizontal = yagi where 
  startSim = addToSim (GM $ zeroMove { mY = pos }) $ makeFoldedDipole wDia l r horizontal
  yagi = addToSim rot $ foldl f startSim elms 
  f sim (l, pos) = wire l pos sim
  wire l pos sim = addWire defaultWire { z1 = - (l/2), z2 = (l/2), y1 = pos, y2 = pos, rad = wDia / 2} sim
  rot = GM $ zeroMove { rY = if horizontal then  90 else 0 }




