module ColorPalettePicker.Utils.PreScale where

import Prelude

import Color.Scale (ColorStops(..), colorStop, stopRatio, stopColor)
import Data.List (List(..), reverse, (:))


-- for ```(redToBlue `combineStops x` orangeToGray)```
-- color at `x` will be orange and color at `x - combineEpsilon` will be blue
-- if we want color at `x` to be blue, then `combineStops (x+combineEpsilon)` could be used
combineEpsilon :: Number
combineEpsilon = 0.000001

combineStops :: Number → ColorStops → ColorStops → ColorStops
combineStops = combineStops' combineEpsilon

combineStops' :: Number → Number → ColorStops → ColorStops → ColorStops
combineStops' epsilon at (ColorStops bStart bStops bEnd) (ColorStops eStart eStops eEnd) =
  ColorStops bStart (startStops <> midStops <> endStops) eEnd
  where
  startStops = bStops <#>
    \stop -> colorStop (stopColor stop) (stopRatio stop / (1.0 / at))
  midStops = (colorStop bEnd $ at - epsilon) : (colorStop eStart $ at) : Nil
  endStops = eStops <#>
    \stop -> colorStop (stopColor stop) (at + stopRatio stop / (1.0 / (1.0 - at)))

reverseStops :: ColorStops → ColorStops
reverseStops (ColorStops start stops end) =
  ColorStops end newStops start
  where
  newStops = reverse stops <#>
    \stop -> colorStop (stopColor stop) (1.0 - stopRatio stop)
