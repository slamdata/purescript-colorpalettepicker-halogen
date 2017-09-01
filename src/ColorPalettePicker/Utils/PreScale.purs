module ColorPalettePicker.Utils.PreScale where

import Prelude

import Color.Scale (ColorStops(..), colorStop, stopRatio, stopColor)
import Data.List (List(..), reverse, (:))


combineScale :: Number → ColorStops → ColorStops → ColorStops
combineScale at (ColorStops bStart bStops bEnd) (ColorStops eStart eStops eEnd) =
  ColorStops bStart (startStops <> midStops <> endStops) eEnd
  where
  epsilon = 0.01
  startStops = bStops <#>
    \stop -> colorStop (stopColor stop) (stopRatio stop / (1.0 / at))
  midStops = (colorStop bEnd $ at - epsilon) : (colorStop eStart $ at + epsilon) : Nil
  endStops = eStops <#>
    \stop -> colorStop (stopColor stop) (at + stopRatio stop / (1.0 / (1.0 - at)))

reverseScale :: ColorStops → ColorStops
reverseScale (ColorStops start stops end) =
  ColorStops end newStops start
  where
  newStops = reverse stops <#>
    \stop -> colorStop (stopColor stop) (1.0 - stopRatio stop)
