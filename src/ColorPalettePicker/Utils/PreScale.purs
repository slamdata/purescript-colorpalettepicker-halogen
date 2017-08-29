module ColorPalettePicker.Utils.PreScale where

import Prelude

import Color (Color)
import Color as Color
import Color.Scale as Scale
import Data.List (List(..), reverse, (:))

type PreScale = { start :: Color, stops :: List PreStop, end :: Color }
type PreStop = { color :: Color, ratio :: Number }

colorStop :: Color -> Number -> PreStop
colorStop = { color: _, ratio: _ }


mkScaleBuilder :: PreScale -> Scale.ColorStops
mkScaleBuilder {start, stops, end} = Scale.ColorStops start (stops <#> \{color, ratio} -> Scale.colorStop color ratio) end

mkScale :: Color.ColorSpace -> PreScale -> Scale.ColorScale
mkScale space {start, stops, end} =
  Scale.colorScale space start (stops <#> \{color, ratio} -> Scale.colorStop color ratio) end

combineScale :: Number → PreScale → PreScale → PreScale
combineScale at start end =
  { start: start.start
  , stops: startStops <> midStops <> endStops
  , end: end.end
  }
  where
  epsilon = 0.01
  startStops = start.stops <#> \{ratio, color} -> colorStop color (ratio / (1.0 / at))
  midStops = (colorStop start.end $ at - epsilon) : (colorStop end.start $ at + epsilon) : Nil
  endStops = end.stops <#> \{ratio, color} -> colorStop color (at + ratio / (1.0 / (1.0 - at)))

reverseScale :: PreScale → PreScale
reverseScale {start, stops, end} =
  { start:end
  , stops: reverse stops <#> \{ratio, color} -> colorStop color (1.0 - ratio)
  , end: start
  }
