module ColorPalettePicker.Utils.Palettes
  ( sequentialPalettes
  , divergingPalettes
  , qualitativePalettes
  , PaletteGenerator
  ) where

import Prelude

import Color (Color)
import Color as Color
import Color.Scale as Scale
import ColorPalettePicker.Utils.Easing (quadratic)
import ColorPalettePicker.Utils.PreScale (PreScale, colorStop, combineScale, mkScale, reverseScale)
import Data.Array (fromFoldable, reverse, sortBy, take)
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import Data.List as List
import Math (abs, sqrt, (%))


type PaletteGenerator = Color -> Int -> Array Color

sequentialPalettes :: Array PaletteGenerator
sequentialPalettes = map
  (mkSequentialPalette >>> preScaleToGenerator)
  (hueShifts [ 0.0 ] [ 30.0, 60.0, 90.0, 120.0 ])
  where
  hueShifts :: Array Number -> Array Number -> Array Number
  hueShifts mid hues = reverse hues <> mid <> map (_ * -1.0) hues

divergingPalettes :: Array PaletteGenerator
divergingPalettes = map preScaleToGenerator do
  hueShift <- [45.0, 25.0, 0.0, -25.0, -45.0]
  secondaryHueShift <- [ -135.0, -90.0, 90.0, 135.0, 180.0]
  pure \color ->
    let
      hsl = Color.toHSLA color
      secondaryColor = Color.hsla (secondaryHueShift + hsl.h) hsl.s hsl.l hsl.a
      start = reverseScale $ mkSequentialPalette hueShift secondaryColor
      end = mkSequentialPalette hueShift color
    in start `combineScale 0.5` end

qualitativePalettes :: Array PaletteGenerator
qualitativePalettes = map mkGenerator palettes
  where
  palettes =
    [ [ { h: 0.0, s: 0.0, l: 0.4 }
      , { h: 24.29, s: 0.785, l: 0.4196 }
      , { h: 29.24, s: 0.9675, l: 0.7588 }
      , { h: 60.0, s: 1.0, l: 0.8 }
      , { h: 120.0, s: 0.4066, l: 0.6431 }
      , { h: 214.0, s: 0.5172, l: 0.4549 }
      , { h: 265.26, s: 0.3065, l: 0.7569 }
      , { h: 328.49, s: 0.9835, l: 0.4745} ]
    , [ { h: 0.0, s: 0.0, l: 0.4 }
      , { h: 25.95, s: 0.9817, l: 0.4294 }
      , { h: 38.98, s: 0.7026, l: 0.3824 }
      , { h: 44.47, s: 0.9828, l: 0.4549 }
      , { h: 88.24, s: 0.6939, l: 0.3843 }
      , { h: 162.14, s: 0.7081, l: 0.3627 }
      , { h: 244.48, s: 0.3059, l: 0.5706 }
      , { h: 329.37, s: 0.7983, l: 0.5333} ]
    , [ { h: 0.61, s: 0.9245, l: 0.7922 }
      , { h: 21.46, s: 0.6313, l: 0.4255 }
      , { h: 29.88, s: 1.0, l: 0.5 }
      , { h: 33.8, s: 0.9726, l: 0.7137 }
      , { h: 60.0, s: 1.0, l: 0.8 }
      , { h: 91.76, s: 0.5705, l: 0.7078 }
      , { h: 116.38, s: 0.5686, l: 0.4 }
      , { h: 200.66, s: 0.5214, l: 0.7706 }
      , { h: 204.16, s: 0.7062, l: 0.4137 }
      , { h: 269.03, s: 0.4326, l: 0.4216 }
      , { h: 280.0, s: 0.3051, l: 0.7686 }
      , { h: 359.4, s: 0.7945, l: 0.4961} ]
    , [ { h: 0.0, s: 0.0, l: 0.949 }
      , { h: 4.68, s: 0.9059, l: 0.8333 }
      , { h: 34.77, s: 0.9778, l: 0.8235 }
      , { h: 40.5, s: 0.4348, l: 0.8196 }
      , { h: 60.0, s: 1.0, l: 0.9 }
      , { h: 108.95, s: 0.4872, l: 0.8471 }
      , { h: 207.5, s: 0.4615, l: 0.7961 }
      , { h: 285.6, s: 0.3165, l: 0.8451 }
      , { h: 329.14, s: 0.8974, l: 0.9235} ]
    , [ { h: 0.0, s: 0.0, l: 0.8 }
      , { h: 24.44, s: 0.9529, l: 0.8333 }
      , { h: 35.68, s: 0.5692, l: 0.8725 }
      , { h: 50.37, s: 1.0, l: 0.8412 }
      , { h: 80.45, s: 0.6875, l: 0.8745 }
      , { h: 153.19, s: 0.4476, l: 0.7941 }
      , { h: 219.31, s: 0.3867, l: 0.8529 }
      , { h: 322.86, s: 0.6563, l: 0.8745} ]
    , [ { h: 0.0, s: 0.0, l: 0.6 }
      , { h: 21.9, s: 0.6117, l: 0.4039 }
      , { h: 29.88, s: 1.0, l: 0.5 }
      , { h: 60.0, s: 1.0, l: 0.6 }
      , { h: 118.22, s: 0.4056, l: 0.4882 }
      , { h: 206.98, s: 0.5397, l: 0.4686 }
      , { h: 292.24, s: 0.3527, l: 0.4725 }
      , { h: 328.47, s: 0.8806, l: 0.7373 }
      , { h: 359.41, s: 0.7953, l: 0.498} ]
    , [ { h: 0.0, s: 0.0, l: 0.702 }
      , { h: 16.75, s: 0.9625, l: 0.6863 }
      , { h: 35.56, s: 0.609, l: 0.7392 }
      , { h: 49.04, s: 1.0, l: 0.5922 }
      , { h: 82.73, s: 0.6286, l: 0.5882 }
      , { h: 161.09, s: 0.4299, l: 0.5804 }
      , { h: 221.61, s: 0.3735, l: 0.6745 }
      , { h: 323.23, s: 0.6596, l: 0.7235} ]
    , [ { h: 0.0, s: 0.0, l: 0.851 }
      , { h: 6.13, s: 0.9448, l: 0.7157 }
      , { h: 31.74, s: 0.9748, l: 0.6882 }
      , { h: 52.5, s: 1.0, l: 0.7176 }
      , { h: 60.0, s: 1.0, l: 0.851 }
      , { h: 82.05, s: 0.6393, l: 0.6412 }
      , { h: 108.95, s: 0.4872, l: 0.8471 }
      , { h: 169.71, s: 0.443, l: 0.6902 }
      , { h: 204.58, s: 0.4854, l: 0.6647 }
      , { h: 247.5, s: 0.3019, l: 0.7922 }
      , { h: 299.02, s: 0.3161, l: 0.6216 }
      , { h: 329.36, s: 0.8868, l: 0.8961}
      ]
    ]
  -- TODO use Lab for persceptual distance
  hslDistance :: ∀ r z
    . { h :: Number , s :: Number , l :: Number | r }
    → { h :: Number , s :: Number , l :: Number | z }
    → Number
  hslDistance a b = sqrt $ sqr (hueDistance a_h b_h) + sqr (a_s - b_s)  + sqr (a_l - b_l)
    where
    hueDistance a b = d'/ 1.8
      where
      d = abs (a - b)
      d' = if d < 180.0 then d else 360.0 - d
    a_h = a.h
    b_h = b.h
    a_s = a.s * 100.0
    b_s = b.s * 100.0
    a_l = a.l * 100.0
    b_l = b.l * 100.0
  sqr a = a * a

  mkGenerator palette seedColor =
    takeer seedColor $
      fromFoldable $ foldr foldFunc List.Nil palette
    where
    distance start end = abs $ end - start
    seed = Color.toHSLA seedColor
    foldFunc color@{h, s, l} rest =
      let mixed = {h, s: (s + seed.s) / 2.0, l: (l + seed.l) / 2.0 }
      in
        if hslDistance seed mixed > 10.0 then
          Cons (Color.hsla mixed.h mixed.s mixed.l 1.0) rest
        else
          rest
  takeer seed arr count = huesort $ take (count - 1) arr <> [seed]
  huesort = sortBy huesorter
  huesorter a b =  compare (Color.toHSLA a).h (Color.toHSLA b).h



preScaleToGenerator :: (Color -> PreScale) -> PaletteGenerator
preScaleToGenerator f c n =  fromFoldable $ Scale.colors (mkScale Color.Lab $ f c) n

mkSequentialPalette :: Number -> Color -> PreScale
mkSequentialPalette hueShift inputColor = {start: startColor, stops, end: endColor}
  where
  input = Color.toHSVA inputColor
  endColor = Color.hsva
    input.h
    (quadratic 0.5 0.90 input.s)
    (quadratic 0.5 0.95 input.v)
    input.a
  end = Color.toHSLA endColor
  start =
    { h: end.h + hueShift
    , s: quadratic 0.4 0.70 end.s
    , l: quadratic 0.92 0.97 end.l
    , a: end.a
    }
  startColor = Color.hsla start.h start.s start.l start.a
  mid =
    { h: start.h
    , s: (end.s + start.s) / 2.0
    , l: (end.l + start.l) / 2.0
    , a: end.a
    }
  absHueShift = (abs hueShift % 180.0)
  stops =
    if absHueShift > 40.0 then
      colorStop (Color.hsla mid.h mid.s mid.l mid.a) 0.30 : Nil
    else if absHueShift > 20.0 then
      colorStop (Color.hsla mid.h mid.s mid.l mid.a) 0.45 : Nil
    else if absHueShift > 10.0 then
      colorStop (Color.hsla mid.h mid.s mid.l mid.a) 0.60 : Nil
    else Nil
