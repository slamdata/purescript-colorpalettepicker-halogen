module ColorPalettePicker.Utils.Palettes
  ( Palette
  , mkPalette

  , SequentialGenerator(..)
  , SequentialGeneratorSpec
  , sequentialToCSSGradient
  , sequentialPaletteGenerators
  , runSequentialGenerator

  , DivergingGenerator(..)
  , DivergingGeneratorSpec
  , divergingToCSSGradient
  , divergingPaletteGenerators
  , runDivergingGenerator

  , QualitativeGenerator(..)
  , QualitativeGeneratorSpec
  , ColorHSL
  , qualitativePaletteGenerators
  , runQualitativeGenerator
  ) where

import Prelude

import CSS as CSS
import Color (Color)
import Color as Color
import Color.Scale as Scale
import ColorPalettePicker.Utils.Easing (linear, quadratic)
import ColorPalettePicker.Utils.PreScale (combineStops, reverseStops)
import Data.Array (fromFoldable, intercalate, reverse, sortBy, take, uncons)
import Data.Foldable (foldr)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (fromJust)
import Data.NonEmpty (NonEmpty(..))
import Math (abs, sqrt, (%))
import Partial.Unsafe (unsafePartial)

type Palette =
  { seed :: Color
  , sequential :: SequentialGenerator
  , diverging :: DivergingGenerator
  , qualitative :: QualitativeGenerator
  }

data PaletteType = Sequential | Diverging | Qualitative


newtype SequentialGenerator = SequentialGenerator SequentialGeneratorSpec

type SequentialGeneratorSpec =
  { hueShift :: Number
  , darknessRange :: { min :: Number, max :: Number }
  , lightnessRange :: { min :: Number, max :: Number }
  }



derive instance sequentialGeneratorEq :: Eq SequentialGenerator
derive instance sequentialGeneratorOrd :: Ord SequentialGenerator


newtype DivergingGenerator = DivergingGenerator DivergingGeneratorSpec

type DivergingGeneratorSpec =
  { sequentialGenerator :: SequentialGeneratorSpec
  , startColorHueShift :: Number
  }

derive instance divergingGeneratorEq :: Eq DivergingGenerator
derive instance divergingGeneratorOrd :: Ord DivergingGenerator


newtype QualitativeGenerator = QualitativeGenerator QualitativeGeneratorSpec

type QualitativeGeneratorSpec =
  { colors :: Array ColorHSL }

newtype ColorHSL = ColorHSL { h :: Number, s :: Number, l :: Number }

derive instance colorHSLEq :: Eq ColorHSL
derive instance colorHSLOrd :: Ord ColorHSL

derive instance qualitativeGeneratorEq :: Eq QualitativeGenerator
derive instance qualitativeGeneratorOrd :: Ord QualitativeGenerator


type PaletteRunner = Color -> Int -> Array Color

mkPalette
  :: Color
  -> SequentialGenerator
  -> DivergingGenerator
  -> QualitativeGenerator
  -> Palette
mkPalette =
  { seed: _
  , sequential: _
  , diverging: _
  , qualitative: _
  }

sequentialPaletteGenerators :: NonEmpty Array SequentialGenerator
sequentialPaletteGenerators = nonEmpty cubehelixGenerators
  where
  cubehelixGenerators = do
    hueShift <- hueShifts [ 0.0] [ 30.0, 60.0, 90.0, 120.0, 150.0, 180.0, 210.0, 240.0, 270.0, 300.0, 330.0, 360.0]
    [ SequentialGenerator { hueShift, darknessRange: {min: 0.1, max: 0.5}, lightnessRange: {min: 0.85, max: 0.97} }
    , SequentialGenerator { hueShift, darknessRange: {min: 0.0, max: 0.2}, lightnessRange: {min: 0.92, max: 1.0} }
    ]

hueShifts :: Array Number -> Array Number -> Array Number
hueShifts mid hues = reverse hues <> mid <> map (_ * -1.0) hues

divergingPaletteGenerators :: NonEmpty Array DivergingGenerator
divergingPaletteGenerators = nonEmpty $ cubehelixGenerators
  where
  cubehelixGenerators = do
    -- TODO make sure spaces do not overlap
    hueShift <- hueShifts [0.0] [45.0, 90.0]
    sequentialGenerator <-
      [ { hueShift, darknessRange: {min: 0.1, max: 0.3}, lightnessRange: {min: 0.95, max: 1.0} }
      , { hueShift, darknessRange: {min: 0.2, max: 0.5}, lightnessRange: {min: 0.95, max: 1.0} }
      ]
    secondaryHueShift <- [-135.0, -90.0, 90.0, 135.0, 180.0]
    pure $ DivergingGenerator
      { sequentialGenerator
      , startColorHueShift: secondaryHueShift
      }
qualitativePaletteGenerators :: NonEmpty Array QualitativeGenerator
qualitativePaletteGenerators = nonEmpty $ map
  ({colors: _} >>> QualitativeGenerator)
  [ [ hsl 0.0     0.0     0.4
    , hsl 24.29   0.785   0.4196
    , hsl 29.24   0.9675  0.7588
    , hsl 60.0    1.0     0.8
    , hsl 120.0   0.4066  0.6431
    , hsl 214.0   0.5172  0.4549
    , hsl 265.26  0.3065  0.7569
    , hsl 328.49  0.9835  0.4745
    ]
  , [ hsl 0.0     0.0     0.4
    , hsl 25.95   0.9817  0.4294
    , hsl 38.98   0.7026  0.3824
    , hsl 44.47   0.9828  0.4549
    , hsl 88.24   0.6939  0.3843
    , hsl 162.14  0.7081  0.3627
    , hsl 244.48  0.3059  0.5706
    , hsl 329.37  0.7983  0.5333
    ]
  , [ hsl 0.61    0.9245  0.7922
    , hsl 21.46   0.6313  0.4255
    , hsl 29.88   1.0     0.5
    , hsl 33.8    0.9726  0.7137
    , hsl 60.0    1.0     0.8
    , hsl 91.76   0.5705  0.7078
    , hsl 116.38  0.5686  0.4
    , hsl 200.66  0.5214  0.7706
    , hsl 204.16  0.7062  0.4137
    , hsl 269.03  0.4326  0.4216
    , hsl 280.0   0.3051  0.7686
    , hsl 359.4   0.7945  0.4961
    ]
  , [ hsl 0.0     0.0     0.949
    , hsl 4.68    0.9059  0.8333
    , hsl 34.77   0.9778  0.8235
    , hsl 40.5    0.4348  0.8196
    , hsl 60.0    1.0     0.9
    , hsl 108.95  0.4872  0.8471
    , hsl 207.5   0.4615  0.7961
    , hsl 285.6   0.3165  0.8451
    , hsl 329.14  0.8974  0.9235
    ]
  , [ hsl 0.0     0.0     0.8
    , hsl 24.44   0.9529  0.8333
    , hsl 35.68   0.5692  0.8725
    , hsl 50.37   1.0     0.8412
    , hsl 80.45   0.6875  0.8745
    , hsl 153.19  0.4476  0.7941
    , hsl 219.31  0.3867  0.8529
    , hsl 322.86  0.6563  0.8745
    ]
  , [ hsl 0.0     0.0     0.6
    , hsl 21.9    0.6117  0.4039
    , hsl 29.88   1.0     0.5
    , hsl 60.0    1.0     0.6
    , hsl 118.22  0.4056  0.4882
    , hsl 206.98  0.5397  0.4686
    , hsl 292.24  0.3527  0.4725
    , hsl 328.47  0.8806  0.7373
    , hsl 359.41  0.7953  0.498
    ]
  , [ hsl 0.0     0.0     0.702
    , hsl 16.75   0.9625  0.6863
    , hsl 35.56   0.609   0.7392
    , hsl 49.04   1.0     0.5922
    , hsl 82.73   0.6286  0.5882
    , hsl 161.09  0.4299  0.5804
    , hsl 221.61  0.3735  0.6745
    , hsl 323.23  0.6596  0.7235
    ]
  , [ hsl 0.0     0.0     0.851
    , hsl 6.13    0.9448  0.7157
    , hsl 31.74   0.9748  0.6882
    , hsl 52.5    1.0     0.7176
    , hsl 60.0    1.0     0.851
    , hsl 82.05   0.6393  0.6412
    , hsl 108.95  0.4872  0.8471
    , hsl 169.71  0.443   0.6902
    , hsl 204.58  0.4854  0.6647
    , hsl 247.5   0.3019  0.7922
    , hsl 299.02  0.3161  0.6216
    , hsl 329.36  0.8868  0.8961
    ]
  ]
  where
  hsl h s l = ColorHSL {h, s, l}


runQualitativeGenerator :: Int -> Color -> QualitativeGenerator -> Array Color
runQualitativeGenerator n seedColor (QualitativeGenerator {colors}) =
  takeer seedColor (fromFoldable $ foldr foldFunc List.Nil colors) n
  where
  distance start end = abs $ end - start
  seed = Color.toHSLA seedColor
  foldFunc (ColorHSL color) rest =
    let
      mixed = {h: color.h, s: (color.s + seed.s) / 2.0, l: (color.l + seed.l) / 2.0 }
    in
      if hslDistance seed mixed > 10.0 then
        Cons (Color.hsla mixed.h mixed.s mixed.l 1.0) rest
      else
        rest
  takeer seed arr count = huesort $ take (count - 1) arr <> [seed]
  huesort = sortBy huesorter
  huesorter a b =  compare (Color.toHSLA a).h (Color.toHSLA b).h
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

runSequentialGenerator :: Int -> Color -> SequentialGenerator -> Array Color
runSequentialGenerator n seed (SequentialGenerator spec) =
  (mkRunner $ mkSequentialPalette spec)
  seed
  n

runDivergingGenerator :: Int -> Color -> DivergingGenerator -> Array Color
runDivergingGenerator n seed (DivergingGenerator spec) = (mkRunner scale) seed n
  where
  scale = \color ->
    let
      hsl = Color.toHSLA color
      start = reverseStops
        $ mkSequentialPalette spec.sequentialGenerator
        $ Color.hsla (spec.startColorHueShift + hsl.h) hsl.s hsl.l hsl.a
      end = mkSequentialPalette spec.sequentialGenerator color
    in start `combineStops 0.5` end

sequentialToCSSGradient ::  Color -> SequentialGenerator -> CSS.BackgroundImage
sequentialToCSSGradient seed g = mkGradient $ runSequentialGenerator 5 seed g

divergingToCSSGradient :: Color -> DivergingGenerator -> CSS.BackgroundImage
divergingToCSSGradient seed g = mkGradient $ runDivergingGenerator 10 seed g


nonEmpty :: ∀ a. Array a -> NonEmpty Array a
nonEmpty arr =
  let {head, tail} = (unsafePartial $ fromJust $ uncons arr)
  in NonEmpty head tail

mkGradient :: Array Color -> CSS.BackgroundImage
mkGradient colors = CSS.fromString
  $ "linear-gradient(to right, " <> intercalate ", " (map Color.cssStringHSLA colors) <> ")"

mkRunner :: (Color -> Scale.ColorStops) -> PaletteRunner
mkRunner f c n = fromFoldable $ Scale.colors' (Scale.cubehelixSample $ f c) n


mkSequentialPalette
  :: SequentialGeneratorSpec
  -> Color
  -> Scale.ColorStops
mkSequentialPalette {hueShift, lightnessRange, darknessRange} inputColor = Scale.ColorStops startColor stops endColor
  where
  input = Color.toHSLA inputColor
  endColor = Color.hsla
    input.h
    (quadratic 0.5 0.90 input.s)
    (quadratic darknessRange.min darknessRange.max input.l)
    input.a
  end = Color.toHSLA endColor
  start =
    { h: end.h + hueShift
    , s: quadratic 0.4 0.70 end.s
    , l: linear lightnessRange.min lightnessRange.max  input.l
    , a: end.a
    }
  startColor = Color.hsla start.h start.s start.l start.a
  absHueShift = (abs hueShift % 180.0)
  stops = Nil
