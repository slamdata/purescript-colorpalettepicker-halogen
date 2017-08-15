module ColorPalettePicker.Halogen.Component
  ( input
  , Query(..)
  , Message(..)
  , PickerEffects
  )
  where

import Prelude

import CSS as CSS
import Color (Color)
import Color as Color
import ColorPalettePicker.Utils.Easing (Generator, Progresion, interpolate, lineTo, mix, steps)
import ColorPicker.Halogen.ColorComponents as CPickerComponents
import ColorPicker.Halogen.Component as CPicker
import Control.Monad.Aff.Class (class MonadAff)
import Data.Array (reverse, (..))
import Data.Either.Nested as Either
import Data.Foldable (foldMap)
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type State = {
  color :: Color
}


colorPickerProps ∷ CPicker.Props
colorPickerProps =
  { editing:
      [ [CPickerComponents.componentHue]
        <> CPickerComponents.componentSV
        <> CPickerComponents.componentSL
        <> [CPickerComponents.componentHEX]
      , CPickerComponents.componentRGB
      ]
  , classes: Map.fromFoldable
    [ Tuple CPicker.Root [HH.ClassName "ColorPicker"]
    , Tuple CPicker.Dragger [HH.ClassName "ColorPicker-dragger"]
    , Tuple CPicker.Field [HH.ClassName "ColorPicker-field"]
    , Tuple CPicker.FieldSelector [HH.ClassName "ColorPicker-fieldSelector"]
    , Tuple CPicker.Slider [HH.ClassName "ColorPicker-slider"]
    , Tuple CPicker.SliderSelector [HH.ClassName "ColorPicker-sliderSelector"]
    , Tuple CPicker.Aside [HH.ClassName "ColorPicker-aside"]
    , Tuple CPicker.Stage [HH.ClassName "ColorPicker-stage"]
    , Tuple CPicker.ColorBlockCurrent [HH.ClassName "ColorPicker-colorBlockCurrent"]
    , Tuple CPicker.ColorBlockNext [HH.ClassName "ColorPicker-colorBlockNext"]
    , Tuple CPicker.Editing [HH.ClassName "ColorPicker-editing"]
    , Tuple CPicker.EditingItem [HH.ClassName "ColorPicker-editingItem"]
    , Tuple CPicker.Input [HH.ClassName "ColorPicker-input"]
    , Tuple CPicker.InputLabel [HH.ClassName "ColorPicker-inputLabel"]
    , Tuple CPicker.InputElem [HH.ClassName "ColorPicker-inputElem"]
    , Tuple CPicker.InputElemInvalid [HH.ClassName "ColorPicker-inputElem--invalid"]
    , Tuple CPicker.Actions [HH.ClassName "ColorPicker-actions"]
    , Tuple CPicker.ActionSet [HH.ClassName "ColorPicker-actionSet"]
    ]
  }

type Message = Unit
data Query next
  = Query next
  | PickerEvents CPicker.Message next

type ChildQuery = Coproduct.Coproduct1 CPicker.Query
type Slot = Either.Either1 Unit

cpColor ∷ CP.ChildPath CPicker.Query ChildQuery Unit Slot
cpColor = CP.cp1

type DSL = H.ParentDSL State Query ChildQuery Slot Message
type HTML m = H.ParentHTML Query ChildQuery Slot m

type PickerEffects r = CPicker.PickerEffects r
input ∷ ∀ m r. MonadAff (PickerEffects r) m => H.Component HH.HTML Query Unit Message m
input = H.parentComponent
  { initialState: const $ {color: Color.hsl 0.0 1.0 0.5}
  , render: render
  , eval: eval
  , receiver: const Nothing
  }

render ∷ ∀ m r. MonadAff (PickerEffects r) m => State → HTML m
render state = HH.div [HP.class_ $ HH.ClassName $ "ColorSchemePicker"] $
  [ HH.slot' cpColor unit CPicker.picker colorPickerProps $ HE.input PickerEvents
  , HH.div [HP.class_ $ HH.ClassName $ "Picker-paletteGroupes"] paletteGroupes
  ]
  where
  paletteGroupes = map renderGroup
    [ sequentialPaletes
    , divergingPaletes
    , qualitativePaletes
    ]
  renderGroup { name, generators } =
    HH.div
      [ HP.class_ $ H.ClassName "ColorPalettePicker-paletteGroup"]
      [ HH.div
        [ HP.class_ $ H.ClassName "ColorPalettePicker-paletteGroupHeader" ]
        [ HH.text $ name <> " Palette" ]
      , HH.div
        [ HP.class_ $ H.ClassName "ColorPalettePicker-paletteGroupItems" ]
        $ generators <#> \gen ->
          HH.div
          [ HP.class_ $ HH.ClassName $ "ColorPalettePicker-paletteGroupItem"]
          (renderColors $ gen state.color $ 12)
      ]
  renderColors = map \color -> HH.div
    [ HCSS.style $ CSS.backgroundColor color
    , HP.class_ $ HH.ClassName $ "Picker-paletteColor"
    ]
    []

eval ∷ ∀ m . Query ~> DSL m
eval = case _ of
  Query next -> pure next
  PickerEvents msg next -> do
    case msg of
      CPicker.NextChange color -> H.put {color}
      CPicker.NotifyChange color -> H.put {color}
    pure next

type PaletteGroup = { name ∷ String, generators ∷ Array PaletteGenerator }
sequentialPaletes :: PaletteGroup
sequentialPaletes =
  { name: "Sequential"
  , generators: map mkGenerator sequentialPalettes
  -- , generators: originalSequencialPalettes
  }

divergingPaletes :: PaletteGroup
divergingPaletes =
  { name: "Diverging"
  , generators: map mkGenerator divergingPalettes
  -- , generators: originalDivergingPalettes <> map mkGenerator divergingPalettes
  }

qualitativePaletes :: PaletteGroup
qualitativePaletes =
  { name: "Qualitative"
  , generators: []
  }

hueRange :: (Number -> Number) -> Generator
hueRange f start = start `lineTo` (f start)

mkColors :: Array String -> Array Color
mkColors strs = foldMap (maybe [] pure <<< Color.fromHexString) strs

type HSLGenerator =
  { h :: Generator
  , s :: Generator
  , l :: Generator
  }
mkGenerator :: HSLGenerator -> PaletteGenerator
mkGenerator gen color count = let {h, s, l} = Color.toHSLA color
  in progresses <#> \p -> Color.hsl (gen.h h p) (gen.s s p) (gen.l l p)
  where
  progresses = ((count-1) .. 0) <#> \n -> toNumber n / toNumber (count-1)

type PaletteGenerator = Color -> Int -> Array Color

hueSmall :: Array Number
hueSmall = [ 15.0 ]

hueMiddle :: Array Number
hueMiddle = [ 30.0, 60.0 ]

hueLarge :: Array Number
hueLarge = [ 90.0, 120.0 ]


hueGenerators :: Array Number -> Array (Generator)
hueGenerators hues = do
  hShift <- reverse hues <> map (_ * -1.0) hues
  pure \h -> h `lineTo` (h + hShift)

wideHuePalettes :: Array HSLGenerator
wideHuePalettes = do
  h <- hueGenerators $ hueSmall <> hueMiddle -- <> hueLarge
  pure
    { h
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.60, val: 0.60 }, { at: 0.80, val: 0.75 }]` 0.97
    , l: \n ->
      let
        mixHigh x = interpolate 0.3 x n
        mixLow x = interpolate 0.2 x n
        positions = [{ at: 0.3, val: mixHigh 0.4 }, { at: 0.60, val: mixHigh 0.50 }, { at: 0.8, val: mixLow 0.65 }]
      in
        mixHigh 0.20 `steps positions` mixLow 0.97
    }

smallHuePalletes :: Array HSLGenerator
smallHuePalletes = do
  h <- hueGenerators (hueSmall <> hueMiddle)
  l <-
    [ \n -> interpolate 0.50 0.30 n `lineTo` interpolate 0.1 0.97 n
    , \n -> interpolate 0.30 0.20 n `lineTo` interpolate 0.1 0.97 n ]
  pure { h, s: mix 0.30 $ 1.0 `lineTo` 0.60, l }

sequentialPalettes :: Array HSLGenerator
sequentialPalettes = wideHuePalettes <> smallHuePalletes

divergingPalettes :: Array HSLGenerator
divergingPalettes = [ (_ + 180.0), (_ + 120.0), (_ - 120.0) ] >>= \hueShift ->
  smallHuePalletes <#> \c -> joinPalettes c { h: hueShift >>> c.h, s: c.s, l: c.l }

flipProgresion :: Progresion -> Progresion
flipProgresion a p = a (1.0 - p)

joinProgresion :: Progresion -> Progresion ->Progresion
-- joinProgresion a b p = if p <= 0.5 then a (p * 2.0) else flipProgresion b ((p - 0.5) * 2.0)
joinProgresion a b p = if p <= 0.5 then a (p * 2.0) else b (1.0 - (p - 0.5) * 2.0)

joinPalettes :: HSLGenerator -> HSLGenerator -> HSLGenerator
joinPalettes a b =
  { h: \n -> joinProgresion (a.h n) (b.h n)
  , s: \n -> joinProgresion (a.s n) (b.s n)
  , l: \n -> joinProgresion (a.l n) (b.l n)
  }

-- TODO remove this
originalDivergingPalettes :: Array PaletteGenerator
originalDivergingPalettes =
  [ \_ _ -> mkColors [ "#8c510a" , "#bf812d" , "#dfc27d" , "#f6e8c3" , "#f5f5f5" , "#c7eae5" , "#80cdc1" , "#35978f" , "#01665e" ]
  , \_ _ -> mkColors [ "#c51b7d" , "#de77ae" , "#f1b6da" , "#fde0ef" , "#f7f7f7" , "#e6f5d0" , "#b8e186" , "#7fbc41" , "#4d9221" ]
  , \_ _ -> mkColors [ "#762a83" , "#9970ab" , "#c2a5cf" , "#e7d4e8" , "#f7f7f7" , "#d9f0d3" , "#a6dba0" , "#5aae61" , "#1b7837" ]
  , \_ _ -> mkColors [ "#b35806" , "#e08214" , "#fdb863" , "#fee0b6" , "#f7f7f7" , "#d8daeb" , "#b2abd2" , "#8073ac" , "#542788" ]
  , \_ _ -> mkColors [ "#b2182b" , "#d6604d" , "#f4a582" , "#fddbc7" , "#f7f7f7" , "#d1e5f0" , "#92c5de" , "#4393c3" , "#2166ac" ]
  , \_ _ -> mkColors [ "#d73027" , "#f46d43" , "#fdae61" , "#fee090" , "#ffffbf" , "#e0f3f8" , "#abd9e9" , "#74add1" , "#4575b4" ]
  ]
  where
  mkColors :: Array String -> Array Color
  mkColors strs = foldMap (maybe [] pure <<< Color.fromHexString) strs


-- TODO remove this?
originalSequencialPalettes :: Array PaletteGenerator
originalSequencialPalettes = map mkGenerator
  [ { h: hueRange (_ + 10.0)
    , s: mix 0.25 $ 1.00 `lineTo` 1.00
    , l: mix 0.25 $ 0.25 `lineTo` 1.00
    }
  , { h: hueRange (_ + 30.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.30, val: 0.80 }]` 1.00
    , l: mix 0.25 $ 0.20 `lineTo` 1.00
    }
  , { h: \h -> h `steps [{ at: 0.40, val: h + 5.0 }, { at: 0.70, val: h + 30.0 }]` (h + 35.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.15, val: 0.97 }, { at: 0.25, val: 0.70 }, { at: 0.40, val: 0.95 }]` 1.00
    , l: mix 0.25 $ 0.25 `lineTo` 1.00
    }
  , { h: hueRange (_ + 40.0)
    , s: mix 0.25 $ 1.00 `lineTo` 1.00
    , l: mix 0.25 $ 0.20 `steps [{ at: 0.25, val: 0.45 }]` 1.00
    }
  , { h: \h -> h `steps [{ at: 0.30, val: h - 5.0 }]` (h + 50.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.15, val: 0.95 }, { at: 0.30, val: 0.60 }, { at: 0.50, val: 0.40 }]` 0.60
    , l: mix 0.25 $ 0.15 `lineTo` 1.00
    }
  , { h: \h -> h `lineTo` (h + 60.0)
    , s: mix 0.25 $ 1.00 `lineTo` 0.60
    , l: mix 0.25 $ 0.15 `lineTo` 1.00
    }
  , { h: hueRange (_ + 80.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.25, val: 0.80 }, { at: 0.35, val: 0.90 }]` 1.00
    , l: mix 0.25 $ 0.25 `steps [{ at: 0.25, val: 0.55 }]` 1.00
    }
  , { h: \h -> h `steps [{ at: 0.6, val: h + 60.0 }]` (h + 85.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.70 }, { at: 0.55, val: 0.85 }]` 0.90
    , l: mix 0.25 $ 0.20 `steps [{ at: 0.25, val: 0.35 }, { at: 0.6, val: 0.75 }]` 1.00
    }
  , { h: \h -> h `lineTo` (h + 90.0)
    , s: mix 0.25 $ 1.00 `lineTo` 0.60
    , l: mix 0.25 $ 0.15 `lineTo` 1.00
    }
  , { h: \h -> h `lineTo` (h + 120.0)
    , s: mix 0.25 $ 1.00 `lineTo` 0.60
    , l: mix 0.25 $ 0.15 `lineTo` 1.00
    }
  , { h: \h -> h `steps [{ at: 0.50, val: h + 5.0 }, { at: 0.75, val: h + 40.0 }]` (h + 130.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.25, val: 0.90 }, { at: 0.35, val: 0.55 }]` 0.30
    , l: mix 0.25 $ 0.20 `lineTo` 1.00
    }
  , { h: \h -> h `steps [{ at: 0.5, val: h + 30.0 }, { at: 0.75, val: h + 70.0 }]` (h + 160.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.25, val: 0.97 }, { at: 0.35, val: 0.55 }]` 0.40
    , l: mix 0.25 $ 0.15 `steps [{ at: 0.25, val: 0.3 }, { at: 0.6, val: 0.75 }]` 1.00
    }
  , { h: \h -> h `steps [{ at: 0.5, val: h - 15.0 }]` (h - 5.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.55 }]` 0.65
    , l: mix 0.25 $ 0.25 `lineTo` 1.00
    }
  , { h: \h -> h `steps [{ at: 0.5, val: h - 25.0 }, { at: 0.75, val: h - 30.0 }]` h
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.35, val: 0.30 }]` 0.35
    , l: mix 0.25 $ 0.25 `lineTo` 1.00
    }
  , { h: hueRange (_ - 40.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.15, val: 0.95 }, { at: 0.40, val: 0.40 }]` 0.50
    , l: mix 0.25 $ 0.15 `lineTo` 1.00
    }
  , { h: hueRange (_ - 60.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.15, val: 0.95 }, { at: 0.45, val: 0.80 }, { at: 0.55, val: 0.35 }]` 0.30
    , l: mix 0.25 $ 0.20 `lineTo` 1.00
    }
  , { h: \h -> h `steps [{ at: 0.4, val: h - 25.0 }, { at: 0.6, val: h - 60.0 }]` (h - 95.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.40 }]` 0.95
    , l: mix 0.25 $ 0.15 `steps [{ at: 0.25, val: 0.3 }, { at: 0.6, val: 0.7 }]` 1.00
    }
  , { h: \h -> h `steps [{ at: 0.40, val: h - 20.0 }, { at: 0.50, val: h - 60.0 }]` (h - 110.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.40, val: 0.30 }]` 0.60
    , l: mix 0.25 $ 0.15 `lineTo` 1.00
    }
  , { h: \h -> h `steps [{ at: 0.45, val: h - 10.0 }, { at: 0.75, val: h - 100.0 }]` (h - 120.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.10, val: 0.90 }, { at: 0.30, val: 0.70 }, { at: 0.50, val: 0.45 }]` 0.60
    , l: mix 0.25 $ 0.27 `lineTo` 1.00
    }
  , { h: \h -> h `steps [{ at: 0.3, val: h - 25.0 }, { at: 0.6, val: h - 50.0 }, { at: 0.7, val: h - 120.0 }]` (h - 165.0)
    , s: mix 0.25 $ 1.00 `steps [{ at: 0.10, val: 0.60 }, { at: 0.35, val: 0.75 }, { at: 0.60, val: 0.40 }]` 0.95
    , l: mix 0.25 $ 0.20 `steps [{ at: 0.10, val: 0.4 }, { at: 0.5, val: 0.5 }]` 1.00
    }
  , { h: hueRange id
    , s: mix 0.25 $ 0.00 `lineTo` 0.00
    , l: mix 0.25 $ 0.00 `lineTo` 1.00
    }
  ]


-- NOTE take a look for inspiration
-- https://jiffyclub.github.io/palettable/cmocean/diverging/
-- https://jiffyclub.github.io/palettable/colorbrewer/qualitative/
-- https://jiffyclub.github.io/palettable/colorbrewer/diverging/

-- https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
-- https://github.com/d3/d3-scale-chromatic
-- https://betterfigures.org/2015/06/23/picking-a-colour-scale-for-scientific-graphics/

-- cubehelix
-- https://www.mrao.cam.ac.uk/~dag/CUBEHELIX/
-- https://www.mrao.cam.ac.uk/~dag/CUBEHELIX/cubetry.html
-- http://www.ifweassume.com/2014/04/cubehelix-colormap-for-python.html
-- https://github.com/d3/d3-plugins/tree/master/cubehelix


-- https://gka.github.io/palettes
-- https://github.com/thibauts/bezier-curve
-- https://github.com/gka/chroma.js/blob/d2c6d917df4ba2b87d8a740de116a0656bcbdfd5/src/generator/bezier.coffee
-- [A Primer on Bézier Curves](https://pomax.github.io/bezierinfo/)
-- http://www.albany.edu/faculty/fboscoe/papers/harrower2003.pdf
