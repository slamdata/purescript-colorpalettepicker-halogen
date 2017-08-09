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
import ColorPalettePicker.Utils.Easing (Progresion, lineTo, steps, mix)
import ColorPicker.Halogen.ColorComponents as CPickerComponents
import ColorPicker.Halogen.Component as CPicker
import Control.Monad.Aff.Class (class MonadAff)
import Data.Array ((..))
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
    [ Tuple CPicker.Root [HH.ClassName "ColorPicker", HH.ClassName "ColorPicker--large"]
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
render state = HH.div_ $
  [ HH.slot' cpColor unit CPicker.picker colorPickerProps $ HE.input PickerEvents
  , HH.div [HP.class_ $ HH.ClassName $ "paletteSet"]
    $ paletes <#> \(Tuple name { gen, colors, count }) ->
      HH.div
      [ HP.class_ $ HH.ClassName $ "paletteGroup"]
      [ HH.p_ [HH.text name]
      , HH.div [HP.class_ $ HH.ClassName $ "palettes"]
        [ HH.div
          [ HP.classes [HH.ClassName "palette", HH.ClassName "palette-original"] ]
          (renderColors colors)
        , HH.div
          [HP.classes [HH.ClassName "palette", HH.ClassName "palette-dinamic"] ]
          (renderColors $ gen state.color $ count * 2)
        ]
      ]
  ]
  where
  renderColors = map \color -> HH.div
    [ HCSS.style $ CSS.backgroundColor color]
    [ HH.text $ Color.cssStringHSLA color ]

eval ∷ ∀ m . Query ~> DSL m
eval = case _ of
  Query next -> pure next
  PickerEvents msg next -> do
    case msg of
      CPicker.NextChange color -> H.put {color}
      CPicker.NotifyChange color -> H.put {color}
    pure next


hueRange :: (Number -> Number) -> Number -> Progresion
hueRange f start = start `lineTo` (f start)

mkColors :: Array String -> Array Color
mkColors strs = foldMap (maybe [] pure <<< Color.fromHexString) strs

mkGenerator ::
  { h :: Number -> Progresion
  , s :: Number -> Progresion
  , l :: Number -> Progresion
  } -> Color -> Int -> Array Color
mkGenerator gen color count = let {h, s, l} = Color.toHSLA color
  in progresses <#> \p -> Color.hsl (gen.h h p) (gen.s s p) (gen.l l p)
  where
  progresses = ((count-1) .. 0) <#> \n -> toNumber n / toNumber (count-1)


paletes ::
  Array
    (Tuple
      String
      { gen :: Color -> Int -> Array Color
      , colors :: Array Color
      , count :: Int
      }
    )
paletes =
  [ Tuple "sequential: blue green" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.30, val: h - 5.0 }]` (h + 50.0)
      , s: mix 0.15 $ 1.00 `steps [{ at: 0.15, val: 0.95 }, { at: 0.30, val: 0.60 }, { at: 0.50, val: 0.40 }]` 0.60
      , l: mix 0.15 $ 0.15 `lineTo` 0.97
      }
    , colors: mkColors [ "#f7fcfd" , "#e5f5f9" , "#ccece6" , "#99d8c9" , "#66c2a4" , "#41ae76" , "#238b45" , "#006d2c" , "#00441b" ]
    }
  , Tuple "sequential: blue purple" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.40, val: h - 20.0 }, { at: 0.50, val: h - 60.0 }]` (h - 110.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.40, val: 0.30 }]` 0.60
      , l: mix 0.15 $ 0.15 `lineTo` 0.97
      }
    , colors: mkColors [ "#f7fcfd" , "#e0ecf4" , "#bfd3e6" , "#9ebcda" , "#8c96c6" , "#8c6bb1" , "#88419d" , "#810f7c" , "#4d004b" ]
    }
  , Tuple "sequential: green blue" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.45, val: h - 10.0 }, { at: 0.75, val: h - 100.0 }]` (h - 120.0)
      , s: mix 0.15 $ 0.85 `steps [{ at: 0.1, val: 0.90 }, { at: 0.3, val: 0.70 }, { at: 0.5, val: 0.45 }]` 0.60
      , l: mix 0.15 $ 0.27 `lineTo` 0.96
      }
    , colors: mkColors [ "#f7fcf0" , "#e0f3db" , "#ccebc5" , "#a8ddb5" , "#7bccc4" , "#4eb3d3" , "#2b8cbe" , "#0868ac" , "#084081" ]
    }
  , Tuple "sequential: orange red" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.40, val: h + 5.0 }, { at: 0.70, val: h + 30.0 }]` (h + 35.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.15, val: 0.97 }, { at: 0.25, val: 0.7 }, { at: 0.40, val: 0.95 }]` 1.0
      , l: mix 0.15 $ 0.25 `lineTo` 0.97
      }
    , colors: mkColors [ "#fff7ec" , "#fee8c8" , "#fdd49e" , "#fdbb84" , "#fc8d59" , "#ef6548" , "#d7301f" , "#b30000" , "#7f0000" ]
    }
  , Tuple "sequential: purple blue" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.50, val: h + 5.0 }, { at: 0.75, val: h + 40.0 }]` (h + 130.0)
      , s: mix 0.15 $ 0.95 `steps [{ at: 0.25, val: 0.90 }, { at: 0.35, val: 0.55 }]` 0.3
      , l: mix 0.15 $ 0.20 `lineTo` 0.95
      }
    , colors: mkColors [ "#fff7fb" , "#ece7f2" , "#d0d1e6" , "#a6bddb" , "#74a9cf" , "#3690c0" , "#0570b0" , "#045a8d" , "#023858" ]
    }
  , Tuple "sequential: purple blue green" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.5, val: h + 30.0 }, { at: 0.75, val: h + 70.0 }]` (h + 160.0)
      , s: mix 0.15 $ 0.95 `steps [{ at: 0.25, val: 0.97 }, { at: 0.35, val: 0.55 }]` 0.4
      , l: mix 0.15 $ 0.15 `steps [{ at: 0.25, val: 0.3 }, { at: 0.6, val: 0.75 }]` 0.98
      }
    , colors: mkColors [ "#fff7fb" , "#ece2f0" , "#d0d1e6" , "#a6bddb" , "#67a9cf" , "#3690c0" , "#02818a" , "#016c59" , "#014636" ]
    }
  , Tuple "sequential: purple red" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ - 60.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.15, val: 0.95 }, { at: 0.45, val: 0.80 }, { at: 0.55, val: 0.35 }]` 0.3
      , l: mix 0.15 $ 0.20 `lineTo` 0.97
      }
    , colors: mkColors [ "#f7f4f9" , "#e7e1ef" , "#d4b9da" , "#c994c7" , "#df65b0" , "#e7298a" , "#ce1256" , "#980043" , "#67001f" ]
    }
  , Tuple "sequential: red purple" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.6, val: h + 60.0 }]` (h + 85.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.70 }, { at: 0.55, val: 0.85 }]` 0.9
      , l: mix 0.15 $ 0.20 `steps [{ at: 0.25, val: 0.35 }, { at: 0.6, val: 0.75 }]` 0.97
      }
    , colors: mkColors [ "#fff7f3" , "#fde0dd" , "#fcc5c0" , "#fa9fb5" , "#f768a1" , "#dd3497" , "#ae017e" , "#7a0177" , "#49006a" ]
    }
  , Tuple "sequential: yellow green" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.4, val: h - 25.0 }, { at: 0.6, val: h - 60.0 }]` (h - 95.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.40 }]` 0.95
      , l: mix 0.15 $ 0.15 `steps [{ at: 0.25, val: 0.3 }, { at: 0.6, val: 0.7 }]` 0.95
      }
    , colors: mkColors [ "#ffffe5" , "#f7fcb9" , "#d9f0a3" , "#addd8e" , "#78c679" , "#41ab5d" , "#238443" , "#006837" , "#004529" ]
    }
  , Tuple "sequential: yellow green blue" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.3, val: h - 25.0 }, { at: 0.6, val: h - 50.0 }, { at: 0.7, val: h - 120.0 }]` (h - 165.0)
      , s: mix 0.15 $ 0.85 `steps [{ at: 0.10, val: 0.6 }, { at: 0.35, val: 0.75 }, { at: 0.60, val: 0.40 }]` 0.95
      , l: mix 0.15 $ 0.20 `steps [{ at: 0.10, val: 0.4 }, { at: 0.5, val: 0.5 }]` 0.95
      }
    , colors: mkColors [ "#ffffd9" , "#edf8b1" , "#c7e9b4" , "#7fcdbb" , "#41b6c4" , "#1d91c0" , "#225ea8" , "#253494" , "#081d58" ]
    }
  , Tuple "sequential: yellow orange brown" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ + 40.0)
      , s: mix 0.15 $ 0.85 `lineTo` 1.0
      , l: mix 0.15 $ 0.20 `steps [{ at: 0.25, val: 0.45 }]` 0.95
      }
    , colors: mkColors [ "#ffffe5" , "#fff7bc" , "#fee391" , "#fec44f" , "#fe9929" , "#ec7014" , "#cc4c02" , "#993404" , "#662506" ]
    }
  , Tuple "sequential: yellow orange red" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ + 80.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.25, val: 0.8 }, { at: 0.35, val: 0.9 }]` 1.0
      , l: mix 0.15 $ 0.25 `steps [{ at: 0.25, val: 0.55 }]` 1.0
      }
    , colors: mkColors [ "#ffffcc" , "#ffeda0" , "#fed976" , "#feb24c" , "#fd8d3c" , "#fc4e2a" , "#e31a1c" , "#bd0026" , "#800026" ]
    }
  , Tuple "sequential: blues" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.5, val: h - 15.0 }]` (h - 5.0)
      , s: mix 0.15 $ 0.85 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.55 }]` 0.65
      , l: mix 0.15 $ 0.25 `lineTo` 0.97
      }
    , colors: mkColors [ "#f7fbff" , "#deebf7" , "#c6dbef" , "#9ecae1" , "#6baed6" , "#4292c6" , "#2171b5" , "#08519c" , "#08306b" ]
    }
  , Tuple "sequential: greens" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ - 40.0)
      , s: mix 0.15 $ 0.85 `steps [{ at: 0.15, val: 0.95 }, { at: 0.40, val: 0.4 }]` 0.5
      , l: mix 0.15 $ 0.15 `lineTo` 0.97
      }
    , colors: mkColors [ "#f7fcf5" , "#e5f5e0" , "#c7e9c0" , "#a1d99b" , "#74c476" , "#41ab5d" , "#238b45" , "#006d2c" , "#00441b" ]
    }
  , Tuple "sequential: greys" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ + 0.0)
      , s: mix 0.15 $ 0.0 `lineTo` 0.0
      , l: mix 0.15 $ 0.0 `lineTo` 1.0
      }
    , colors: mkColors [ "#ffffff" , "#f0f0f0" , "#d9d9d9" , "#bdbdbd" , "#969696" , "#737373" , "#525252" , "#252525" , "#000000" ]
    }
  , Tuple "sequential: oranges" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ + 10.0)
      , s: mix 0.15 $ 0.90 `lineTo` 1.0
      , l: mix 0.15 $ 0.25 `lineTo` 0.95
      }
    , colors: mkColors [ "#fff5eb" , "#fee6ce" , "#fdd0a2" , "#fdae6b" , "#fd8d3c" , "#f16913" , "#d94801" , "#a63603" , "#7f2704" ]
    }
  , Tuple "sequential: purples" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.5, val: h - 25.0 }, { at: 0.75, val: h - 30.0 }]` h
      , s: mix 0.15 $ 0.90 `steps [{ at: 0.35, val: 0.3 }]` 0.35
      , l: mix 0.15 $ 0.25 `lineTo` 0.95
      }
    , colors: mkColors [ "#fcfbfd" , "#efedf5" , "#dadaeb" , "#bcbddc" , "#9e9ac8" , "#807dba" , "#6a51a3" , "#54278f" , "#3f007d" ]
    }
  , Tuple "sequential: reds" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ + 30.0)
      , s: mix 0.15 $ 1.00 `steps [{ at: 0.30, val: 0.8 }]` 1.0
      , l: mix 0.15 $ 0.20 `lineTo` 0.95
      }
    , colors: mkColors [ "#fff5f0" , "#fee0d2" , "#fcbba1" , "#fc9272" , "#fb6a4a" , "#ef3b2c" , "#cb181d" , "#a50f15" , "#67000d" ]
    }
    {--
  , Tuple "diverging: brown-blue green" $
    { count: 285
    , gen: const $ const []
    , colors:mkColors [ "#8c510a" , "#bf812d" , "#dfc27d" , "#f6e8c3" , "#f5f5f5" , "#c7eae5" , "#80cdc1" , "#35978f" , "#01665e" ]
    }
  , Tuple "diverging: pink-yellow green" $
    { count: 285
    , gen: const $ const []
    , colors:mkColors [ "#c51b7d" , "#de77ae" , "#f1b6da" , "#fde0ef" , "#f7f7f7" , "#e6f5d0" , "#b8e186" , "#7fbc41" , "#4d9221" ]
    }
  , Tuple "diverging: purple-green" $
    { count: 285
    , gen: const $ const []
    , colors:mkColors [ "#762a83" , "#9970ab" , "#c2a5cf" , "#e7d4e8" , "#f7f7f7" , "#d9f0d3" , "#a6dba0" , "#5aae61" , "#1b7837" ]
    }
  , Tuple "diverging: purple-orange" $
    { count: 285
    , gen: const $ const []
    , colors:mkColors [ "#b35806" , "#e08214" , "#fdb863" , "#fee0b6" , "#f7f7f7" , "#d8daeb" , "#b2abd2" , "#8073ac" , "#542788" ]
    }
  , Tuple "diverging: red-blue" $
    { count: 285
    , gen: const $ const []
    , colors:mkColors [ "#b2182b" , "#d6604d" , "#f4a582" , "#fddbc7" , "#f7f7f7" , "#d1e5f0" , "#92c5de" , "#4393c3" , "#2166ac" ]
    }
  , Tuple "diverging: red-yellow-blue" $
    { count: 285
    , gen: const $ const []
    , colors:mkColors [ "#d73027" , "#f46d43" , "#fdae61" , "#fee090" , "#ffffbf" , "#e0f3f8" , "#abd9e9" , "#74add1" , "#4575b4" ]
    }
    --}
  ]
  where
  mkColors strs = foldMap (maybe [] pure <<< Color.fromHexString) strs
