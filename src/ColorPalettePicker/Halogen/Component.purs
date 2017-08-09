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
  , HH.div [HP.class_ $ HH.ClassName $ "Picker-paletteList"]
    $ paletes <#> \gen ->
      HH.div
      [ HP.class_ $ HH.ClassName $ "Picker-palette"]
      (renderColors $ gen state.color $ 9)
  ]
  where
  renderColors = map \color -> HH.div
    [ HCSS.style $ CSS.backgroundColor color
    , HP.class_ $ HH.ClassName $ "Picker-paletteColor"]
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


paletes :: Array (Color -> Int -> Array Color)
paletes =
  [ mkGenerator
      { h: \h -> h `steps [{ at: 0.30, val: h - 5.0 }]` (h + 50.0)
      , s: mix 0.15 $ 1.00 `steps [{ at: 0.15, val: 0.95 }, { at: 0.30, val: 0.60 }, { at: 0.50, val: 0.40 }]` 0.60
      , l: mix 0.15 $ 0.15 `lineTo` 0.97
      }
  , mkGenerator
      { h: \h -> h `steps [{ at: 0.40, val: h - 20.0 }, { at: 0.50, val: h - 60.0 }]` (h - 110.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.40, val: 0.30 }]` 0.60
      , l: mix 0.15 $ 0.15 `lineTo` 0.97
      }
  , mkGenerator
      { h: \h -> h `steps [{ at: 0.45, val: h - 10.0 }, { at: 0.75, val: h - 100.0 }]` (h - 120.0)
      , s: mix 0.15 $ 0.85 `steps [{ at: 0.1, val: 0.90 }, { at: 0.3, val: 0.70 }, { at: 0.5, val: 0.45 }]` 0.60
      , l: mix 0.15 $ 0.27 `lineTo` 0.96
      }
  , mkGenerator
      { h: \h -> h `steps [{ at: 0.40, val: h + 5.0 }, { at: 0.70, val: h + 30.0 }]` (h + 35.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.15, val: 0.97 }, { at: 0.25, val: 0.7 }, { at: 0.40, val: 0.95 }]` 1.0
      , l: mix 0.15 $ 0.25 `lineTo` 0.97
      }
  , mkGenerator
      { h: \h -> h `steps [{ at: 0.50, val: h + 5.0 }, { at: 0.75, val: h + 40.0 }]` (h + 130.0)
      , s: mix 0.15 $ 0.95 `steps [{ at: 0.25, val: 0.90 }, { at: 0.35, val: 0.55 }]` 0.3
      , l: mix 0.15 $ 0.20 `lineTo` 0.95
      }
  , mkGenerator
      { h: \h -> h `steps [{ at: 0.5, val: h + 30.0 }, { at: 0.75, val: h + 70.0 }]` (h + 160.0)
      , s: mix 0.15 $ 0.95 `steps [{ at: 0.25, val: 0.97 }, { at: 0.35, val: 0.55 }]` 0.4
      , l: mix 0.15 $ 0.15 `steps [{ at: 0.25, val: 0.3 }, { at: 0.6, val: 0.75 }]` 0.98
      }
  , mkGenerator
      { h: hueRange (_ - 60.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.15, val: 0.95 }, { at: 0.45, val: 0.80 }, { at: 0.55, val: 0.35 }]` 0.3
      , l: mix 0.15 $ 0.20 `lineTo` 0.97
      }
  , mkGenerator
      { h: \h -> h `steps [{ at: 0.6, val: h + 60.0 }]` (h + 85.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.70 }, { at: 0.55, val: 0.85 }]` 0.9
      , l: mix 0.15 $ 0.20 `steps [{ at: 0.25, val: 0.35 }, { at: 0.6, val: 0.75 }]` 0.97
      }
  , mkGenerator
      { h: \h -> h `steps [{ at: 0.4, val: h - 25.0 }, { at: 0.6, val: h - 60.0 }]` (h - 95.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.40 }]` 0.95
      , l: mix 0.15 $ 0.15 `steps [{ at: 0.25, val: 0.3 }, { at: 0.6, val: 0.7 }]` 0.95
      }
  , mkGenerator
      { h: \h -> h `steps [{ at: 0.3, val: h - 25.0 }, { at: 0.6, val: h - 50.0 }, { at: 0.7, val: h - 120.0 }]` (h - 165.0)
      , s: mix 0.15 $ 0.85 `steps [{ at: 0.10, val: 0.6 }, { at: 0.35, val: 0.75 }, { at: 0.60, val: 0.40 }]` 0.95
      , l: mix 0.15 $ 0.20 `steps [{ at: 0.10, val: 0.4 }, { at: 0.5, val: 0.5 }]` 0.95
      }
  , mkGenerator
      { h: hueRange (_ + 40.0)
      , s: mix 0.15 $ 0.85 `lineTo` 1.0
      , l: mix 0.15 $ 0.20 `steps [{ at: 0.25, val: 0.45 }]` 0.95
      }
  , mkGenerator
      { h: hueRange (_ + 80.0)
      , s: mix 0.15 $ 1.0 `steps [{ at: 0.25, val: 0.8 }, { at: 0.35, val: 0.9 }]` 1.0
      , l: mix 0.15 $ 0.25 `steps [{ at: 0.25, val: 0.55 }]` 1.0
      }
  , mkGenerator
      { h: \h -> h `steps [{ at: 0.5, val: h - 15.0 }]` (h - 5.0)
      , s: mix 0.15 $ 0.85 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.55 }]` 0.65
      , l: mix 0.15 $ 0.25 `lineTo` 0.97
      }
  , mkGenerator
      { h: hueRange (_ - 40.0)
      , s: mix 0.15 $ 0.85 `steps [{ at: 0.15, val: 0.95 }, { at: 0.40, val: 0.4 }]` 0.5
      , l: mix 0.15 $ 0.15 `lineTo` 0.97
      }
  , mkGenerator
      { h: hueRange (_ + 10.0)
      , s: mix 0.15 $ 0.90 `lineTo` 1.0
      , l: mix 0.15 $ 0.25 `lineTo` 0.95
      }
  , mkGenerator
      { h: \h -> h `steps [{ at: 0.5, val: h - 25.0 }, { at: 0.75, val: h - 30.0 }]` h
      , s: mix 0.15 $ 0.90 `steps [{ at: 0.35, val: 0.3 }]` 0.35
      , l: mix 0.15 $ 0.25 `lineTo` 0.95
      }
  , mkGenerator
      { h: hueRange (_ + 30.0)
      , s: mix 0.15 $ 1.00 `steps [{ at: 0.30, val: 0.8 }]` 1.0
      , l: mix 0.15 $ 0.20 `lineTo` 0.95
      }
  , mkGenerator
      { h: hueRange id
      , s: mix 0.15 $ 0.0 `lineTo` 0.0
      , l: mix 0.15 $ 0.0 `lineTo` 1.0
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
  -- where
  -- mkColors strs = foldMap (maybe [] pure <<< Color.fromHexString) strs
