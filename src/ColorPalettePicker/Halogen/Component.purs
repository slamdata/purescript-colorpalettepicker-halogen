-- TODO adopt cubehelix scheme
-- * https://github.com/sharkdp/purescript-colors/issues/30
-- TODO adopt Color.Scale.Perceptual.* color scales
-- * https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
-- TODO use perceptually uniform color space
-- * https://github.com/sharkdp/purescript-colors/issues/29
-- TODO clreate palletes using multiple color
-- * https://github.com/sharkdp/purescript-colors/issues/28
-- * https://gka.github.io/palettes
-- NOTE some articles on color palettes and visualisation
-- * https://www.youtube.com/watch?v=DjJr8D4Bxjw
-- * https://earthobservatory.nasa.gov/blogs/elegantfigures/2013/08/05/subtleties-of-color-part-1-of-6/
-- * https://betterfigures.org/2015/06/23/picking-a-colour-scale-for-scientific-graphics/
-- * https://github.com/d3/d3-scale-chromatic


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
import Color.Scale as Scale
import ColorPalettePicker.Utils.Easing (quadratic)
import ColorPalettePicker.Utils.Palettes (divergingPalettes,
  , sequentialPalettes)
import ColorPalettePicker.Utils.PreScale (PreScale, colorStop, combineScale, mkScale, reverseScale)
import ColorPicker.Halogen.ColorComponents as CPickerComponents
import ColorPicker.Halogen.Component as CPicker
import Control.Monad.Aff.Class (class MonadAff)
import Data.Array (fromFoldable, intercalate, reverse, sortBy, take)
import Data.Either.Nested as Either
import Data.Foldable (foldr)
import Data.Functor.Coproduct.Nested as Coproduct
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (abs, sqrt, (%))


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
          (renderColors $ gen state.color $ 9)
      ]
  renderColors colors =
    [ HH.div [HP.class_ $ HH.ClassName $ "Picker-paletteColors" ]
      $ colors <#> \color -> HH.div
        [ HCSS.style $ CSS.backgroundColor color
        , HP.class_ $ HH.ClassName $ "Picker-paletteColor"
        ] []
    , HH.div
        [ HP.class_ $ HH.ClassName $ "Picker-paletteGradient"
        , HCSS.style $ CSS.backgroundImage $ CSS.fromString $ "linear-gradient(to right, "<> intercalate ", " (map Color.cssStringHSLA colors) <>")"
        ] []
    ]

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
  , generators: sequentialPalettes
  }

divergingPaletes :: PaletteGroup
divergingPaletes =
  { name: "Diverging"
  , generators: divergingPalettes
  }

qualitativePaletes :: PaletteGroup
qualitativePaletes =
  { name: "Qualitative"
  , generators:

  }
