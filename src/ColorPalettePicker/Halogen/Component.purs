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
  , Input
  , Query(..)
  , Message(..)
  , PickerEffects
  )
  where

import Prelude

import CSS as CSS
import Color (Color)
import Color as Color
import ColorPalettePicker.Utils.Palettes (PaletteGenerator, mkPalette, runPalette, toCSSGradient, divergingPaletteGenerator, qualitativePaletteGenerator, sequentialPaletteGenerator)
import ColorPicker.Halogen.Component as CPicker
import ColorPicker.Halogen.Layout as L
import Control.Monad.Aff.Class (class MonadAff)
import Control.MonadZero (guard)
import Control.Plus (empty)
import Data.Either.Nested as Either
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
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
  { layout:
    [ H.ClassName "ColorPicker" ] `L.Root`
      [ [ H.ClassName "ColorPicker-dragger" ] `L.Group`
          [ L.Component $ L.componentDragSV
              { root: [ H.ClassName "ColorPicker-field" ]
              , isLight: [ H.ClassName "IsLight" ]
              , isDark: [ H.ClassName "IsDark" ]
              , selector: [ H.ClassName "ColorPicker-fieldSelector"]
              }
          , L.Component $ L.componentDragHue
              { root: [ H.ClassName "ColorPicker-slider" ]
              , selector: [ H.ClassName "ColorPicker-sliderSelector"]
              }
          ]
      , [ H.ClassName "ColorPicker-aside" ] `L.Group`
          [ [ H.ClassName "ColorPicker-stage" ] `L.Group`
              [ L.Component $ L.componentPreview [ H.ClassName "ColorPicker-colorBlockCurrent" ]
              , L.Component $ L.componentHistory 4 [ H.ClassName "ColorPicker-colorBlockOld" ]
              ]
          , [ H.ClassName "ColorPicker-editing" ] `L.Group`
              [ [ H.ClassName "ColorPicker-editingItem" ] `L.Group`
                  [ L.Component $ L.componentHue inputClasses
                  , L.Component $ L.componentSaturationHSV inputClasses
                  , L.Component $ L.componentValue inputClasses
                  , L.Component $ L.componentSaturationHSL inputClasses
                  , L.Component $ L.componentLightness inputClasses
                  ]
              , [ H.ClassName "ColorPicker-editingItem" ] `L.Group`
                  [ L.Component $ L.componentRed inputClasses
                  , L.Component $ L.componentGreen inputClasses
                  , L.Component $ L.componentBlue inputClasses
                  , L.Component $ L.componentHEX inputClasses
                  ]
              ]
          , [ H.ClassName "ColorPicker-actions" ] `L.Group`
              [ L.Component $ L.componentSet [ H.ClassName "ColorPicker-actionSet" ] ]
          ]
      ]
  }
  where
  inputClasses ∷ L.InputProps
  inputClasses =
    { root: [H.ClassName "ColorPicker-input"]
    , label: [H.ClassName "ColorPicker-inputLabel"]
    , elem: [H.ClassName "ColorPicker-inputElem"]
    , elemInvalid: [H.ClassName "ColorPicker-inputElem--invalid"]
    }

type Input = Unit

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

input ∷ ∀ m r. MonadAff (PickerEffects r) m => H.Component HH.HTML Query Input Message m
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
    [ sequentialPaletteGroup
    , divergingPaletteGroup
    , qualitativePaletteGroup
    ]
  renderGroup { name, generators } =
    HH.div
      [ HP.class_ $ H.ClassName "ColorPalettePicker-paletteGroup"]
      [ HH.div
        [ HP.class_ $ H.ClassName "ColorPalettePicker-paletteGroupHeader" ]
        [ HH.text $ name <> " Palette" ]
      , HH.div
        [ HP.class_ $ H.ClassName "ColorPalettePicker-paletteGroupItems" ]
        $ generators <#> \generator ->
          (renderPalette (mkPalette generator state.color))


      ]
  renderPalette palette =
    HH.div
      [ HP.classes  $ [HH.ClassName "ColorPalettePicker-paletteGroupItem"] <> hasGradient] $
      [ HH.div [HP.class_ $ HH.ClassName $ "Picker-palette" ]
        $ runPalette palette 9 <#> \color -> HH.div
          [ HCSS.style $ CSS.backgroundColor color
          , HP.class_ $ HH.ClassName $ "Picker-paletteColor"
          ] []
      ] <> gradient
    where
    toAlt = maybe empty pure
    hasGradient = guard (isJust cssGradient) $> H.ClassName "hasGradient"
    cssGradient = toCSSGradient palette
    gradient = toAlt $ cssGradient <#> \img -> HH.div
      [ HP.class_ $ HH.ClassName $ "Picker-paletteGradient"
      , HCSS.style $ CSS.backgroundImage img
      ] []


eval ∷ ∀ m . Query ~> DSL m
eval = spy >>> case _ of
  Query next -> pure next
  PickerEvents msg next -> do
    case msg of
      CPicker.NextChange color -> H.put {color}
      CPicker.NotifyChange color -> H.put {color}
    pure next


type PaletteGroup = { name ∷ String, generators ∷ Array PaletteGenerator }

sequentialPaletteGroup :: PaletteGroup
sequentialPaletteGroup =
  { name: "Sequential"
  , generators: sequentialPaletteGenerator
  }

divergingPaletteGroup :: PaletteGroup
divergingPaletteGroup =
  { name: "Diverging"
  , generators: divergingPaletteGenerator
  }

qualitativePaletteGroup :: PaletteGroup
qualitativePaletteGroup =
  { name: "Qualitative"
  , generators: qualitativePaletteGenerator
  }
