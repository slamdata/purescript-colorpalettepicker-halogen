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
-- * http://www.personal.psu.edu/faculty/c/a/cab38/ColorSch/Schemes.html
-- * https://www.youtube.com/watch?v=DjJr8D4Bxjw
-- * https://earthobservatory.nasa.gov/blogs/elegantfigures/2013/08/05/subtleties-of-color-part-1-of-6/
-- * https://betterfigures.org/2015/06/23/picking-a-colour-scale-for-scientific-graphics/
-- * https://github.com/d3/d3-scale-chromatic


module ColorPalettePicker.Halogen.Component
  ( input
  , Input
  , State
  , Query(..)
  , Message(..)
  , PickerEffects
  )
  where

import Prelude

import CSS as CSS
import Color (Color)
import Color as Color
import ColorPalettePicker.Halogen.PickerLayout as PickerLayout
import ColorPalettePicker.Utils.Palettes (DivergingGenerator, QualitativeGenerator, SequentialGenerator, divergingPaletteGenerators, divergingToCSSGradient, qualitativePaletteGenerators, runDivergingGenerator, runQualitativeGenerator, runSequentialGenerator, sequentialPaletteGenerators, sequentialToCSSGradient)
import ColorPicker.Halogen.Component as CPicker
import Control.Monad.Aff.Class (class MonadAff)
import Control.MonadZero (guard)
import Control.Plus (empty)
import Data.Array (cons)
import Data.Either.Nested as Either
import Data.Functor.Coproduct.Nested as Coproduct
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.NonEmpty (NonEmpty, fromNonEmpty, head)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type State =
  { seed :: Color
  , sequential ::
      { idx :: Int
      , generator :: SequentialGenerator
      , isOpen :: Boolean
      }
  , diverging ::
      { idx :: Int
      , generator :: DivergingGenerator
      , isOpen :: Boolean
      }
  , qualitative ::
      { idx :: Int
      , generator :: QualitativeGenerator
      , isOpen :: Boolean
      }
  }
type Input = Unit
type Message = Unit
data Query next
  = PickerEvents CPicker.Message next
  | Update (State -> State) next

type ChildQuery = Coproduct.Coproduct1 CPicker.Query
type Slot = Either.Either1 Unit

cpColor ∷ CP.ChildPath CPicker.Query ChildQuery Unit Slot
cpColor = CP.cp1

type DSL = H.ParentDSL State Query ChildQuery Slot Message
type HTML m = H.ParentHTML Query ChildQuery Slot m

type PickerEffects r = CPicker.PickerEffects r

classes ::
  { root :: H.ClassName
  , paletteGroups :: H.ClassName
  , hasGradient :: H.ClassName
  , isActive :: H.ClassName
  , paletteGradient :: H.ClassName
  , palette :: H.ClassName
  , paletteColor :: H.ClassName
  , paletteGroup :: H.ClassName
  , paletteGroupHeader :: H.ClassName
  , paletteGroupHeaderText :: H.ClassName
  , paletteGroupHeaderAction :: H.ClassName
  , paletteGroupList :: H.ClassName
  , paletteSelected :: H.ClassName
  , paletteGroupItem :: H.ClassName
  , paletteGroupItemAction :: H.ClassName
  }
classes =
  { root:  HH.ClassName "ColorSchemePicker"
  , paletteGroups: HH.ClassName "ColorPalettePicker-paletteGroupes"
  , hasGradient: HH.ClassName "hasGradient"
  , isActive: HH.ClassName "isActive"
  , paletteGradient: HH.ClassName "ColorPalettePicker-paletteGradient"
  , palette: HH.ClassName "ColorPalettePicker-palette"
  , paletteColor: HH.ClassName "ColorPalettePicker-paletteColor"
  , paletteGroup: HH.ClassName "ColorPalettePicker-paletteGroup"
  , paletteGroupHeader: HH.ClassName "ColorPalettePicker-paletteGroupHeader"
  , paletteGroupHeaderText: HH.ClassName "ColorPalettePicker-paletteGroupHeaderText"
  , paletteGroupHeaderAction: HH.ClassName "ColorPalettePicker-paletteGroupHeaderAction"
  , paletteGroupList: HH.ClassName "ColorPalettePicker-paletteGroupList"
  , paletteSelected: HH.ClassName "ColorPalettePicker-paletteSelected"
  , paletteGroupItem: HH.ClassName "ColorPalettePicker-paletteGroupItem"
  , paletteGroupItemAction: HH.ClassName "ColorPalettePicker-paletteGroupItem--action"
  }


input ∷ ∀ m r. MonadAff (PickerEffects r) m => H.Component HH.HTML Query Input Message m
input = H.parentComponent
  { initialState: const $
      { seed: Color.hsl 0.0 1.0 0.5
      , sequential:
          { idx: 0
          , generator: head sequentialPaletteGenerators
          , isOpen: false
          }
      , diverging:
          { idx: 0
          , generator: head divergingPaletteGenerators
          , isOpen: false
          }
      , qualitative:
          { idx: 0
          , generator: head qualitativePaletteGenerators
          , isOpen: false
          }
      }
  , render: render
  , eval: eval
  , receiver: const Nothing
  }

render ∷ ∀ m r. MonadAff (PickerEffects r) m => State → HTML m
render state = HH.div [HP.class_ $ classes.root] $
  [ HH.slot' cpColor unit CPicker.picker PickerLayout.props $ HE.input PickerEvents
  , HH.div
    [HP.class_ $ classes.paletteGroups]
    [ renderGroup
        state.sequential.idx
        state.sequential.isOpen
        state.sequential.generator
        (runSequentialGenerator 16 state.seed)
        (sequentialToCSSGradient state.seed >>> Just)
        (\s -> s{sequential{isOpen = not s.sequential.isOpen}})
        (\idx generator s → s{sequential{ isOpen = false, idx = idx, generator = generator }})
        sequentialPaletteGroup
    , renderGroup
        state.diverging.idx
        state.diverging.isOpen
        state.diverging.generator
        (runDivergingGenerator 27 state.seed)
        (divergingToCSSGradient state.seed >>> Just)
        (\s -> s{diverging{isOpen = not s.diverging.isOpen}})
        (\idx generator s → s{diverging{ isOpen = false, idx = idx, generator = generator }})
        divergingPaletteGroup
    , renderGroup
        state.qualitative.idx
        state.qualitative.isOpen
        state.qualitative.generator
        (runQualitativeGenerator 12 state.seed)
        (const Nothing)
        (\s -> s{qualitative{isOpen = not s.qualitative.isOpen}})
        (\idx generator s → s{qualitative{ isOpen = false, idx = idx, generator = generator }})
        qualitativePaletteGroup
    ]
  ]

  where
  renderGroup ∷ ∀ a
    . Int
    → Boolean
    → a
    → (a → Array Color)
    → (a → Maybe CSS.BackgroundImage)
    → (State → State)
    → (Int → a → State → State)
    → PaletteGroup a
    → HTML m
  renderGroup activeIdx isOpen activeGenerator run gradient toggle update { name, generators } =
    HH.div
      [ HP.class_ $ classes.paletteGroup]
      [ HH.div
          [ HP.class_ $ classes.paletteGroupHeader ]
          [ HH.div
              [ HP.class_ $ classes.paletteGroupHeaderText ]
              [ HH.text $ name <> " Palette" ]
          , HH.div
              [ HP.class_ $ classes.paletteGroupHeaderAction ]
              [ HH.a
                  [ HP.href "javascript:void(0)"
                  , HE.onClick $ HE.input_ $ Update toggle
                  ]
                  [ HH.text $ if isOpen then "cancel" else "change" ]
              ]
          ]
      , if isOpen then
          HH.div
            [ HP.class_ $ classes.paletteGroupList ]
            $ flip mapWithIndex (fromNonEmpty cons generators) \idx generator ->
                renderPalette (activeIdx == idx) (run generator) (gradient generator) (update idx generator)
        else
          renderCompact (run activeGenerator) (gradient activeGenerator)
      ]
  renderCompact palette cssGradient =
    HH.div
      [ HP.classes $
          [ classes.paletteSelected]
          <> hasGradientClasses cssGradient
      ]
      $ renderPaletteChildren palette cssGradient

  renderPalette isActive palette cssGradient update =
    HH.a
      [ HP.classes
          $ [ classes.paletteGroupItem
            , classes.paletteGroupItemAction
            ]
          <> hasGradientClasses cssGradient
          <> isActivetoClasses isActive
      , HE.onClick $  HE.input_ $ Update update
      , HP.href "javascript:void(0)"
      ]
      $ renderPaletteChildren palette cssGradient

  toAlt = maybe empty pure
  hasGradientClasses cssGradient = guard (isJust cssGradient) $> classes.hasGradient
  isActivetoClasses isActive = guard isActive $> classes.isActive
  gradientToHTML cssGradient = toAlt $ cssGradient <#> \img -> HH.div
    [ HP.class_ $ classes.paletteGradient
    , HCSS.style $ CSS.backgroundImage img
    ] []
  renderPaletteChildren palette cssGradient =
    [ HH.div
      [ HP.class_ $ classes.palette ]
      $ palette <#> \color -> HH.div
        [ HCSS.style $ CSS.backgroundColor color
        , HP.class_ $ classes.paletteColor
        ] []
    ] <> gradientToHTML cssGradient




eval ∷ ∀ m . Query ~> DSL m
eval = case _ of
  Update modify next -> do
    H.modify modify
    pure next
  PickerEvents msg next -> do
    case msg of
      CPicker.NextChange seed -> H.modify _{seed = seed}
      CPicker.NotifyChange seed -> H.modify _{seed = seed}
    pure next


type PaletteGroup a = { name ∷ String, generators ∷ NonEmpty Array a }

sequentialPaletteGroup :: PaletteGroup SequentialGenerator
sequentialPaletteGroup =
  { name: "Sequential"
  , generators: sequentialPaletteGenerators
  }

divergingPaletteGroup :: PaletteGroup DivergingGenerator
divergingPaletteGroup =
  { name: "Diverging"
  , generators: divergingPaletteGenerators
  }

qualitativePaletteGroup :: PaletteGroup QualitativeGenerator
qualitativePaletteGroup =
  { name: "Qualitative"
  , generators: qualitativePaletteGenerators
  }
