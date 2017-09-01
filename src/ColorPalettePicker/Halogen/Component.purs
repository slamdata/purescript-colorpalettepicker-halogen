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
  -- ( input
  -- , Input
  -- , State
  -- , PaletteGenerators
  -- , PaletteGeneratorsF(..)
  -- , Query(..)
  -- , Message(..)
  -- , PickerEffects
  -- )
  where

import Prelude

import CSS as CSS
import Color (Color)
import Color as Color
import ColorPalettePicker.Halogen.PickerLayout as PickerLayout
import ColorPalettePicker.Utils.Palettes as Palettes
import ColorPicker.Halogen.Component as CPicker
import Control.Monad.Aff.Class (class MonadAff)
import Control.MonadZero (guard)
import Control.Plus (empty)
import Data.Array (cons, index, updateAt)
import Data.Either.Nested as Either
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (for_)
import Data.Functor.Coproduct.Nested as Coproduct
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.NonEmpty (NonEmpty(..), fromNonEmpty, head)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type PaletteGenerators = Exists PaletteGeneratorsF
newtype PaletteGeneratorsF a = PaletteGeneratorsF
  { generators :: NonEmpty Array a
  , name :: String
  , activeIdx :: Int
  , isOpen :: Boolean
  , run :: Color → a → Array Color
  , gradient :: Color → a → Maybe CSS.BackgroundImage
  }

type State =
  { seed :: Color
  , group :: Array PaletteGenerators
  }

type Input = Unit
type Message = Unit
data Query next
  = PickerEvents CPicker.Message next
  | Update (State -> State) next
  | ToggleGroup Int next
  | SelectPalette Int Int next

type ChildQuery = Coproduct.Coproduct1 CPicker.Query
type Slot = Either.Either1 Unit

cpColor ∷ CP.ChildPath CPicker.Query ChildQuery Unit Slot
cpColor = CP.cp1

type DSL = H.ParentDSL State Query ChildQuery Slot Message
type HTML m = H.ParentHTML Query ChildQuery Slot m

type PickerEffects r = CPicker.PickerEffects r

classes ::
  { root :: H.ClassName
  , paletteGroup :: H.ClassName
  , hasGradient :: H.ClassName
  , isActive :: H.ClassName
  , paletteGradient :: H.ClassName
  , palette :: H.ClassName
  , paletteColor :: H.ClassName
  , paletteGenerators :: H.ClassName
  , paletteGeneratorsHeader :: H.ClassName
  , paletteGeneratorsHeaderText :: H.ClassName
  , paletteGeneratorsHeaderAction :: H.ClassName
  , paletteGeneratorsList :: H.ClassName
  , paletteSelected :: H.ClassName
  , paletteGenerator :: H.ClassName
  , paletteGeneratorAction :: H.ClassName
  }
classes =
  { root:  HH.ClassName "ColorSchemePicker"
  , paletteGroup: HH.ClassName "ColorPalettePicker-paletteGroup"
  , hasGradient: HH.ClassName "hasGradient"
  , isActive: HH.ClassName "isActive"
  , paletteGradient: HH.ClassName "ColorPalettePicker-paletteGradient"
  , palette: HH.ClassName "ColorPalettePicker-palette"
  , paletteColor: HH.ClassName "ColorPalettePicker-paletteColor"
  , paletteGenerators: HH.ClassName "ColorPalettePicker-paletteGenerators"
  , paletteGeneratorsHeader: HH.ClassName "ColorPalettePicker-paletteGeneratorsHeader"
  , paletteGeneratorsHeaderText: HH.ClassName "ColorPalettePicker-paletteGeneratorsHeaderText"
  , paletteGeneratorsHeaderAction: HH.ClassName "ColorPalettePicker-paletteGeneratorsHeaderAction"
  , paletteGeneratorsList: HH.ClassName "ColorPalettePicker-paletteGeneratorsList"
  , paletteSelected: HH.ClassName "ColorPalettePicker-paletteSelected"
  , paletteGenerator: HH.ClassName "ColorPalettePicker-paletteGenerator"
  , paletteGeneratorAction: HH.ClassName "ColorPalettePicker-paletteGenerator--action"
  }


input ∷ ∀ m r. Array PaletteGenerators -> MonadAff (PickerEffects r) m => H.Component HH.HTML Query Input Message m
input group = H.parentComponent
  { initialState: const $
      { seed: Color.hsl 0.0 1.0 0.5
      , group
      }
  , render: render
  , eval: eval
  , receiver: const Nothing
  }

sequentialPaletteGenerators :: PaletteGenerators --  SequentialGenerator
sequentialPaletteGenerators = mkExists $ PaletteGeneratorsF
  { name: "Sequential"
  , generators: Palettes.sequentialPaletteGenerators
  , activeIdx: 0
  , isOpen: false
  , run: Palettes.runSequentialGenerator 16
  , gradient: \c p -> Just $ Palettes.sequentialToCSSGradient c p
  }

divergingPaletteGenerators :: PaletteGenerators --  DivergingGenerator
divergingPaletteGenerators = mkExists $ PaletteGeneratorsF
  { name: "Diverging"
  , generators: Palettes.divergingPaletteGenerators
  , activeIdx: 0
  , isOpen: false
  , run: Palettes.runDivergingGenerator 27
  , gradient: \c p -> Just $ Palettes.divergingToCSSGradient c p
  }

qualitativePaletteGenerators :: PaletteGenerators --  QualitativeGenerator
qualitativePaletteGenerators = mkExists $ PaletteGeneratorsF
  { name: "Qualitative"
  , generators: Palettes.qualitativePaletteGenerators
  , activeIdx: 0
  , isOpen: false
  , run: Palettes.runQualitativeGenerator 12
  , gradient: \_ _ -> Nothing
  }


render ∷ ∀ m r. MonadAff (PickerEffects r) m => State → HTML m
render state = HH.div [HP.class_ $ classes.root] $
  [ HH.slot' cpColor unit CPicker.picker PickerLayout.props $ HE.input PickerEvents
  , HH.div
      [HP.class_ $ classes.paletteGroup] $
        mapWithIndex (\idx -> runExists (renderGroup idx)) state.group
  ]

  where
  renderGroup ∷ ∀ a. Int -> PaletteGeneratorsF a → HTML m
  renderGroup groupIdx (PaletteGeneratorsF p@{isOpen, generators, run, gradient, name })=
    HH.div
      [ HP.class_ $ classes.paletteGenerators]
      [ HH.div
          [ HP.class_ $ classes.paletteGeneratorsHeader ]
          [ HH.div
              [ HP.class_ $ classes.paletteGeneratorsHeaderText ]
              [ HH.text $ name <> " Palette" ]
          , HH.div
              [ HP.class_ $ classes.paletteGeneratorsHeaderAction ]
              [ HH.a
                  [ HP.href "javascript:void(0)"
                  , HE.onClick $ HE.input_ $ ToggleGroup groupIdx
                  ]
                  [ HH.text $ if isOpen then "cancel" else "change" ]
              ]
          ]
      , if isOpen then
          HH.div
            [ HP.class_ $ classes.paletteGeneratorsList ]
            $ flip mapWithIndex (fromNonEmpty cons generators) \idx generator ->
                renderPalette
                  (activeIdx == idx)
                  (run state.seed generator)
                  (gradient state.seed generator)
                  (SelectPalette groupIdx idx)
        else
          renderCompact (run state.seed activeGenerator) (gradient state.seed activeGenerator)
      ]
    where
    activeGenerator' = indexNEA generators p.activeIdx
    activeGenerator = fromMaybe (head generators) activeGenerator'
    activeIdx = maybe 0 (const p.activeIdx) activeGenerator'
  renderCompact palette cssGradient =
    HH.div
      [ HP.classes $
          [ classes.paletteSelected]
          <> hasGradientClasses cssGradient
      ]
      $ renderPaletteChildren palette cssGradient

  renderPalette isActive palette cssGradient action =
    HH.a
      [ HP.classes
          $ [ classes.paletteGenerator
            , classes.paletteGeneratorAction
            ]
          <> hasGradientClasses cssGradient
          <> isActivetoClasses isActive
      , HE.onClick $ HE.input_ $ action
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

indexNEA :: ∀ a. NonEmpty Array a -> Int -> Maybe a
indexNEA (NonEmpty val arr) 0 = pure val
indexNEA (NonEmpty val arr) idx = index arr (idx - 1)


eval ∷ ∀ m . Query ~> DSL m
eval = case _ of
  SelectPalette groupId paletteId next -> do
    state <- H.get
    for_ (index state.group groupId) $ runExists \(PaletteGeneratorsF g) ->
      let
        focusedId = case indexNEA g.generators paletteId of
          Nothing -> 0
          Just _ -> paletteId
        newGroup = mkExists $ PaletteGeneratorsF g{isOpen = false, activeIdx = focusedId}
        newGroups = updateAt groupId newGroup state.group
      in H.put state{group = fromMaybe state.group newGroups }

    pure next
  ToggleGroup groupId next -> do
    state <- H.get
    for_ (index state.group groupId) $ runExists \(PaletteGeneratorsF g) ->
      let
        newGroup = mkExists $ PaletteGeneratorsF g{isOpen = not g.isOpen}
        newGroups = updateAt groupId newGroup state.group
      in H.put state{group = fromMaybe state.group newGroups }
    pure next
  Update modify next -> do
    H.modify modify
    pure next
  PickerEvents msg next -> do
    case msg of
      CPicker.NextChange seed -> H.modify _{seed = seed}
      CPicker.NotifyChange seed -> H.modify _{seed = seed}
    pure next
