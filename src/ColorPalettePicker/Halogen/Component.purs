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
  ( picker
  , Input
  , State
  , PaletteGeneratorGroup
  , PaletteGeneratorGroupF(..)
  , Query(..)
  , Message(..)
  , PaletteGeneratorIdx
  , PaletteGeneratorGroupIdx
  , PickerEffects
  , sequentialPaletteGeneratorGroup
  , divergingPaletteGeneratorGroup
  , qualitativePaletteGeneratorGroup
  )
  where

import Prelude

import CSS as CSS
import Color (Color)
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


type PaletteGeneratorGroup = Exists PaletteGeneratorGroupF
newtype PaletteGeneratorGroupF a = PaletteGeneratorGroupF
  { generators :: NonEmpty Array a
  , name :: String
  , activeIdx :: Int
  , isOpen :: Boolean
  , run :: Color → a → Array Color
  , gradient :: Color → a → Maybe CSS.BackgroundImage
  }

type State =
  { seed :: Color
  , groups :: Array PaletteGeneratorGroup
  }

type Input = Unit

type PaletteGeneratorIdx = Int
type PaletteGeneratorGroupIdx = Int
data Message
  = NewSeed Color
  | PaletteGeneratorSelected PaletteGeneratorGroupIdx PaletteGeneratorIdx
  | PaletteGeneratorGroupToggled PaletteGeneratorGroupIdx

data Query next
  = PickerEvents CPicker.Message next
  | TogglePaletteGeneratorGroup PaletteGeneratorGroupIdx next
  | SelectPaletteGenerator PaletteGeneratorGroupIdx PaletteGeneratorIdx next

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
  , isActive :: H.ClassName
  , paletteGeneratorGroup :: H.ClassName
  , paletteGeneratorGroupHeader :: H.ClassName
  , paletteGeneratorGroupHeaderText :: H.ClassName
  , paletteGeneratorGroupHeaderAction :: H.ClassName
  , paletteGeneratorGroupList :: H.ClassName
  , paletteGenerator :: H.ClassName
  , paletteGeneratorActionable :: H.ClassName
  , hasGradient :: H.ClassName
  , paletteGradient :: H.ClassName
  , palette :: H.ClassName
  , paletteColor :: H.ClassName
  }
classes =
  { root:  HH.ClassName "ColorSchemePicker"
  , paletteGroups: HH.ClassName "ColorPalettePicker-paletteGroups"
  , isActive: HH.ClassName "isActive"
  , paletteGeneratorGroup: HH.ClassName "ColorPalettePicker-paletteGeneratorGroup"
  , paletteGeneratorGroupHeader: HH.ClassName "ColorPalettePicker-paletteGeneratorGroupHeader"
  , paletteGeneratorGroupHeaderText: HH.ClassName "ColorPalettePicker-paletteGeneratorGroupHeaderText"
  , paletteGeneratorGroupHeaderAction: HH.ClassName "ColorPalettePicker-paletteGeneratorGroupHeaderAction"
  , paletteGeneratorGroupList: HH.ClassName "ColorPalettePicker-paletteGeneratorGroupList"
  , paletteGenerator: HH.ClassName "ColorPalettePicker-paletteGenerator"
  , paletteGeneratorActionable: HH.ClassName "ColorPalettePicker-paletteGenerator--actionable"
  , hasGradient: HH.ClassName "hasGradient"
  , paletteGradient: HH.ClassName "ColorPalettePicker-paletteGradient"
  , palette: HH.ClassName "ColorPalettePicker-palette"
  , paletteColor: HH.ClassName "ColorPalettePicker-paletteColor"
  }


picker ∷ ∀ m r. Color -> Array PaletteGeneratorGroup -> MonadAff (PickerEffects r) m => H.Component HH.HTML Query Input Message m
picker seed groups = H.parentComponent
  { initialState: const $
      { seed
      , groups
      }
  , render: render
  , eval: eval
  , receiver: const Nothing
  }

sequentialPaletteGeneratorGroup :: PaletteGeneratorGroup --  SequentialGenerator
sequentialPaletteGeneratorGroup = mkExists $ PaletteGeneratorGroupF
  { name: "Sequential"
  , generators: Palettes.sequentialPaletteGenerators
  , activeIdx: 0
  , isOpen: false
  , run: Palettes.runSequentialGenerator 16
  , gradient: \c p -> Just $ Palettes.sequentialToCSSGradient c p
  }

divergingPaletteGeneratorGroup :: PaletteGeneratorGroup --  DivergingGenerator
divergingPaletteGeneratorGroup = mkExists $ PaletteGeneratorGroupF
  { name: "Diverging"
  , generators: Palettes.divergingPaletteGenerators
  , activeIdx: 0
  , isOpen: false
  , run: Palettes.runDivergingGenerator 27
  , gradient: \c p -> Just $ Palettes.divergingToCSSGradient c p
  }

qualitativePaletteGeneratorGroup :: PaletteGeneratorGroup --  QualitativeGenerator
qualitativePaletteGeneratorGroup = mkExists $ PaletteGeneratorGroupF
  { name: "Qualitative"
  , generators: Palettes.qualitativePaletteGenerators
  , activeIdx: 0
  , isOpen: false
  , run: Palettes.runQualitativeGenerator 12
  , gradient: \_ _ -> Nothing
  }


render ∷ ∀ m r. MonadAff (PickerEffects r) m => State → HTML m
render state = HH.div [HP.class_ $ classes.root] $
  [ HH.slot' cpColor unit (CPicker.picker state.seed) PickerLayout.props $ HE.input PickerEvents
  , HH.div
      [HP.class_ $ classes.paletteGroups] $
        mapWithIndex (\idx -> runExists (renderGroup idx)) state.groups
  ]

  where
  renderGroup ∷ ∀ a. Int -> PaletteGeneratorGroupF a → HTML m
  renderGroup groupIdx (PaletteGeneratorGroupF p@{isOpen, generators, run, gradient, name })=
    HH.div
      [ HP.class_ $ classes.paletteGeneratorGroup]
      [ HH.div
          [ HP.class_ $ classes.paletteGeneratorGroupHeader ]
          [ HH.div
              [ HP.class_ $ classes.paletteGeneratorGroupHeaderText ]
              [ HH.text $ name <> " Palette" ]
          , HH.div
              [ HP.class_ $ classes.paletteGeneratorGroupHeaderAction ]
              [ HH.a
                  [ HP.href "javascript:void(0)"
                  , HE.onClick $ HE.input_ $ TogglePaletteGeneratorGroup groupIdx
                  ]
                  [ HH.text $ if isOpen then "cancel" else "change" ]
              ]
          ]
      , if isOpen then
          HH.div
            [ HP.class_ $ classes.paletteGeneratorGroupList ]
            $ flip mapWithIndex (fromNonEmpty cons generators) \idx generator ->
                renderPalette
                  (activeIdx == idx)
                  (run state.seed generator)
                  (gradient state.seed generator)
                  (SelectPaletteGenerator groupIdx idx)
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
          [ classes.paletteGenerator ]
          <> hasGradientClasses cssGradient
      ]
      $ renderPaletteChildren palette cssGradient

  renderPalette isActive palette cssGradient action =
    HH.a
      [ HP.classes
          $ [ classes.paletteGenerator
            , classes.paletteGeneratorActionable
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
  SelectPaletteGenerator groupId paletteId next -> do
    state <- H.get
    for_ (index state.groups groupId) $ runExists \(PaletteGeneratorGroupF g) ->
      let
        focusedId = case indexNEA g.generators paletteId of
          Nothing -> 0
          Just _ -> paletteId
        newGroup = mkExists $ PaletteGeneratorGroupF g{isOpen = false, activeIdx = focusedId}
        newGroups = updateAt groupId newGroup state.groups
      in do
        H.put state{groups = fromMaybe state.groups newGroups }
        H.raise $ PaletteGeneratorSelected groupId paletteId
    pure next
  TogglePaletteGeneratorGroup groupId next -> do
    state <- H.get
    for_ (index state.groups groupId) $ runExists \(PaletteGeneratorGroupF g) ->
      let
        newGroup = mkExists $ PaletteGeneratorGroupF g{isOpen = not g.isOpen}
        newGroups = updateAt groupId newGroup state.groups
      in do
        H.put state{groups = fromMaybe state.groups newGroups }
        H.raise $ PaletteGeneratorGroupToggled groupId
    pure next
  PickerEvents msg next -> do
    let
      seed = case msg of
        CPicker.NextChange seed -> seed
        CPicker.NotifyChange seed -> seed
    H.modify _{seed = seed}
    H.raise $ NewSeed seed
    pure next
