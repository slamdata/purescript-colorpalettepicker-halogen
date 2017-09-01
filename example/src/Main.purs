module Main where

import Prelude

import ColorPalettePicker.Halogen.Component as CPP
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Data.Either.Nested as Either
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main ∷ Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI example unit body

data Query a
  = Query Unit a

type State = {}
type ChildQuery = Coproduct.Coproduct1 CPP.Query
type Slot = Either.Either1 Int


cpPicker ∷ CP.ChildPath CPP.Query ChildQuery Int Slot
cpPicker = CP.cp1


type HTML m = H.ParentHTML Query ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Void m


example ∷ ∀ m r. MonadAff (CPP.PickerEffects r) m => H.Component HH.HTML Query Unit Void m
example = H.parentComponent
    { initialState: const {}
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ ∀ m r. MonadAff (CPP.PickerEffects r) m => State → HTML m
render _ = HH.div_
  [ HH.h1_ [ HH.text "Picker 0" ]
  , HH.slot' cpPicker 0 (CPP.input allGenerators) unit (HE.input Query)
  -- , HH.h1_ [ HH.text "Picker 1" ]
  -- , HH.slot' cpPicker 1 (CPP.input) unit (HE.input Query)
  ]
  where
  allGenerators =
    [ CPP.sequentialPaletteGenerators
    , CPP.divergingPaletteGenerators
    , CPP.qualitativePaletteGenerators
    ]
eval ∷ ∀ m. Query ~> DSL m
eval (Query _ next) = pure next
