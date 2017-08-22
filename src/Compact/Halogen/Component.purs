module Compact.Halogen.Component where

import Prelude

import Data.Coyoneda (Coyoneda, liftCoyoneda, unCoyoneda)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM as HQ


data Query innerQuery innerProps innerMsg next
  = HandleInnerMsg innerMsg next
  | SetProps (Props innerProps innerMsg) next
  | Expand next
  | Compact next
  | CheckIsExpanded (Boolean -> next)
  | QueryInner (Coyoneda innerQuery next)


data Message innerMsg
  = Expanded
  | Compacted
  | InnerMessage innerMsg

type State innerProps innerMsg =
  { isExpanded :: Boolean
  , props :: Props innerProps innerMsg
  }

type Props innerProps innerMsg =
  { compactView :: ∀ q i
    . { expand :: i
      }
    -> HH.HTML q i
  , detailView :: ∀ q i
    . { compact :: i
      , embed :: innerProps -> HH.HTML q i
      }
     -> HH.HTML q i
  , shouldCompact :: innerMsg -> Boolean
  }

type Slot = Unit
type HTML innerQuery innerProps innerMsg m =
  H.ParentHTML
  (Query innerQuery innerProps innerMsg)
  innerQuery
  Slot
  m
type DSL innerQuery innerProps innerMsg m =
  H.ParentDSL
  (State innerProps innerMsg)
  (Query innerQuery innerProps innerMsg)
  innerQuery
  Slot
  (Message innerMsg)
  m


-- | Lift a query from the inner component to a query of the HOC. Useful when
-- | querying a component thats "inside" this HOC.
liftQuery :: ∀ innerQuery innerProps innerMsg
  . innerQuery ~> Query innerQuery innerProps innerMsg
liftQuery = QueryInner <<< liftCoyoneda


factory ∷ ∀ innerQuery innerQuery innerProps innerMsg m
  . H.Component
      HH.HTML
      innerQuery
      innerProps
      innerMsg
      m
  → H.Component
      HH.HTML
      (Query innerQuery innerProps innerMsg)
      (Props innerProps innerMsg)
      (Message innerMsg)
      m
factory child = H.parentComponent
  { initialState: { isExpanded: false, props: _ }
  , render: render child
  , eval
  , receiver: HE.input SetProps
  }

render ∷ ∀ innerQuery innerQuery innerProps innerMsg m
  . H.Component
      HH.HTML
      innerQuery
      innerProps
      innerMsg
      m
  → State innerProps innerMsg
  → HTML innerQuery innerProps innerMsg m
render child { isExpanded, props } =
  HK.div_
    [ Tuple "expanded-indicator" $ HH.text (show isExpanded)
    , Tuple (if isExpanded then "inner-exp" else "inner-comp") $
        if isExpanded then
          props.detailView
            { compact: H.action Compact
            , embed: \innerProps ->
                HH.slot unit child innerProps $ HE.input HandleInnerMsg }
        else
          props.compactView
            { expand: H.action Expand }
    ]


eval ∷ ∀ innerQuery innerQuery innerProps innerMsg m
  . Query innerQuery innerProps innerMsg
  ~> DSL innerQuery innerProps innerMsg m
eval = spy >>> case _ of
  CheckIsExpanded request -> do
    H.gets $ _.isExpanded >>> request
  Expand next -> do
    H.modify _{isExpanded = true}
    -- H.raise $ Expanded
    pure next
  Compact next -> do
    H.modify _{isExpanded = false}
    -- H.raise $ Compacted
    pure next
  SetProps props next -> do
    H.modify _{props = props}
    pure next
  QueryInner innerQuery -> innerQuery # unCoyoneda \k q -> do
    result <- H.query unit q
    case result of
      -- TODO avoid halt somehow
      Nothing -> HQ.halt $
        "Inner component Must be mounted in order to query it. " <>
        "This might happen if `embed` is not called `in Props.detailView`"
      Just a -> pure (k a)
  HandleInnerMsg msg next -> do
    H.raise $ InnerMessage msg
    {props} <- H.get
    if props.shouldCompact msg
      then eval $ Compact next
      else pure next
