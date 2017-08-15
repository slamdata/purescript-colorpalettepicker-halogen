module ColorPalettePicker.Utils.Easing where

import Prelude

import Data.List (List(..), fromFoldable, (:))

type Progresion = Number -> Number
type Generator = Number -> Progresion
type Easing = Number -> Generator

type ProgressionPoint = { at :: Number, val :: Number }

steps :: Array ProgressionPoint -> Easing
steps = fromFoldable >>> steps' 0.0
  where
  steps' :: Number -> List ProgressionPoint -> Easing
  steps' lastAt points start end progress = case points of
    Nil ->
      linear start end $ (progress - lastAt) / (1.0 - lastAt)
    { at, val } : rest | progress <= at ->
      linear start val $ (progress - lastAt) / (at - lastAt)
    { at, val } : rest ->
      steps' at rest val end progress

linear :: Easing
linear start end progress = progress * (end - start) + start

lineTo :: Easing
lineTo = linear

mix :: Number -> Progresion -> Generator
mix fraction progression num p = interpolate fraction (progression p) num

  -- | Linearly interpolate between two values.
interpolate :: Number -> Number -> Number -> Number
interpolate fraction a b = a + fraction * (b - a)
