module ColorPalettePicker.Halogen.PickerLayout
  ( props
  )
  where

import Prelude

import ColorPicker.Halogen.Component as CPicker
import ColorPicker.Halogen.Layout as L
import Halogen as H

props ∷ CPicker.Props
props =
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
                  , L.Component $ L.componentHEX inputClasses
                  ]
              , [ H.ClassName "ColorPicker-editingItem" ] `L.Group`
                  [ L.Component $ L.componentRed inputClasses
                  , L.Component $ L.componentGreen inputClasses
                  , L.Component $ L.componentBlue inputClasses
                  , [ H.ClassName "ColorPicker-actions" ] `L.Group`
                      [ L.Component $ L.componentSet [ H.ClassName "ColorPicker-actionSet" ] ]
                  ]
              ]
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
