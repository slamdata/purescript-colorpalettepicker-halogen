module ColorPalettePicker.Halogen.Component
  ( input
  , Query(..)
  , Message(..)
  )
  where

import Prelude

import CSS as CSS
import Color (Color)
import Color as Color
import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import ColorPalettePicker.Utils.Easing (steps, lineTo, Progresion)
import Partial.Unsafe (unsafePartial)


type State = {
}

type Message = Unit
data Query next = Query next

type DSL = H.ComponentDSL State Query Message
type HTML = H.ComponentHTML Query


input ∷ ∀ m. H.Component HH.HTML Query Unit Message m
input = H.component
  { initialState: const $ {}
  , render: render
  , eval: eval
  , receiver: const Nothing
  }

render ∷ State → HTML
render state = HH.div_ $
  [ HH.div_ $ paletes <#> \(Tuple name { gen, colors, count }) -> HH.div_
    [ HH.h1_ [HH.text name]
    , HH.div [HP.class_ $ HH.ClassName $ "lst"]
      [ HH.div_ $ renderColors colors
      , HH.div [HP.class_ $ HH.ClassName $ "gen-" <> show count]
        $ (renderColors $ gen (unsafePartial $ fromJust $ Array.last colors) count)
      ]
    ]
  ]
  where
  renderColors = map \color -> HH.div
    [ HCSS.style do
        CSS.backgroundColor color
        -- CSS.width $ CSS.pct (Color.toHSLA color # \{h} -> h / 360.0 * 50.0 + 50.0)
        -- CSS.width $ CSS.pct (Color.toHSLA color # \{s} -> s * 50.0 + 50.0)
        -- CSS.width $ CSS.pct (Color.toHSLA color # \{l} -> l * 50.0 + 50.0)
    ] [HH.text $ Color.cssStringHSLA color]

eval ∷ ∀ m . Query ~> DSL m
eval = case _ of
  Query next -> pure next


hueRange :: (Number -> Number) -> Number -> Progresion
hueRange f start progress = start `lineTo` (f start) $ progress

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


paletes ::
  Array
    (Tuple
      String
      { gen :: Color -> Int -> Array Color
      , colors :: Array Color
      , count :: Int
      }
    )
paletes =
  [ Tuple "sequential: blue green" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.30, val: h - 5.0 }]` (h + 50.0)
      , s: \_ -> 1.00 `steps [{ at: 0.15, val: 0.95 }, { at: 0.30, val: 0.60 }, { at: 0.50, val: 0.40 }]` 0.60
      , l: \_ -> 0.15 `lineTo` 0.97
      }
    , colors: mkColors [ "#f7fcfd" , "#e5f5f9" , "#ccece6" , "#99d8c9" , "#66c2a4" , "#41ae76" , "#238b45" , "#006d2c" , "#00441b" ]
    }
  , Tuple "sequential: blue purple" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.40, val: h - 20.0 }, { at: 0.50, val: h - 60.0 }]` (h - 110.0)
      , s: \_ -> 1.0 `steps [{ at: 0.40, val: 0.30 }]` 0.60
      , l: \_ -> 0.15 `lineTo` 0.97
      }
    , colors: mkColors [ "#f7fcfd" , "#e0ecf4" , "#bfd3e6" , "#9ebcda" , "#8c96c6" , "#8c6bb1" , "#88419d" , "#810f7c" , "#4d004b" ]
    }
  , Tuple "sequential: green blue" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.45, val: h - 10.0 }, { at: 0.75, val: h - 100.0 }]` (h - 120.0)
      , s: \_ -> 0.85 `steps [{ at: 0.1, val: 0.90 }, { at: 0.3, val: 0.70 }, { at: 0.5, val: 0.45 }]` 0.60
      , l: \_ -> 0.27 `lineTo` 0.96
      }
    , colors: mkColors [ "#f7fcf0" , "#e0f3db" , "#ccebc5" , "#a8ddb5" , "#7bccc4" , "#4eb3d3" , "#2b8cbe" , "#0868ac" , "#084081" ]
    }
  , Tuple "sequential: orange red" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.40, val: h + 5.0 }, { at: 0.70, val: h + 30.0 }]` (h + 35.0)
      , s: \_ -> 1.0 `steps [{ at: 0.15, val: 0.97 }, { at: 0.25, val: 0.7 }, { at: 0.40, val: 0.95 }]` 1.0
      , l: \_ -> 0.25 `lineTo` 0.97
      }
    , colors: mkColors [ "#fff7ec" , "#fee8c8" , "#fdd49e" , "#fdbb84" , "#fc8d59" , "#ef6548" , "#d7301f" , "#b30000" , "#7f0000" ]
    }
  , Tuple "sequential: purple blue" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.50, val: h + 5.0 }, { at: 0.75, val: h + 40.0 }]` (h + 130.0)
      , s: \_ -> 0.95 `steps [{ at: 0.25, val: 0.90 }, { at: 0.35, val: 0.55 }]` 0.3
      , l: \_ -> 0.20 `lineTo` 0.95
      }
    , colors: mkColors [ "#fff7fb" , "#ece7f2" , "#d0d1e6" , "#a6bddb" , "#74a9cf" , "#3690c0" , "#0570b0" , "#045a8d" , "#023858" ]
    }
  , Tuple "sequential: purple blue green" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.5, val: h + 30.0 }, { at: 0.75, val: h + 70.0 }]` (h + 160.0)
      , s: \_ -> 0.95 `steps [{ at: 0.25, val: 0.97 }, { at: 0.35, val: 0.55 }]` 0.4
      , l: \_ -> 0.15 `steps [{ at: 0.25, val: 0.3 }, { at: 0.6, val: 0.75 }]` 0.98
      }
    , colors: mkColors [ "#fff7fb" , "#ece2f0" , "#d0d1e6" , "#a6bddb" , "#67a9cf" , "#3690c0" , "#02818a" , "#016c59" , "#014636" ]
    }
  , Tuple "sequential: purple red" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ - 60.0)
      , s: \_ -> 1.0 `steps [{ at: 0.15, val: 0.95 }, { at: 0.45, val: 0.80 }, { at: 0.55, val: 0.35 }]` 0.3
      , l: \_ -> 0.20 `lineTo` 0.97
      }
    , colors: mkColors [ "#f7f4f9" , "#e7e1ef" , "#d4b9da" , "#c994c7" , "#df65b0" , "#e7298a" , "#ce1256" , "#980043" , "#67001f" ]
    }
  , Tuple "sequential: red purple" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.6, val: h + 60.0 }]` (h + 85.0)
      , s: \_ -> 1.0 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.70 }, { at: 0.55, val: 0.85 }]` 0.9
      , l: \_ -> 0.20 `steps [{ at: 0.25, val: 0.35 }, { at: 0.6, val: 0.75 }]` 0.97
      }
    , colors: mkColors [ "#fff7f3" , "#fde0dd" , "#fcc5c0" , "#fa9fb5" , "#f768a1" , "#dd3497" , "#ae017e" , "#7a0177" , "#49006a" ]
    }
  , Tuple "sequential: yellow green" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.4, val: h - 25.0 }, { at: 0.6, val: h - 60.0 }]` (h - 95.0)
      , s: \_ -> 1.0 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.40 }]` 0.95
      , l: \_ -> 0.15 `steps [{ at: 0.25, val: 0.3 }, { at: 0.6, val: 0.7 }]` 0.95
      }
    , colors: mkColors [ "#ffffe5" , "#f7fcb9" , "#d9f0a3" , "#addd8e" , "#78c679" , "#41ab5d" , "#238443" , "#006837" , "#004529" ]
    }
  , Tuple "sequential: yellow green blue" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.3, val: h - 25.0 }, { at: 0.6, val: h - 50.0 }, { at: 0.7, val: h - 120.0 }]` (h - 165.0)
      , s: \_ -> 0.85 `steps [{ at: 0.10, val: 0.6 }, { at: 0.35, val: 0.75 }, { at: 0.60, val: 0.40 }]` 0.95
      , l: \_ -> 0.20 `steps [{ at: 0.10, val: 0.4 }, { at: 0.5, val: 0.5 }]` 0.95
      }
    , colors: mkColors [ "#ffffd9" , "#edf8b1" , "#c7e9b4" , "#7fcdbb" , "#41b6c4" , "#1d91c0" , "#225ea8" , "#253494" , "#081d58" ]
    }
  , Tuple "sequential: yellow orange brown" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ + 40.0)
      , s: \_ -> 0.85 `lineTo` 1.0
      , l: \_ -> 0.20 `steps [{ at: 0.25, val: 0.45 }]` 0.95
      }
    , colors: mkColors [ "#ffffe5" , "#fff7bc" , "#fee391" , "#fec44f" , "#fe9929" , "#ec7014" , "#cc4c02" , "#993404" , "#662506" ]
    }
  , Tuple "sequential: yellow orange red" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ + 80.0)
      , s: \_ -> 1.0 `steps [{ at: 0.25, val: 0.8 }, { at: 0.35, val: 0.9 }]` 1.0
      , l: \_ -> 0.25 `steps [{ at: 0.25, val: 0.55 }]` 1.0
      }
    , colors: mkColors [ "#ffffcc" , "#ffeda0" , "#fed976" , "#feb24c" , "#fd8d3c" , "#fc4e2a" , "#e31a1c" , "#bd0026" , "#800026" ]
    }
  , Tuple "sequential: blues" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.5, val: h - 15.0 }]` (h - 5.0)
      , s: \_ -> 0.85 `steps [{ at: 0.25, val: 0.95 }, { at: 0.45, val: 0.55 }]` 0.65
      , l: \_ -> 0.25 `lineTo` 0.97
      }
    , colors: mkColors [ "#f7fbff" , "#deebf7" , "#c6dbef" , "#9ecae1" , "#6baed6" , "#4292c6" , "#2171b5" , "#08519c" , "#08306b" ]
    }
  , Tuple "sequential: greens" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ - 40.0)
      , s: \_ -> 0.85 `steps [{ at: 0.15, val: 0.95 }, { at: 0.40, val: 0.4 }]` 0.5
      , l: \_ -> 0.15 `lineTo` 0.97
      }
    , colors: mkColors [ "#f7fcf5" , "#e5f5e0" , "#c7e9c0" , "#a1d99b" , "#74c476" , "#41ab5d" , "#238b45" , "#006d2c" , "#00441b" ]
    }
  , Tuple "sequential: greys" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ + 0.0)
      , s: \_ -> 0.0 `lineTo` 0.0
      , l: \_ -> 0.0 `lineTo` 1.0
      }
    , colors: mkColors [ "#ffffff" , "#f0f0f0" , "#d9d9d9" , "#bdbdbd" , "#969696" , "#737373" , "#525252" , "#252525" , "#000000" ]
    }
  , Tuple "sequential: oranges" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ + 10.0)
      , s: \_ -> 0.90 `lineTo` 1.0
      , l: \_ -> 0.25 `lineTo` 0.95
      }
    , colors: mkColors [ "#fff5eb" , "#fee6ce" , "#fdd0a2" , "#fdae6b" , "#fd8d3c" , "#f16913" , "#d94801" , "#a63603" , "#7f2704" ]
    }
  , Tuple "sequential: purples" $
    { count: 9
    , gen: mkGenerator
      { h: \h -> h `steps [{ at: 0.5, val: h - 25.0 }, { at: 0.75, val: h - 30.0 }]` h
      , s: \_ -> 0.90 `steps [{ at: 0.35, val: 0.3 }]` 0.35
      , l: \_ -> 0.25 `lineTo` 0.95
      }
    , colors: mkColors [ "#fcfbfd" , "#efedf5" , "#dadaeb" , "#bcbddc" , "#9e9ac8" , "#807dba" , "#6a51a3" , "#54278f" , "#3f007d" ]
    }
  , Tuple "sequential: reds" $
    { count: 9
    , gen: mkGenerator
      { h: hueRange (_ + 30.0)
      , s: \_ -> 1.00 `steps [{ at: 0.30, val: 0.8 }]` 1.0
      , l: \_ -> 0.20 `lineTo` 0.95
      }
    , colors: mkColors [ "#fff5f0" , "#fee0d2" , "#fcbba1" , "#fc9272" , "#fb6a4a" , "#ef3b2c" , "#cb181d" , "#a50f15" , "#67000d" ]
    }
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
  ]
  where
  mkColors strs = foldMap (maybe [] pure <<< Color.fromHexString) strs
