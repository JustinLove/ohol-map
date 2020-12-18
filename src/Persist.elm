module Persist exposing (Persist)

import Theme

type alias Persist =
  { theme : Theme.Theme
  , showNaturalObjectsAboveZoom : Int
  , showActivityMapBelowZoom : Int
  }
