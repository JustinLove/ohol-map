module Persist.Decode exposing (persist)

import Persist exposing (Persist)
import Theme exposing (Theme)

import Json.Decode exposing (..)

persist : Decoder Persist
persist =
  map3 Persist
    (field "theme" theme)
    (oneOf
      [ (field "showNaturalObjectsAboveZoom" int)
      , (succeed 26)
      ]
    )
    (oneOf
      [ (field "showActivityMapBelowZoom" int)
      , (succeed 24)
      ]
    )

theme : Decoder Theme
theme =
  string |> andThen (\str ->
    case Theme.fromString str of
      Just t -> succeed t
      Nothing -> fail "unknown theme"
    )
