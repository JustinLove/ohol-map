module Persist.Decode exposing (persist)

import Persist exposing (Persist)
import Theme exposing (Theme)

import Json.Decode exposing (..)

persist : Decoder Persist
persist =
  map2 Persist
    (field "theme" theme)
    (oneOf
      [ (field "showNaturalObjectsAboveZoom" int)
      , (succeed 26)
      ]
    )

theme : Decoder Theme
theme =
  string |> andThen (\str ->
    case Theme.fromString str of
      Just t -> succeed t
      Nothing -> fail "unknown theme"
    )
