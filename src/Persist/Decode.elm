module Persist.Decode exposing (persist)

import Persist exposing (Persist)
import Theme exposing (Theme)

import Json.Decode exposing (..)

persist : Decoder Persist
persist =
  map Persist
    (field "theme" theme)

theme : Decoder Theme
theme =
  string |> andThen (\str ->
    case Theme.fromString str of
      Just t -> succeed t
      Nothing -> fail "unknown theme"
    )
