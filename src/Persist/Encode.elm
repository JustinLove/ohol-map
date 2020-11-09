module Persist.Encode exposing (persist)

import Persist exposing (Persist)
import Theme exposing (Theme)

import Json.Encode exposing (..)

persist : Persist -> Value
persist p =
  object
    [ ("theme", theme p.theme)
    ]

theme : Theme -> Value
theme t =
  t |> Theme.toString |> string
