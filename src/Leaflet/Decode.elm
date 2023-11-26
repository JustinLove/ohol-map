module Leaflet.Decode exposing
  ( lives
  , timeStamp
  )

import Leaflet.Types exposing (Life)

import Json.Decode exposing (..)
import Time exposing (Posix)

lives : Decoder (List Life)
lives =
  field "data" (list life)

life : Decoder Life
life =
  succeed Life
    |> map2 (|>) (field "birth_x" int)
    |> map2 (|>) (field "birth_y" int)
    |> map2 (|>) (field "birth_time" timeStamp)
    |> map2 (|>) (field "gender" string)
    |> map2 (|>) (field "chain" int)
    |> map2 (|>) (field "lineage" (oneOf [ int, null 0 ]))
    |> map2 (|>) (field "name" (nullable string))
    |> map2 (|>) (field "server_id" int)
    |> map2 (|>) (field "epoch" int)
    |> map2 (|>) (field "playerid" int)
    |> map2 (|>) (field "account_hash" (maybe string))
    |> map2 (|>) (field "age" (maybe float))
    |> map2 (|>) (field "death_x" (maybe int))
    |> map2 (|>) (field "death_y" (maybe int))
    |> map2 (|>) (field "death_time" (maybe timeStamp))
    |> map2 (|>) (field "cause" (maybe string))

timeStamp : Decoder Posix
timeStamp =
  int
    |> map (\t -> t * 1000)
    |> map Time.millisToPosix
