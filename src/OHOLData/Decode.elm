module OHOLData.Decode exposing
  ( lives
  , servers
  , arcs
  , objects
  , timeStamp
  )

import OHOLData exposing (Life, Server, Arc, Objects)

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
    |> map2 (|>) (field "chain" int)
    |> map2 (|>) (field "lineage" (oneOf [ int, null 0 ]))
    |> map2 (|>) (field "name" (nullable string))
    |> map2 (|>) (field "server_id" int)
    |> map2 (|>) (field "epoch" int)
    |> map2 (|>) (field "playerid" int)
    |> map2 (|>) (field "age" (oneOf [ float, null 0.0 ]))

servers : Decoder (List Server)
servers =
  field "data" (list server)

server : Decoder Server
server =
  succeed Server
    |> map2 (|>) (field "id" int)
    |> map2 (|>) (field "server_name" string)
    |> map2 (|>) (field "min_time" timeStamp)
    |> map2 (|>) (field "max_time" timeStamp)

arcs : Decoder (List Arc)
arcs =
  list arc

arc : Decoder Arc
arc =
  succeed Arc
    |> map2 (|>) (succeed 17)
    |> map2 (|>) (field "start" timeStamp)
    |> map2 (|>) (field "end" timeStamp)
    |> map2 (|>) (field "seed" int)

objects : Decoder Objects
objects =
  succeed Objects
    |> map2 (|>) (field "ids" value)
    |> map2 (|>) (field "bounds" value)
    |> map2 (|>) (field "spawnChanges" value)

timeStamp : Decoder Posix
timeStamp =
  int
    |> map (\t -> t * 1000)
    |> map Time.millisToPosix
