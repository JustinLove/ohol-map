module OHOLData.Decode exposing
  ( lives
  , servers
  , arcs
  , objects
  , version
  , timeStamp
  )

import OHOLData exposing (Life, Server, Arc, Objects, VersionChange, Spawn, SpawnChange(..))

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
    |> map2 (|>) (field "spawnChanges" (list version))

version : Decoder VersionChange
version =
  succeed VersionChange
    |> map2 (|>) (field "id" int)
    |> map2 (|>) (field "date" dateTime)
    |> map2 (|>) (field "spawnChanges" (list spawnChange))

spawnChange : Decoder SpawnChange
spawnChange =
  oneOf
    [ removed |> map Removed
    , spawn |> map Changed
    ]

removed : Decoder Int
removed =
  (field "removed" bool)
    |> andThen (\r ->
      if r then
        (field "id" id)
      else
        fail "not a removal"
      )

spawn : Decoder Spawn
spawn =
  succeed Spawn
    |> map2 (|>) (field "id" id)
    |> map2 (|>) (field "mapChance" float)
    |> map2 (|>) (field "biomes" (list int))
    |> map2 (|>) (oneOf
      [ (field "moving" bool)
      , succeed False
      ]
    )
    |> map2 (|>) (oneOf
      [ (field "leftBlockingRadius" int)
      , succeed 0
      ]
    )
    |> map2 (|>) (oneOf
      [ (field "rightBlockingRadius" int)
      , succeed 0
      ]
    )
    |> map2 (|>) (maybe (field "gridPlacement" int))
    |> map2 (|>) (maybe (field "randPlacement" int))

id : Decoder Int
id =
  string
    |> map String.toInt
    |> andThen (\mid -> mid
      |> Maybe.map succeed
      |> Maybe.withDefault (fail "bad id")
    )

dateTime : Decoder Posix
dateTime =
  int
    |> map Time.millisToPosix

timeStamp : Decoder Posix
timeStamp =
  int
    |> map (\t -> t * 1000)
    |> map Time.millisToPosix
