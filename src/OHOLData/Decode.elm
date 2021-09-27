module OHOLData.Decode exposing
  ( lives
  , population
  , servers
  , monuments
  , arcs
  , spans
  , objects
  , version
  , timeStamp
  , gridPlacement
  )

import OHOLData exposing (Life, Server, Monument, Arc, Span, Objects, VersionChange, Spawn, SpawnChange(..), GridPlacement)

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
    |> map2 (|>) (field "age" (maybe float))
    |> map2 (|>) (field "death_x" (maybe int))
    |> map2 (|>) (field "death_y" (maybe int))
    |> map2 (|>) (field "death_time" (maybe timeStamp))

population : Decoder (List (Posix, Int, Bool))
population =
  field "data" (map (List.concat>>List.reverse) (list populationLife))

populationLife : Decoder (List (Posix, Int, Bool))
populationLife =
  map2 List.append
    (map2 (\mt mp ->
        case Maybe.map2 (\t p -> (t, p, False)) mt mp of
          Just item -> [item]
          Nothing -> []
      )
      (field "death_time" (maybe timeStamp))
      (field "death_population" (maybe int))
    )
    (map2 (\t p -> [(t, p, True)])
      (field "birth_time" timeStamp)
      (field "birth_population" int)
    )

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

monuments : Decoder (List Monument)
monuments =
  field "data" (list monument)

monument : Decoder Monument
monument =
  succeed Monument
    |> map2 (|>) (field "x" int)
    |> map2 (|>) (field "y" int)
    |> map2 (|>) (field "date" timeStamp)
    |> map2 (|>) (succeed Nothing)

arcs : Decoder (List Arc)
arcs =
  list arc

arc : Decoder Arc
arc =
  succeed Arc
    |> map2 (|>) (succeed 17)
    |> map2 (|>) (field "start" timeStamp)
    |> map2 (|>) (field "end" (nullable timeStamp))
    |> map2 (|>) (field "end" (nullable timeStamp))
    |> map2 (|>) (field "seed" (nullable int))
    |> map2 (|>) (field "seedB" (nullable int))

spans : Decoder (List Span)
spans =
  list span

span : Decoder Span
span =
  succeed Span
    |> map2 (|>) (field "start" timeStamp)
    |> map2 (|>) (field "end" timeStamp)
    |> map2 (|>) (field "base" timeStamp)

objects : Decoder Objects
objects =
  succeed Objects
    |> map2 (|>) (field "ids" value)
    |> map2 (|>) (field "ids" (list objectId))
    |> map2 (|>) (field "names" (list string))
    |> map2 (|>) (field "bounds" value)
    |> map2 (|>) (field "spawnChanges" (list version))

objectId : Decoder Int
objectId =
  string
    |> andThen (\s -> case String.toInt s of
      Just i -> succeed i
      Nothing -> fail "object id is not parsable as an integer"
    )

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
    |> map2 (|>) (succeed False)
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
    |> map2 (|>) (maybe gridPlacement)
    |> map2 (|>) (maybe (field "randPlacement" int))
    |> map (\obj -> {obj | wide = obj.leftBlockingRadius > 0 || obj.rightBlockingRadius > 0})

gridPlacement : Decoder GridPlacement
gridPlacement =
  succeed GridPlacement
    |> map2 (|>) (field "gridPlacement" int)
    |> map2 (|>) (oneOf
      [ (field "gridPlacementY" int)
      , (field "gridPlacement" int)
      ]
    )
    |> map2 (|>) (oneOf
      [ (field "gridPlacementPhaseX" int)
      , succeed 0
      ]
    )
    |> map2 (|>) (oneOf
      [ (field "gridPlacementPhaseY" int)
      , succeed 0
      ]
    )

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
