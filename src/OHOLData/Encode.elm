module OHOLData.Encode exposing
  ( lives
  , life
  , servers
  , server
  , arcs
  , arc
  , spawn
  , generation
  , world
  , worlds
  , timeStamp
  )

import OHOLData exposing (Life, Server, Arc, Spawn, Generation, World)

import Json.Encode exposing (..)
import Time exposing (Posix)

lives : List Life -> Value
lives data =
  object
    [ ("data", list life data)
    ]

life : Life -> Value
life l =
  object
    [ ("birth_x", int l.birthX)
    , ("birth_y", int l.birthY)
    , ("birth_time", timeStamp l.birthTime)
    , ("chain", int l.chain)
    , ("lineage", int l.lineage)
    , ("name", l.name |> Maybe.map string |> Maybe.withDefault null)
    , ("server_id", int l.serverId)
    , ("epoch", int l.epoch)
    , ("playerid", int l.playerid)
    , ("age", float l.age)
    ]

servers : List Server -> Value
servers data =
  object
    [ ("data", list server data)
    ]

server : Server -> Value
server s =
  object
    [ ("id", int s.id)
    , ("server_name", string s.serverName)
    , ("min_time", timeStamp s.minTime)
    , ("max_time", timeStamp s.maxTime)
    ]

arcs : List Arc -> Value
arcs data =
  object
    [ ("data", list arc data)
    ]

arc : Arc -> Value
arc a =
  object
    [ ("server_id", int a.serverId)
    , ("start", timeStamp a.start)
    , ("end", timeStamp a.end)
    , ("seed", int a.seed)
    ]

spawn : Spawn -> Value
spawn s =
  [ [ ("id", int s.id)
    , ("mapChance", float s.mapChance)
    , ("biomes", list int s.biomes)
    , ("moving", bool s.moving)
    , ("wide", bool s.wide)
    , ("leftBlockingRadius", int s.leftBlockingRadius)
    , ("rightBlockingRadius", int s.rightBlockingRadius)
    ]
  , s.gridPlacement
    |> Maybe.map (\x -> [("gridPlacement", int x)])
    |> Maybe.withDefault []
  , s.randPlacement
    |> Maybe.map (\x -> [("randPlacement", int x)])
    |> Maybe.withDefault []
  ]
    |> List.concat
    |> object

generation : Generation -> Value
generation g =
  object
    [ ("allowOffBiomeMovingObjects", bool g.allowOffBiomeMovingObjects)
    , ("biomeMap", list int g.biomeMap)
    , ("tallHeight", int g.tallHeight)
    , ("veryTallHeight", int g.veryTallHeight)
    , ("computeMapBiomeIndex", string g.computeMapBiomeIndex)
    , ("biomeTotalWeight", float g.biomeTotalWeight)
    , ("biomeCumuWeights", list float g.biomeCumuWeights)
    , ("numSpecialBiomes", int g.numSpecialBiomes)
    , ("objects", dict String.fromInt spawn g.objects)
    , ("gridPlacements", list spawn g.gridPlacements)
    , ("randPlacements", list spawn g.randPlacements)
    , ("biomeSeedOffset", int g.biomeSeedOffset)
    ]

worlds : List World -> Value
worlds data =
  object
    [ ("data", list world data)
    ]

world : World -> Value
world w =
  object
    [ ("name", string w.name)
    , ("msStart", msTime w.start)
    , ("msEnd", maybe msTime w.end)
    , ("dataTime", maybe timeStamp w.dataTime)
    , ("biomeLayer", maybe string w.biomeLayer)
    , ("generation", generation w.generation)
    ]

timeStamp : Posix -> Value
timeStamp time =
  time
    |> Time.posixToMillis
    |> (\t -> t // 1000)
    |> int

msTime : Posix -> Value
msTime time =
  time
    |> Time.posixToMillis
    |> int

maybe : (a -> Value) -> Maybe a -> Value
maybe encoder =
  Maybe.map encoder >> Maybe.withDefault null
