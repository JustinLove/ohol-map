module OHOLData.Encode exposing
  ( lives
  , life
  , servers
  , server
  , arcs
  , arc
  , spawn
  , biome
  , generation
  , world
  , worlds
  , timeStamp
  )

import OHOLData exposing (Life, Server, Arc, Spawn, Biome, Generation, World)

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
    , ("name", maybe string l.name)
    , ("server_id", int l.serverId)
    , ("epoch", int l.epoch)
    , ("playerid", int l.playerid)
    , ("age", maybe float l.age)
    , ("death_x", maybe int l.deathX)
    , ("death_y", maybe int l.deathY)
    , ("death_time", maybe timeStamp l.deathTime)
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
    , ("seed", maybe int a.seed)
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

biome : Biome -> Value
biome b =
  object
    [ ("id", int b.id)
    , ("objects", list spawn b.objects)
    , ("totalChanceWeight", float b.totalChanceWeight)
    ]

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
    , ("biomes", dict String.fromInt biome g.biomes)
    , ("gridPlacements", list spawn g.gridPlacements)
    , ("randPlacements", list spawn g.randPlacements)
    , ("biomeSeedOffset", maybe int g.biomeSeedOffset)
    , ("randSeed", maybe int g.randSeed)
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
