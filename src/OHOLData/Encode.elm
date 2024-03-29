module OHOLData.Encode exposing
  ( servers
  , server
  , monuments
  , monument
  , arcs
  , arc
  , spans
  , span
  , spawn
  , biome
  , generation
  , world
  , worlds
  , timeStamp
  , msTime
  )

import OHOLData exposing (Server, Monument, Arc, Span, Spawn, GridPlacement, Biome, Generation, SecondPlaceBiomeObjects(..), World)

import Json.Encode exposing (..)
import Time exposing (Posix)

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

monuments : List Monument -> Value
monuments data =
  object
    [ ("data", list monument data)
    ]

monument : Monument -> Value
monument m =
  object
    [ ("x", int m.x)
    , ("y", int m.y)
    , ("date", timeStamp m.date)
    , ("end", maybe timeStamp m.end)
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
    , ("end", maybe timeStamp a.end)
    , ("seed", maybe int a.seedA)
    , ("seedB", maybe int a.seedB)
    ]

spans : List Span -> Value
spans data =
  object
    [ ("data", list span data)
    ]

span : Span -> Value
span s =
  object
    [ ("msStart", msTime s.start)
    , ("msEnd", msTime s.end)
    , ("dataTime", timeStamp s.end)
    , ("base", timeStamp s.base)
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
    |> Maybe.map (\x -> [("gridPlacement", gridPlacment x)])
    |> Maybe.withDefault []
  , s.randPlacement
    |> Maybe.map (\x -> [("randPlacement", int x)])
    |> Maybe.withDefault []
  ]
    |> List.concat
    |> object

gridPlacment : GridPlacement -> Value
gridPlacment g =
  object
    [ ("spacingX", int g.spacingX)
    , ("spacingY", int g.spacingY)
    , ("phaseX", int g.phaseX)
    , ("phaseY", int g.phaseY)
    ]

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
    [ ("secondPlaceBiomeObjects", secondPlaceBiomeObjects g.secondPlaceBiomeObjects)
    , ("biomeMap", list int g.biomeMap)
    , ("tallHeight", int g.tallHeight)
    , ("veryTallHeight", int g.veryTallHeight)
    , ("computeMapBiomeIndex", string g.computeMapBiomeIndex)
    , ("biomeTotalWeight", float g.biomeTotalWeight)
    , ("biomeCumuWeights", list float g.biomeCumuWeights)
    , ("numSpecialBiomes", int g.numSpecialBiomes)
    , ("specialBiomeBandThickness", int g.specialBiomeBandThickness)
    , ("specialBiomeBandOrder", list int g.specialBiomeBandOrder)
    , ("specialBiomeBandYCenter", list int g.specialBiomeBandYCenter)
    , ("specialBiomeBandDefault", int g.specialBiomeBandDefault)
    , ("objects", dict String.fromInt spawn g.objects)
    , ("biomes", dict String.fromInt biome g.biomes)
    , ("gridPlacements", list spawn g.gridPlacements)
    , ("randPlacements", list spawn g.randPlacements)
    , ("biomeRandSeedA", maybe int g.biomeRandSeedA)
    , ("biomeRandSeedB", maybe int g.biomeRandSeedB)
    , ("randSeed", maybe int g.randSeed)
    , ("biomeOffset", float g.biomeOffset)
    , ("biomeScale", float g.biomeScale)
    , ("biomeFractalRoughness", float g.biomeFractalRoughness)
    ]

secondPlaceBiomeObjects : SecondPlaceBiomeObjects -> Value
secondPlaceBiomeObjects second =
  case second of
    SecondPlaceObjects -> string "SecondPlaceObjects"
    SecondPlaceStaticObjects -> string "SecondPlaceStaticObjects"
    NoMovingObjects -> string "NoMovingObjects"
    NoSecondPlace -> string "NoSecondPlace"

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
    , ("spans", list span w.spans)
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
