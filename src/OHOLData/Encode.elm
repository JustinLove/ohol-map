module OHOLData.Encode exposing
  ( lives
  , life
  , servers
  , server
  , arcs
  , arc
  , timeStamp
  )

import OHOLData exposing (Life, Server, Arc)

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
arc s =
  object
    [ ("server_id", int s.serverId)
    , ("start", timeStamp s.start)
    , ("end", timeStamp s.end)
    , ("seed", int s.seed)
    ]

timeStamp : Posix -> Value
timeStamp time =
  time
    |> Time.posixToMillis
    |> (\t -> t // 1000)
    |> int
