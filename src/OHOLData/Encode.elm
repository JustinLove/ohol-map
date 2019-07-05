module OHOLData.Encode exposing
  ( lives
  , life
  , servers
  , server
  )

import OHOLData exposing (Life, Server)

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
    ]

timeStamp : Posix -> Value
timeStamp time =
  time
    |> Time.posixToMillis
    |> (\t -> t // 1000)
    |> int
