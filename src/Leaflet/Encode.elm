module Leaflet.Encode exposing
  ( lives
  , life
  , timeStamp
  , msTime
  )

import Leaflet.Types exposing (Life)

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
    , ("cause", maybe string l.deathCause)
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
