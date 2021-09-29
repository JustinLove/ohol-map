module Leaflet.Types exposing (Life)

import Time exposing (Posix)

type alias Life =
  { birthX : Int
  , birthY : Int
  , birthTime : Posix
  , chain : Int
  , lineage : Int
  , name : Maybe String
  , serverId : Int
  , epoch : Int
  , playerid : Int
  , age : Maybe Float
  , deathX : Maybe Int
  , deathY : Maybe Int
  , deathTime : Maybe Posix
  , deathCause : Maybe String
  }
