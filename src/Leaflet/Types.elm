module Leaflet.Types exposing (Life, Cluster)

import Time exposing (Posix)

type alias Life =
  { birthX : Int
  , birthY : Int
  , birthTime : Posix
  , gender : String
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

type alias Cluster =
  { x : Int
  , y : Int
  , members : Int
  }

