module Leaflet.Types exposing
  ( Life
  , Cluster
  , Point
  , PointLocation(..)
  , PointColor(..)
  , Animatable(..)
  )

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
  , accountHash : Maybe String
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

type alias Point =
  { x : Int
  , y : Int
  , z : Int
  }

type PointLocation
  = BirthLocation
  | DeathLocation

type PointColor
  = LineageColor
  | BirthTimeColor
  | ChainColor
  | CauseOfDeathColor
  | AgeColor

type Animatable
  = Static
  | Animated
  | Inert

