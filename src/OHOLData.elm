module OHOLData exposing (Life, Server)

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
  , age : Float
  }

type alias Server =
  { id : Int
  , serverName : String
  }
