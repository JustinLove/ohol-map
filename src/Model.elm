module Model exposing
  ( Arc
  , Config
  , Life
  , Mode(..)
  , Model
  , Monument
  , Notice(..)
  , Player(..)
  , Server
  , Span
  , TimeMode(..)
  , Version
  , World
  )

import Leaflet exposing (Point, PointColor(..), PointLocation(..))
import OHOLData as Data
import RemoteData exposing (RemoteData(..))

import Browser.Navigation as Navigation
import Dict exposing(Dict)
import Http
import Time exposing (Posix)
import Url exposing (Url)

type alias Arc = Data.Arc
type alias Monument = Data.Monument
type alias Span = Data.Span
type alias Version = Data.Version
type alias World = Data.World

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , zone : Time.Zone
  , time : Posix
  , notice : Notice
  , center : Point
  , cachedApiUrl : String
  , apiUrl : String
  , lineageUrl: String
  , seedsUrl: String
  , spansUrl: String
  , sidebarOpen : Bool
  , sidebarMode : Mode
  , searchTerm : String
  , timeMode : TimeMode
  , coarseStartTime : Posix
  , startTime : Posix
  , mapTime : Maybe Posix
  , hoursPeriod : Int
  , coarseArc : Maybe Arc
  , currentArc : Maybe Arc
  , dataAnimated : Bool
  , lifeDataVisible : Bool
  , gameSecondsPerSecond : Int
  , framesPerSecond : Int
  , timeRange : Maybe (Posix, Posix)
  , player : Player
  , fadeTallObjects : Bool
  , showNaturalObjectsAboveZoom : Int
  , pointColor : PointColor
  , pointLocation : PointLocation
  , selectedServer : Maybe Int
  , serverList : RemoteData (List Data.Server)
  , servers : Dict Int Server
  , arcs : RemoteData (List Arc)
  , spans : RemoteData (List Span)
  , versions : RemoteData (List Version)
  , worlds : List World
  , dataLayer : RemoteData Bool
  , lives : RemoteData (List Life)
  , focus : Maybe Life
  }

type alias Config =
  { cachedApiUrl: String
  , apiUrl: String
  , lineageUrl: String
  , seedsUrl: String
  , spansUrl: String
  }

type alias Life =
  { birthTime : Posix
  , generation : Int
  , playerid : Int
  , lineage : Int
  , name : Maybe String
  , serverId : Int
  , epoch : Int
  , age : Maybe Float
  , birthX : Int
  , birthY : Int
  , deathTime : Maybe Posix
  , deathX : Maybe Int
  , deathY : Maybe Int
  }

type alias Server =
  { id : Int
  , serverName : String
  , minTime: Posix
  , maxTime: Posix
  , arcs : RemoteData (List Arc)
  , spans : RemoteData (List Span)
  , versions : RemoteData (List Version)
  , worlds : List World
  , monuments : RemoteData (List Monument)
  }

type Mode
  = LifeSearch
  | DataFilter
  | Cosmetics

type TimeMode
  = ServerRange
  | FromNow
  | ArcRange

type Notice
  = TimeNotice Posix Posix
  | NoNotice

type Player
  = Stopped
  | Starting
  | Playing Posix

