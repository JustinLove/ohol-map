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
  , centerUrl
  , currentArcs
  , currentServer
  , initialModel
  , defaultCenter
  )

import Leaflet exposing (Point, PointColor(..), PointLocation(..))
import OHOLData as Data
import RemoteData exposing (RemoteData(..))

import Browser.Navigation as Navigation
import Dict exposing(Dict)
import Http
import Time exposing (Posix)
import Url exposing (Url)
import Url.Builder as Url

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
  , evesOnly : Bool
  , dataAnimated : Bool
  , lifeDataVisible : Bool
  , showOnlyCurrentMonuments : Bool
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
  , versions : RemoteData (List Version)
  , dataLayer : RemoteData Int
  , lives : RemoteData (List Life)
  , focus : Maybe Life
  }

initialModel : Config -> Url -> Navigation.Key -> Model
initialModel config location key =
  { location = location
  , navigationKey = key
  , zone = Time.utc
  , time = Time.millisToPosix 0
  , notice = NoNotice
  , center = defaultCenter
  , cachedApiUrl = config.cachedApiUrl
  , apiUrl = config.apiUrl
  , lineageUrl = config.lineageUrl
  , seedsUrl = config.seedsUrl
  , spansUrl = config.spansUrl
  , sidebarOpen = False
  , sidebarMode = LifeSearch
  , searchTerm = ""
  , timeMode = ServerRange
  , coarseStartTime = Time.millisToPosix 0
  , startTime = Time.millisToPosix 0
  , mapTime = Nothing
  , hoursPeriod = 48
  , coarseArc = Nothing
  , currentArc = Nothing
  , evesOnly = False
  , dataAnimated = False
  , lifeDataVisible = False
  , showOnlyCurrentMonuments = True
  , gameSecondsPerSecond = 600
  , framesPerSecond = 10
  , timeRange = Nothing
  , player = Stopped
  , fadeTallObjects = False
  , showNaturalObjectsAboveZoom = 26
  , pointColor = LineageColor
  , pointLocation = BirthLocation
  , selectedServer = Nothing
  , serverList = NotRequested
  , servers = Dict.empty
  , versions = NotRequested
  , dataLayer = NotRequested
  , lives = NotRequested
  , focus = Nothing
  }

type alias Config =
  { cachedApiUrl: String
  , apiUrl: String
  , lineageUrl: String
  , seedsUrl: String
  , spansUrl: String
  }

defaultCenter = (Point 0 0 25)

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
  , codeChanges : List Data.Age
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

currentServer : Model -> Maybe Server
currentServer model =
  model.selectedServer
    |> Maybe.andThen (\id -> Dict.get id model.servers)

currentArcs : Model -> RemoteData (List Arc)
currentArcs model =
  model
    |> currentServer
    |> Maybe.map .arcs
    |> Maybe.withDefault NotRequested

centerUrl : Url -> Maybe Posix -> Bool -> Maybe Int -> Point -> String
centerUrl location mt yd ms {x, y, z} =
  { location
  | fragment =
    [ Just <| Url.int "x" x
    , Just <| Url.int "y" y
    , Just <| Url.int "z" z
    , Maybe.map (Url.int "s") ms
    , mt
      |> Maybe.map
      (  Time.posixToMillis
      >> (\t -> t // 1000)
      >> Url.int "t"
      )
    , if yd then
        Just <| Url.string "preset" "yesterday"
      else
        Nothing
    ]
      |> List.filterMap identity
      |> Url.toQuery
      |> String.dropLeft 1
      |> Just
  } |> Url.toString

