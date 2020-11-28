module Model exposing
  ( Arc
  , Config
  , Life
  , Sidebar(..)
  , SidebarMode(..)
  , SearchMode(..)
  , ObjectListMode(..)
  , Center(..)
  , Model
  , Monument
  , Notice(..)
  , ObjectId
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
  , highlightObjects
  , areAllObjectChecked
  )

import Leaflet exposing (Point, PointColor(..), PointLocation(..))
import OHOLData as Data
import RemoteData exposing (RemoteData(..))
import Theme exposing (Theme)

import Browser.Navigation as Navigation
import Dict exposing(Dict)
import Http
import Set exposing (Set)
import Time exposing (Posix)
import Url exposing (Url)
import Url.Builder as Url

type alias Arc = Data.Arc
type alias Monument = Data.Monument
type alias Span = Data.Span
type alias Version = Data.Version
type alias World = Data.World

type alias ObjectId = Int

type Center
  = DefaultCenter
  | SetCenter Point

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , theme : Theme
  , zone : Time.Zone
  , time : Posix
  , notice : Notice
  , center : Center
  , cachedApiUrl : String
  , apiUrl : String
  , lineageUrl: String
  , seedsUrl: String
  , spansUrl: String
  , sidebar : Sidebar
  , sidebarMode : SidebarMode
  , searchMode : SearchMode
  , lifeSearchTerm : String
  , objectSearchTerm : String
  , objectListMode : ObjectListMode
  , timeMode : TimeMode
  , coarseStartTime : Posix
  , startTime : Posix
  , mapTime : Maybe Posix
  , urlTime : Maybe Posix
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
  , displayServer : Maybe Int
  , serverList : RemoteData (List Data.Server)
  , servers : Dict Int Server
  , versions : RemoteData (List Version)
  , objects : RemoteData (Dict ObjectId String)
  , objectIndex : RemoteData (List (ObjectId, String))
  , dataLayer : RemoteData Int
  , lives : RemoteData (List Life)
  , focus : Maybe Life
  , maxiumMatchingObjects: Maybe Int
  , totalMatchingObjects: Int
  , matchingObjects : List ObjectId
  , selectedMatchingObjects : Set ObjectId
  , lockedObjects : Set ObjectId
  }

initialModel : Config -> Url -> Navigation.Key -> Model
initialModel config location key =
  { location = location
  , navigationKey = key
  , theme = Theme.Dark
  , zone = Time.utc
  , time = Time.millisToPosix 0
  , notice = NoNotice
  , center = DefaultCenter
  , cachedApiUrl = config.cachedApiUrl
  , apiUrl = config.apiUrl
  , lineageUrl = config.lineageUrl
  , seedsUrl = config.seedsUrl
  , spansUrl = config.spansUrl
  , sidebar = ClosedSidebar
  , sidebarMode = DataFilter
  , searchMode = SearchLives
  , lifeSearchTerm = ""
  , objectSearchTerm = ""
  , objectListMode = MatchingObjects
  , timeMode = ServerRange
  , coarseStartTime = Time.millisToPosix 0
  , startTime = Time.millisToPosix 0
  , mapTime = Nothing
  , urlTime = Nothing
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
  , displayServer = Nothing
  , serverList = NotRequested
  , servers = Dict.empty
  , versions = NotRequested
  , objects = NotRequested
  , objectIndex = NotRequested
  , dataLayer = NotRequested
  , lives = NotRequested
  , focus = Nothing
  , maxiumMatchingObjects = Just 20
  , totalMatchingObjects = 0
  , matchingObjects = []
  , selectedMatchingObjects = Set.fromList []
  , lockedObjects = Set.fromList []
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
  , objects : Dict ObjectId String
  , objectIndex : List (ObjectId, String)
  , monuments : RemoteData (List Monument)
  }

type Sidebar
  = ClosedSidebar
  | OpenSidebar

type SidebarMode
  = Search
  | DataFilter
  | Cosmetics

type SearchMode
  = SearchLives
  | SearchObjects

type ObjectListMode
  = MatchingObjects
  | LockedObjects

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

centerCoord : (Point -> a) -> Center -> Maybe a
centerCoord f center =
  case center of
    DefaultCenter -> Nothing
    SetCenter cen -> Just (f cen)

centerUrl : Url -> Maybe Posix -> Bool -> Maybe Int -> Center -> String
centerUrl location mt yd ms center =
  { location
  | fragment =
    [ centerCoord (.x >> Url.int "x") center
    , centerCoord (.y >> Url.int "y") center
    , centerCoord (.z >> Url.int "z") center
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

highlightObjects : Model -> Set ObjectId
highlightObjects model =
  Set.union model.selectedMatchingObjects model.lockedObjects

areAllObjectChecked : Model -> Bool
areAllObjectChecked model =
  (List.length model.matchingObjects) == (Set.size model.selectedMatchingObjects)

