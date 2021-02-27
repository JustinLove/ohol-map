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
  , Preset(..)
  , Server
  , Span
  , SpanData
  , BrowseLocation(..)
  , BrowsePlacement(..)
  , TimeMode(..)
  , Version
  , World
  , centerUrl
  , currentArcs
  , currentSpans
  , currentServer
  , serverLoading
  , initialModel
  , defaultCenter
  , highlightObjectPoints
  , highlightObjectIcons
  , areAllObjectChecked
  , asSpanData
  , mapBrowseLocations
  , mapFocusBrowseLocations
  , mapBrowsePlacements
  , mapFocusBrowsePlacements
  , mapNotableLocations
  , currentLocation
  , currentPlacement
  , browseLocationInTutorial
  , browsePlacementInTutorial
  , lockedObjectList
  )

import Leaflet exposing (Point, PointColor(..), PointLocation(..))
import OHOLData as Data
import OHOLData.Parse as Parse
import RemoteData exposing (RemoteData(..))
import Theme exposing (Theme)
import Zipper exposing (Zipper)

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

type alias ObjectId = Data.ObjectId

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
  , pendingPreset : Preset
  , cachedApiUrl : String
  , apiUrl : String
  , lineageUrl: String
  , seedsUrl: String
  , spansUrl: String
  , keySearchIndex: String
  , keySearch: String
  , keySearchNotable: String
  , logSearchIndex: String
  , logSearch: String
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
  , graticuleVisible : Bool
  , monumentsVisible : Bool
  , showOnlyCurrentMonuments : Bool
  , gameSecondsPerSecond : Int
  , framesPerSecond : Int
  , timeRange : Maybe (Posix, Posix)
  , player : Player
  , fadeTallObjects : Bool
  , showNaturalObjectsAboveZoom : Int
  , activityMapVisible : Bool
  , activityMapSampleSize : Int
  , showActivityMapBelowZoom : Int
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
  , focusLife : Maybe Life
  , spanData : Maybe SpanData
  , maxiumMatchingObjects: Maybe Int
  , totalMatchingObjects: Int
  , matchingObjects : List ObjectId
  , debouncedMatchingObjects : List ObjectId
  , selectedMatchingObjects : Set ObjectId
  , lockedObjects : Set ObjectId
  , iconObjects : Set ObjectId
  , focusObject : Maybe ObjectId
  , browseProbablyTutorial : Bool
  }

initialObjectSearch = [4654, 4737]

initialModel : Config -> Url -> Navigation.Key -> Model
initialModel config location key =
  { location = location
  , navigationKey = key
  , theme = Theme.Dark
  , zone = Time.utc
  , time = Time.millisToPosix 0
  , notice = NoNotice
  , center = DefaultCenter
  , pendingPreset = NoPreset
  , cachedApiUrl = config.cachedApiUrl
  , apiUrl = config.apiUrl
  , lineageUrl = config.lineageUrl
  , seedsUrl = config.seedsUrl
  , spansUrl = config.spansUrl
  , keySearchIndex = config.keySearchIndex
  , keySearch = config.keySearch
  , keySearchNotable = config.keySearchNotable
  , logSearchIndex = config.logSearchIndex
  , logSearch = config.logSearch
  , sidebar = OpenSidebar
  , sidebarMode = Search
  , searchMode = SearchObjects
  , lifeSearchTerm = ""
  , objectSearchTerm = "adobe"
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
  , graticuleVisible = False
  , monumentsVisible = True
  , showOnlyCurrentMonuments = True
  , gameSecondsPerSecond = 600
  , framesPerSecond = 10
  , timeRange = Nothing
  , player = Stopped
  , fadeTallObjects = False
  , showNaturalObjectsAboveZoom = 26
  , activityMapVisible = True
  , activityMapSampleSize = 2
  , showActivityMapBelowZoom = 24
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
  , focusLife = Nothing
  , spanData = Nothing
  , maxiumMatchingObjects = Just 20
  , totalMatchingObjects = 0
  , matchingObjects = initialObjectSearch
  , debouncedMatchingObjects = initialObjectSearch
  , selectedMatchingObjects = Set.fromList initialObjectSearch
  , lockedObjects = Set.fromList initialObjectSearch
  , iconObjects = Set.fromList initialObjectSearch
  , focusObject = Nothing --List.head initialObjectSearch
  , browseProbablyTutorial = False
  --, browseLocations = Dict.empty --Dict.singleton 4654 (Data (Zipper.construct (BrowseLocation -50809 -52) [BrowseLocation -50809 -53]))
  --, browsePlacements = Dict.empty --Dict.singleton 4654 (Data (Zipper.construct (BrowsePlacement -50809 -52 (Time.millisToPosix 1608319484000)) [BrowsePlacement -50809 -53 (Time.millisToPosix 1608319484000)]))
  }

type alias Config =
  { cachedApiUrl: String
  , apiUrl: String
  , lineageUrl: String
  , seedsUrl: String
  , spansUrl: String
  , keySearchIndex: String
  , keySearch: String
  , keySearchNotable: String
  , logSearchIndex: String
  , logSearch: String
  }

defaultCenter = (Point 0 0 23)

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

type alias SpanData =
  { start: Posix
  , end: Posix
  , base: Posix
  , keyObjectSearchIndex: RemoteData Data.ObjectSearchIndex
  , logObjectSearchIndex: RemoteData Data.ObjectSearchIndex
  , browseLocations : Dict ObjectId (RemoteData (Zipper BrowseLocation))
  , browsePlacements : Dict ObjectId (RemoteData (Zipper BrowsePlacement))
  , notableLocations : RemoteData (List Parse.Key)
  }

type BrowseLocation = BrowseLocation Int Int
type BrowsePlacement = BrowsePlacement Int Int Posix

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

type Preset
  = NoPreset
  | Yesterday
  | DailyReview

currentServer : Model -> Maybe Server
currentServer model =
  model.selectedServer
    |> Maybe.andThen (\id -> Dict.get id model.servers)

serverLoading : Server -> Bool
serverLoading server =
  server.arcs == Loading
  || server.spans == Loading
  || server.monuments == Loading
  || server.versions == Loading

currentArcs : Model -> RemoteData (List Arc)
currentArcs model =
  model
    |> currentServer
    |> Maybe.map .arcs
    |> Maybe.withDefault NotRequested

currentSpans : Model -> RemoteData (List Span)
currentSpans model =
  model
    |> currentServer
    |> Maybe.map .spans
    |> Maybe.withDefault NotRequested

centerCoord : (Point -> a) -> Center -> Maybe a
centerCoord f center =
  case center of
    DefaultCenter -> Nothing
    SetCenter cen -> Just (f cen)

centerUrl : Url -> Maybe Posix -> Preset -> Maybe Int -> Center -> String
centerUrl location mt preset ms center =
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
    , case preset of
        NoPreset -> Nothing
        Yesterday -> Just <| Url.string "preset" "yesterday"
        DailyReview -> Just <| Url.string "preset" "daily-review"
    ]
      |> List.filterMap identity
      |> Url.toQuery
      |> String.dropLeft 1
      |> Just
  } |> Url.toString

highlightObjects : Model -> Set ObjectId
highlightObjects model =
  Set.union model.selectedMatchingObjects model.lockedObjects

highlightObjectPoints : Model -> Set ObjectId
highlightObjectPoints model =
  Set.diff (highlightObjects model) model.iconObjects

highlightObjectIcons : Model -> Set ObjectId
highlightObjectIcons model =
  Set.intersect (highlightObjects model) model.iconObjects

areAllObjectChecked : Model -> Bool
areAllObjectChecked model =
  (List.length model.matchingObjects) == (Set.size model.selectedMatchingObjects)

asSpanData : Span -> SpanData
asSpanData span =
  { start = span.start
  , end = span.end
  , base = span.base
  , keyObjectSearchIndex = NotRequested
  , logObjectSearchIndex = NotRequested
  , browseLocations = Dict.empty
  , browsePlacements = Dict.empty
  , notableLocations = NotRequested
  }

mapBrowseLocations
  : (Dict ObjectId (RemoteData (Zipper BrowseLocation)) -> Dict ObjectId (RemoteData (Zipper BrowseLocation)))
  -> Model
  -> Model
mapBrowseLocations f model =
  { model
  | spanData = model.spanData |> Maybe.map (\spanData ->
    { spanData
    | browseLocations = f spanData.browseLocations
    })
  }

mapFocusBrowseLocations : (Zipper BrowseLocation -> Zipper BrowseLocation) -> Model -> Model
mapFocusBrowseLocations f model =
  mapBrowseLocations (browseUpdate f model.focusObject) model

mapBrowsePlacements
  : (Dict ObjectId (RemoteData (Zipper BrowsePlacement)) -> Dict ObjectId (RemoteData (Zipper BrowsePlacement)))
  -> Model
  -> Model
mapBrowsePlacements f model =
  { model
  | spanData = model.spanData |> Maybe.map (\spanData ->
    { spanData
    | browsePlacements = f spanData.browsePlacements
    })
  }

mapFocusBrowsePlacements : (Zipper BrowsePlacement -> Zipper BrowsePlacement) -> Model -> Model
mapFocusBrowsePlacements f model =
  mapBrowsePlacements (browseUpdate f model.focusObject) model

mapNotableLocations
  : (RemoteData (List Parse.Key) -> RemoteData (List Parse.Key))
  -> Model
  -> Model
mapNotableLocations f model =
  { model
  | spanData = model.spanData |> Maybe.map (\spanData ->
    { spanData
    | notableLocations = f spanData.notableLocations
    })
  }

browseUpdate
  : (Zipper a -> Zipper a)
  -> Maybe ObjectId 
  -> Dict ObjectId (RemoteData (Zipper a))
  -> Dict ObjectId (RemoteData (Zipper a))
browseUpdate f mid browse =
  mid
    |> Maybe.map (\id ->
      Dict.update id (Maybe.map (RemoteData.map f)) browse
    )
    |> Maybe.withDefault browse

currentLocation : Model -> Maybe BrowseLocation
currentLocation model =
  Maybe.map2 Dict.get model.focusObject (Maybe.map .browseLocations model.spanData)
    |> Maybe.withDefault Nothing
    |> Maybe.andThen RemoteData.toMaybe
    |> Maybe.map Zipper.current

currentPlacement : Model -> Maybe BrowsePlacement
currentPlacement model =
  Maybe.map2 Dict.get model.focusObject (Maybe.map .browsePlacements model.spanData)
    |> Maybe.withDefault Nothing
    |> Maybe.andThen RemoteData.toMaybe
    |> Maybe.map Zipper.current

browseLocationInTutorial : BrowseLocation -> Bool
browseLocationInTutorial (BrowseLocation x y) = x >= 5000000

browsePlacementInTutorial : BrowsePlacement -> Bool
browsePlacementInTutorial (BrowsePlacement x y t) = x >= 5000000

lockedObjectList : Model -> List (ObjectId, Maybe String)
lockedObjectList model =
  let
    names = model.selectedServer
      |> Maybe.andThen (\serverId-> Dict.get serverId model.servers)
      |> Maybe.map .objects
      |> Maybe.withDefault Dict.empty
  in
    model.lockedObjects
      |> Set.toList
      |> List.map (\objectId -> (objectId, Dict.get objectId names))
