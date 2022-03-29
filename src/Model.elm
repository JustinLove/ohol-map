module Model exposing
  ( Arc
  , Config
  , Life
  , LifeId
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
  , ScreenLocation
  , DragMode(..)
  , Hover(..)
  , Server
  , Span
  , SpanData
  , BrowseLocation(..)
  , BrowsePlacement(..)
  , TimeMode(..)
  , Version
  , World
  , centerUrl
  , currentWorlds
  , currentArcs
  , currentSpans
  , currentMonuments
  , currentServer
  , serverLoading
  , initialModel
  , defaultCenter
  , effectiveImageObjects
  , highlightObjectSwatches
  , highlightObjectImages
  , toggleIconDisplay
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
  , TimelineId
  , TimelineData(..)
  , Timeline
  , rebuildTimelines
  , makeTimelines
  , timeline
  , timelineScreenToTime
  , timelineTimeInDistance
  , timelineTimeToScreen
  , timelineLeave
  , timelineRange
  , setTimeRange
  , inRange
  , isInRange
  , worldToRange
  , arcToRange
  , spanToRange
  , lifeToRange
  , relativeStartTime
  , relativeEndTime
  , PopulationSample
  , SettingValue
  , Population
  , populationFromLives
  , Clusters
  , clustersFromLives
  , clustersAtTime
  , displayClusters
  )

import Leaflet exposing (PointColor(..), PointLocation(..), Animatable(..))
import OHOLData as Data
import OHOLData.Parse as Parse
import RemoteData exposing (RemoteData(..))
import Theme exposing (Theme)
import Zipper exposing (Zipper)

import Array exposing (Array)
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
  | SetCenter Leaflet.Point

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , windowWidth : Int
  , windowHeight : Int
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
  , timelineVisible : Bool
  , dataAnimated : Bool
  , mapAnimatable : Animatable
  , lifeDataVisible : Bool
  , graticuleVisible : Bool
  , monumentsVisible : Bool
  , showOnlyCurrentMonuments : Bool
  , monumentsOnTimeline : Bool
  , clustersVisible : Bool
  , gameSecondsPerSecond : Int
  , framesPerSecond : Int
  , timeRange : Maybe (Posix, Posix)
  , timelineSelections : Array (Posix, Posix)
  , timelines : List Timeline
  , drag : DragMode
  , hover : Hover
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
  , population : RemoteData Population
  , clusters : RemoteData Clusters
  , focusLife : Maybe Life
  , spanData : Maybe SpanData
  , maxiumMatchingObjects: Maybe Int
  , totalMatchingObjects: Int
  , matchingObjects : List ObjectId
  , debouncedMatchingObjects : List ObjectId
  , selectedMatchingObjects : Set ObjectId
  , lockedObjects : Set ObjectId
  , imageObjects : Set ObjectId
  , swatchObjects : Set ObjectId
  , defaultImageObjects : Set ObjectId
  , focusObject : Maybe ObjectId
  , browseProbablyTutorial : Bool
  }

initialObjectSearch = []--[4654, 4737]

initialModel : Config -> Url -> Navigation.Key -> Model
initialModel config location key =
  { location = location
  , navigationKey = key
  , windowWidth = 320
  , windowHeight = 300
  , theme = Theme.Dark
  , zone = Time.utc
  , time = Time.millisToPosix 0
  , notice = NoNotice
  , center = DefaultCenter
  , pendingPreset = NoPreset --TestData
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
  , timelineVisible = False
  , dataAnimated = False
  , mapAnimatable = Inert
  , lifeDataVisible = False
  , graticuleVisible = False
  , monumentsVisible = True
  , showOnlyCurrentMonuments = True
  , monumentsOnTimeline = False
  , clustersVisible = True
  , gameSecondsPerSecond = 600
  , framesPerSecond = 10
  , timeRange = Nothing
  , timelineSelections = Array.empty
  , timelines = []
  , drag = Released
  , hover = Away
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
  , population = NotRequested
  , clusters = NotRequested
  , focusLife = Nothing
  , spanData = Nothing
  , maxiumMatchingObjects = Just 20
  , totalMatchingObjects = 0
  , matchingObjects = initialObjectSearch
  , debouncedMatchingObjects = initialObjectSearch
  , selectedMatchingObjects = Set.fromList initialObjectSearch
  , lockedObjects = Set.fromList initialObjectSearch
  , imageObjects = Set.fromList initialObjectSearch
  , swatchObjects = Set.empty
  , defaultImageObjects = Set.fromList initialObjectSearch
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

defaultCenter = (Leaflet.Point 0 0 23)

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

type alias LifeId r =
  { r
  | playerid : Int
  , lineage : Int
  , serverId : Int
  , epoch : Int
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
  , hasLives : Bool
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
  | TestData

type alias ScreenLocation = (Float, Float)

type DragMode
  = Released
  | DragStart TimelineId Posix
  | Dragging TimelineId Posix Posix
  | Scrubbing TimelineId Posix

type Hover
  = Away
  | Hovering TimelineId Posix (Maybe String)

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

currentWorlds : Model -> List World
currentWorlds model =
  model
    |> currentServer
    |> Maybe.map .worlds
    |> Maybe.withDefault []

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

currentMonuments : Model -> RemoteData (List Monument)
currentMonuments model =
  model
    |> currentServer
    |> Maybe.map .monuments
    |> Maybe.withDefault NotRequested

centerCoord : (Leaflet.Point -> a) -> Center -> Maybe a
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
        TestData -> Nothing
    ]
      |> List.filterMap identity
      |> Url.toQuery
      |> String.dropLeft 1
      |> Just
  } |> Url.toString

highlightObjects : Model -> Set ObjectId
highlightObjects model =
  Set.union model.selectedMatchingObjects model.lockedObjects

effectiveImageObjects : Model -> Set ObjectId
effectiveImageObjects model =
  model.defaultImageObjects
    |> Set.union model.imageObjects
    |> (\s -> Set.diff s model.swatchObjects)

highlightObjectSwatches : Model -> Set ObjectId
highlightObjectSwatches model =
  effectiveImageObjects model
    |> Set.diff (highlightObjects model)

highlightObjectImages : Model -> Set ObjectId
highlightObjectImages model =
  effectiveImageObjects model
    |> Set.intersect (highlightObjects model)

toggleIconDisplay : ObjectId -> Bool -> Model -> Model
toggleIconDisplay id checked model =
  let
    imageObjects = if checked then
       Set.insert id model.imageObjects
     else
       Set.remove id model.imageObjects
    swatchObjects = if checked then
       Set.remove id model.swatchObjects
     else
       Set.insert id model.swatchObjects
  in
  { model
  | imageObjects = imageObjects
  , swatchObjects = swatchObjects
  }

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

type alias TimelineId = Int

type TimelineData
  = TimelineBlank
  | TimelineWorlds (List World)
  | TimelineArcs (List Arc)
  | TimelineSpans (List Span)
  | TimelinePopulation (List PopulationSample) (List SettingValue)

type alias Timeline =
  { id : TimelineId
  , minTime : Posix
  , maxTime : Posix
  , timeRange : Maybe (Posix, Posix)
  , width : Int
  , data : TimelineData
  , ranges : List (Posix, Posix)
  , monuments : List Monument
  }

rebuildTimelines : Model -> Model
rebuildTimelines model =
  { model | timelines = makeTimelines model }

makeTimelines : Model -> List Timeline
makeTimelines model =
  List.range 0 (Array.length model.timelineSelections)
    |> List.filterMap (timeline model)

timeline : Model -> TimelineId -> Maybe Timeline
timeline model index =
  let
    width = case model.sidebar of
      OpenSidebar -> model.windowWidth - 330
      ClosedSidebar -> model.windowWidth
    timeRange = case model.drag of
      Dragging i start time ->
        if i == index then
          if Time.posixToMillis start <= Time.posixToMillis time then
            Just (start, time)
          else
            Just (time, start)
        else
          Array.get index model.timelineSelections
      _ ->
        Array.get index model.timelineSelections
  in
  if index == 0 then
    let
      selectedServer = model.selectedServer |> Maybe.andThen (\id -> Dict.get id model.servers)
      minTime = serverMinTime model.servers selectedServer
      maxTime = serverMaxTime model.servers selectedServer
      worlds = currentWorlds model
        |> (worldsInRange (minTime, maxTime))
    in
    Just
      { id = index
      , minTime = minTime
      , maxTime = maxTime
      , timeRange = timeRange
      , width = width
      , data = TimelineWorlds worlds
      , ranges = worlds |> List.map (worldToRange maxTime)
      --, data = currentArcs model
        --|> RemoteData.map (arcsInRange (minTime, maxTime))
        --|> RemoteData.map TimelineArcs
        --|> RemoteData.withDefault TimelineBlank
      , monuments =
        if model.monumentsOnTimeline then
          currentMonuments model
            |> RemoteData.map (monumentsInRange (minTime, maxTime))
            |> RemoteData.withDefault []
        else
          []
      }
  else
    case Array.get (index-1) model.timelineSelections of
      Just (min, max) ->
        let
          range = toFloat ((Time.posixToMillis max) - (Time.posixToMillis min))
          day = 24 * 60 * 60 * 1000
          typicalSpanWidth = (day / range) * (toFloat width)
          widthDependentData = \() ->
            if typicalSpanWidth < 10 then
              currentWorlds model
                |> (worldsInRange (min, max))
                |> TimelineWorlds
            else
              currentSpans model
                |> RemoteData.map (spansInRange (min, max))
                |> RemoteData.map TimelineSpans
                |> RemoteData.withDefault TimelineBlank
          worlds = currentWorlds model
            |> (worldsInRange (min, max))
        in
        Just
          { id = index
          , minTime = min
          , maxTime = max
          , timeRange = timeRange
          , width = width
          , data =
            case model.population of
              Data pop ->
                if (rangeOverlap (min, max) pop.populationRange) > 0.5 then
                  TimelinePopulation pop.data (worlds |> settingChanges max .minActivePlayersForSpecialBiomes)
                else
                  widthDependentData ()
              _ ->
                widthDependentData ()
          , ranges =
            if typicalSpanWidth < 10 then
              worlds
                |> List.map (worldToRange max)
            else
              currentSpans model
                |> RemoteData.map (spansInRange (min, max))
                |> RemoteData.withDefault []
                |> List.map spanToRange
          , monuments =
            if model.monumentsOnTimeline then
              currentMonuments model
                |> RemoteData.map (monumentsInRange (min, max))
                |> RemoteData.withDefault []
            else
              []
          }
      Nothing ->
        Nothing

serverMinTime : Dict Int Server -> Maybe Server -> Posix
serverMinTime =
  serverTime .minTime List.minimum

serverMaxTime : Dict Int Server -> Maybe Server -> Posix
serverMaxTime =
  serverTime .maxTime List.maximum

serverTime
  :  (Server -> Posix)
  -> (List Int -> Maybe Int)
  -> Dict Int Server
  -> Maybe Server
  -> Posix
serverTime field aggragate servers current =
  case current of
    Just server ->
      server |> field
    Nothing ->
      servers
        |> Dict.values
        |> List.map field
        |> List.map Time.posixToMillis
        |> aggragate
        |> Maybe.withDefault 0
        |> Time.millisToPosix

timelineLeave : Model -> Float -> Float
timelineLeave model x =
  let
    width =
      (case model.sidebar of
        OpenSidebar -> model.windowWidth - 330
        ClosedSidebar -> model.windowWidth
      ) |> toFloat
  in
    if x < width/2 then
      0
    else
      width

timelineScreenToTime : RemoteData (List Monument) -> Timeline -> Float -> Posix
timelineScreenToTime monuments line x =
  let
    min = (Time.posixToMillis line.minTime) + 1
    max = Time.posixToMillis line.maxTime
    range = (max - min) |> toFloat
    screenRange = line.width |> toFloat
    ms = (x / screenRange)
      |> (*) range
      |> round
      |> (+) min
      |> clamp min max
    snapRange =
      (4 / screenRange)
        |> (*) range
        |> round
    snaps =
      (case line.data of
        TimelineBlank ->
          []
        TimelineWorlds worlds ->
          worlds
            |> List.map (.start>>Time.posixToMillis)
        TimelineArcs arcs ->
          arcs
            |> List.map (.start>>Time.posixToMillis)
        TimelineSpans spans ->
          spans
            |> List.map (.start>>Time.posixToMillis)
        TimelinePopulation _ _ ->
          []
      )
        |> List.append (monuments |> RemoteData.withDefault [] |> List.map (.date>>Time.posixToMillis))
        |> List.filter (\snap -> snap - snapRange < ms && ms < snap + snapRange )
  in
    if List.isEmpty snaps then
      ms |> Time.millisToPosix
    else
      snaps
        |> List.foldl (\snap candidate ->
            if abs (ms - snap) < abs (ms - candidate) then
              snap
            else
              candidate
          ) 0
        |> Time.millisToPosix

timelineTimeInDistance : Timeline -> Float -> Int
timelineTimeInDistance line length =
  let
    min = (Time.posixToMillis line.minTime) + 1
    max = Time.posixToMillis line.maxTime
    range = (max - min) |> toFloat
    screenRange = line.width |> toFloat
  in
    (length / screenRange)
      |> (*) range
      |> round

timelineTimeToScreen : Timeline -> Posix -> Float
timelineTimeToScreen line time =
  let
    min = (Time.posixToMillis line.minTime) + 1
    max = Time.posixToMillis line.maxTime
    range = (max - min) |> toFloat
    screenRange = line.width |> toFloat
    ms = Time.posixToMillis time
  in
    ms
      |> (\t -> t - min)
      |> toFloat
      |> (\t -> t / range)
      |> (*) screenRange
      |> clamp 0 screenRange

timelineRange : TimelineId -> Maybe (Posix, Posix) -> Model -> Model
timelineRange index mrange model =
  let
    animatable = mrange
      |> Maybe.map2 (\time range -> isInRange range time) model.mapTime
      |> Maybe.withDefault False
    lines =
      case mrange of
        Just range ->
          (if index < Array.length model.timelineSelections then
            Array.set index range model.timelineSelections
          else
            Array.push range model.timelineSelections
          )
            |> Array.slice 0 (index+1)
        Nothing ->
          Array.slice 0 index model.timelineSelections
  in
    { model
    | timelineSelections = lines
    , timeRange = mrange
    , dataAnimated = model.dataAnimated && animatable
    }
      |> rebuildTimelines

setTimeRange : Maybe (Posix, Posix) -> Model -> Model
setTimeRange mrange model =
  case model.currentArc of
    Just arc ->
      if model.timeMode == ArcRange then
        timelineRange 0 mrange model
      else
        model
          |> timelineRange 0 (Just (arcToRange model.time arc))
          |> timelineRange 1 (mrange)
    Nothing ->
      timelineRange 0 mrange model

inRange : (Posix, Posix) -> Posix -> Posix
inRange (mint, maxt) t =
  let
    mini = Time.posixToMillis mint
    maxi = Time.posixToMillis maxt
    i = Time.posixToMillis t
  in
    Time.millisToPosix (min maxi (max mini i))

isInRange : (Posix, Posix) -> Posix -> Bool
isInRange (mint, maxt) t =
  let
    mini = Time.posixToMillis mint
    maxi = Time.posixToMillis maxt
    i = Time.posixToMillis t
  in
    mini < i && i <= maxi

rangeOverlap : (Posix, Posix) -> (Posix, Posix) -> Float
rangeOverlap (mina, maxa) (minb, maxb) =
  let
    minai = Time.posixToMillis mina
    maxai = Time.posixToMillis maxa
    minbi = Time.posixToMillis minb
    maxbi = Time.posixToMillis maxb
    start = max minai minbi
    end = min maxai maxbi
    length = max 0 (end - start)
    total = maxai - minai
    over = (toFloat length) / (toFloat total)
  in
    over

worldToRange : Posix -> World -> (Posix, Posix)
worldToRange default {start,end} =
  (start, end |> Maybe.withDefault default)

arcToRange : Posix -> Arc -> (Posix, Posix)
arcToRange default {start,end} =
  (start, end |> Maybe.withDefault default)

spanToRange : {s| start : Posix, end : Posix} -> (Posix, Posix)
spanToRange {start,end} =
  (start, end)

lifeToRange : Life -> (Posix, Posix)
lifeToRange {birthTime, deathTime} =
  (birthTime, deathTime |> Maybe.withDefault (relativeEndTime 1 birthTime))

relativeStartTime : Int -> Posix -> Posix
relativeStartTime hoursPeriod time =
  time
    |> Time.posixToMillis
    |> (\x -> x - hoursPeriod * 60 * 60 * 1000)
    |> Time.millisToPosix

relativeEndTime : Int -> Posix -> Posix
relativeEndTime hoursPeriod time =
  time
    |> Time.posixToMillis
    |> (\x -> x + hoursPeriod * 60 * 60 * 1000)
    |> Time.millisToPosix

worldsInRange : (Posix, Posix) -> List World -> List World
worldsInRange (minTime, maxTime) worlds =
  let
    min = Time.posixToMillis minTime
    max = Time.posixToMillis maxTime
  in
  worlds
    |> List.filter (\{start} -> Time.posixToMillis start <= max)
    |> List.filter (\{end} ->
        case end of
          Just t -> min <= Time.posixToMillis t
          Nothing -> True
        )

arcsInRange : (Posix, Posix) -> List Arc -> List Arc
arcsInRange (minTime, maxTime) arcs =
  let
    min = Time.posixToMillis minTime
    max = Time.posixToMillis maxTime
  in
  arcs
    |> List.filter (\{start} -> Time.posixToMillis start <= max)
    |> List.filter (\{end} ->
        case end of
          Just t -> min <= Time.posixToMillis t
          Nothing -> True
        )

spansInRange : (Posix, Posix) -> List Span -> List Span
spansInRange (minTime, maxTime) spans =
  let
    min = Time.posixToMillis minTime
    max = Time.posixToMillis maxTime
  in
  spans
    |> List.filter (\{start} -> Time.posixToMillis start <= max)
    |> List.filter (\{end} -> min <= Time.posixToMillis end)

monumentsInRange : (Posix, Posix) -> List Monument -> List Monument
monumentsInRange (minTime, maxTime) monuments =
  let
    min = Time.posixToMillis minTime
    max = Time.posixToMillis maxTime
  in
  monuments
    |> List.filter (\{date} ->
        let t = Time.posixToMillis date in
        min <= t && t <= max
      )

type alias PopulationSample = (Posix, Int)
type alias SettingValue = (Posix, Int)

type alias Population =
  { data : List PopulationSample
  , eventRange : (Posix, Posix)
  , populationRange : (Posix, Posix)
  }

populationFromLives : Posix -> List Data.Life -> Population
populationFromLives default data =
  let
    lastBirth = data
      |> List.head
      |> Maybe.map .birthTime
      |> Maybe.withDefault default
    firstEvent = data
      |> List.foldl (\{birthTime} _ -> birthTime) default
    usablePoints = data
      |> List.concatMap (\{birthTime, birthPopulation, deathTime, deathPopulation} ->
          case (deathTime, deathPopulation) of
            (Just dt, Just dp) -> [(birthTime, birthPopulation), (dt, dp)]
            _ -> [(birthTime, birthPopulation)]
        )
      |> List.sortBy (Tuple.first>>Time.posixToMillis)
      |> resample 4096
    lastEvent = usablePoints
      |> List.foldl (\(t, _) _ -> t) default
  in
  { data = usablePoints
  , eventRange = (firstEvent, lastBirth)
  , populationRange = (firstEvent, lastEvent)
  }

resample : Int -> List a -> List a
resample targetSamples input =
  let
    length = List.length input
  in
    if length <= targetSamples then
      input
    else
      input
        |> List.foldl
          (\x (accum, out) ->
            let next = accum + targetSamples in
            if next >= length then
              (next - length, x::out)
            else
              (next, out)
          )
          (0, [])
        |> Tuple.second
        |> List.reverse

settingChanges : Posix -> (World -> Maybe Int) -> List World -> List SettingValue
settingChanges now field worlds =
  worlds
    |> List.concatMap (\w ->
      case field w of
        Just value ->
          [ (w.start, value)
          , (w.end |> Maybe.withDefault now, value)
          ]
        Nothing -> []
      )

type alias LocationSample =
  { time : Posix
  , lineage : Int
  , x : Int
  , y : Int
  }

type alias Cluster =
  { x : Int
  , y : Int
  , members : Int
  }

type alias Lineage =
  { locations : List LocationSample
  , clusters : List Cluster
  }

type alias Clusters =
  { locations : List LocationSample
  , lineages : Dict Int Lineage
  }

locationsFromLives : PointLocation -> Posix -> List Data.Life -> List LocationSample
locationsFromLives pointLocation default data =
  let
    samples =
      case pointLocation of
        BirthLocation ->
          data
            |> List.map (\{lineage, birthTime, birthX, birthY} ->
                { time = birthTime
                , lineage = lineage
                , x = birthX
                , y = birthY
                }
              )
        DeathLocation ->
          data
            |> List.filterMap (\{lineage, deathTime, deathX, deathY} ->
              Maybe.map3 (\dt dx dy ->
                  { time = dt
                  , lineage = lineage
                  , x = dx
                  , y = dy
                  }
                )
                deathTime deathX deathY
              )
  in
    samples-- |> List.sortBy (.time>>Time.posixToMillis)

clustersFromLives : PointLocation -> Posix -> List Data.Life -> Clusters
clustersFromLives pointLocation default data =
  let
    locations = locationsFromLives pointLocation default data
    lineageLocations = groupLineages locations
  in
  { locations = locations
  , lineages = mapValues lineageClusters lineageLocations
  }

clustersAtTime : Posix -> Clusters -> Clusters
clustersAtTime time clusters =
  let
    start = time
      |> Time.posixToMillis
      |> (\x -> x - 60*60*1000)
      |> Time.millisToPosix
    range = (start, time)
    locationsAtTime = clusters.locations
      |> List.filter (.time>>(isInRange range))
    lineageLocations = groupLineages locationsAtTime
  in
  { clusters | lineages = mapValues lineageClusters lineageLocations }

groupLineages : List LocationSample -> Dict Int (List LocationSample)
groupLineages = groupListToDict .lineage

groupListToDict : (a -> comparable) -> List a -> Dict comparable (List a)
groupListToDict toKey list =
  List.foldl (\value ->
      Dict.update (toKey value) (\mprev ->
        case mprev of
          Just prev ->
            Just (value :: prev)
          Nothing ->
            Just ([ value ])
      )
    )
    Dict.empty
    list

mapValues : (a -> b) -> Dict comparable a -> Dict comparable b
mapValues f dict =
  Dict.map (\_ a -> f a) dict

clustering : List LocationSample -> List Cluster
clustering locations =
  List.foldl clusterStep [] locations
    |> List.sortBy (\{members} -> -members)

clusterStep : LocationSample -> List Cluster -> List Cluster
clusterStep location clusters =
  case nearestCluster 200 location clusters of
    Just best ->
      List.map (clusterUpdate location best) clusters
    Nothing ->
      { x = location.x
      , y = location.y
      , members = 1
      } :: clusters

nearestCluster : Int -> LocationSample -> List Cluster -> Maybe Cluster
nearestCluster range {x, y} clusters =
  let
    range2 = range*range
  in
  (List.foldl (\candidate best ->
      let
        dx = x - candidate.x
        dy = y - candidate.y
        d = dx*dx + dy*dy
      in
      if d >= range2 then
        best
      else
        case best of
          Just (bd, bc) ->
            if d < bd then
              Just (d, candidate)
            else
              Just (bd, bc)
          Nothing ->
            Just (d, candidate)
    )
    Nothing
    clusters
  )
    |> Maybe.map Tuple.second

clusterFactor = 0.3

clusterUpdate : LocationSample -> Cluster -> Cluster -> Cluster
clusterUpdate location previous cluster =
  if cluster == previous then
    { x = cluster.x + (round ((toFloat (location.x - cluster.x)) * clusterFactor))
    , y = cluster.y + (round ((toFloat (location.y - cluster.y)) * clusterFactor))
    , members = cluster.members + 1
    }
  else
    cluster

lineageClusters : List LocationSample -> Lineage
lineageClusters locations =
  { locations = locations
  , clusters = clustering locations
  }

displayClusters : Bool -> Int -> Clusters -> Dict Int (List Cluster)
displayClusters dataAnimated zoom clusters =
  let
    threashold =
      if dataAnimated then
        2
      else
        2^(26-zoom)
  in
  clusters
    |> .lineages
    |> Dict.map (\k v ->
        v.clusters
          |> List.filter (\{members} -> members > threashold)
          |> List.map (\{x,y,members} ->
              { x = collapseToWellGrid x
              , y = collapseToWellGrid y
              , members = members
              }
            )
      )

collapseToWellGrid : Int -> Int
collapseToWellGrid x =
  x + 20 - (modBy 40 (x + 20))
