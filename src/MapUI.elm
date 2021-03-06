module MapUI exposing (..)

import Leaflet exposing (Point, PointColor(..), PointLocation(..))
import LocalStorage
import Log
import Model exposing (..)
import OHOLData as Data
import OHOLData.Decode as Decode
import OHOLData.Encode as Encode
import OHOLData.Parse as Parse
import Persist exposing (Persist)
import Persist.Encode
import Persist.Decode
import Persist.Serialize
import RemoteData exposing (RemoteData(..))
import Theme exposing (Theme)
import View exposing (timeNoticeDuration)
import Zipper exposing (Zipper)

import Browser
import Browser.Events
--import Browser.Dom
import Browser.Navigation as Navigation
import Http
import Json.Decode
import Dict exposing(Dict)
import File.Download
import Parser.Advanced as Parser
import Process
import Set exposing (Set)
import Task
import Time exposing (Posix)
import Tuple
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser
import Url.Parser.Query

type Msg
  = Loaded (Maybe Persist)
  | UI View.Msg
  | Event (Result Json.Decode.Error Leaflet.Event)
  | MatchingLives (Result Http.Error (List Data.Life))
  | LineageLives Int (Result Http.Error Json.Decode.Value)
  | ServerList (Result Http.Error (List Data.Server))
  | ArcList Int (Result Http.Error (List Data.Arc))
  | SpanList Int (Result Http.Error (List Data.Span))
  | KeyObjectSearchIndexReceived Int Posix (Result Http.Error Data.ObjectSearchIndex)
  | LogObjectSearchIndexReceived Int Posix (Result Http.Error Data.ObjectSearchIndex)
  | KeyObjectSearchReceived Int Posix ObjectId (Result Http.Error (List BrowseLocation))
  | LogObjectSearchReceived Int Posix ObjectId (Result Http.Error (List BrowsePlacement))
  | NotableObjectsReceived Int Posix (Result Http.Error (List Parse.Key))
  | ObjectsReceived (Result Http.Error Data.Objects)
  | MonumentList Int (Result Http.Error (List Data.Monument))
  | DataLayer Int (Result Http.Error Json.Decode.Value)
  | NoDataLayer
  | FetchUpTo Posix
  | FetchBetween Posix Posix
  | PlayRelativeTo Int Posix
  | ShowTimeNotice Posix Posix
  | CurrentTimeNotice Posix
  | CurrentTime Posix
  | Playback Posix
  | UpdateUrl Posix
  | UpdateHighlightedObjects Posix
  | CurrentZone Time.Zone
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

main = Browser.application
  { init = init
  , update = updatePreset
  , subscriptions = subscriptions
  , view = View.document UI
  , onUrlRequest = Navigate
  , onUrlChange = CurrentUrl
  }

init : Config -> Url -> Navigation.Key -> (Model, Cmd Msg)
init config location key =
  let
    (model, cmd) = changeRouteTo location (initialModel config location key)
  in
    ( model
    , Cmd.batch
      [ cmd
      , Time.here |> Task.perform CurrentZone
      , case model.mapTime of
        Just time -> Cmd.none
        Nothing -> Time.now |> Task.perform CurrentTimeNotice
      , fetchServers model.cachedApiUrl
      , fetchObjects
      , Leaflet.pointColor model.pointColor
      , Leaflet.pointLocation model.pointLocation
      , Leaflet.animOverlay model.dataAnimated
      , Leaflet.overlayVisible "Activity Map" model.activityMapVisible
      , Leaflet.worldList (Data.rebuildWorlds Data.oholCodeChanges [] [] [])
      , Leaflet.showNaturalObjectsAboveZoom model.showNaturalObjectsAboveZoom
      , Leaflet.showActivityMapBelowZoom model.showActivityMapBelowZoom
      , highlightObjectsCommand model
      , sidebarCommand model
      ]
    )

updatePreset : Msg -> Model -> (Model, Cmd Msg)
updatePreset msg model =
  let (m2, c2) = update msg model in
  if (detectPreset model /= detectPreset m2) then
    replaceUrl "preset change" (m2, c2)
  else
    (m2, c2)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loaded mstate ->
      ( case mstate of
          Just state ->
            { model
            | theme = state.theme
            , showNaturalObjectsAboveZoom = state.showNaturalObjectsAboveZoom
            , showActivityMapBelowZoom = state.showActivityMapBelowZoom
            }
          Nothing ->
            model
      )
        |> resolveLoaded
    UI (View.None) -> (model, Cmd.none)
    UI (View.PerformLifeSearch term) ->
      ( { model
        | lifeSearchTerm = term
        , lives = Loading
        }
      , fetchMatchingLives model.apiUrl term
      )
    UI (View.LifeTyping term) ->
      ( { model
        | lifeSearchTerm = term
        }
      , Cmd.none
      )
    UI (View.ObjectTyping term) ->
      { model | objectSearchTerm = term }
        |> updateObjectSearch
        |> requireObjectSearchIndex
    UI (View.SelectTimeMode mode) ->
      ( { model | timeMode = mode }
      , Cmd.none
      )
    UI (View.CoarseStartTime time) ->
      { model
      | coarseStartTime = time
      , startTime = time
      }
        |> jumpTime time
    UI (View.StartTime time) ->
      { model
      | startTime = time
      }
        |> jumpTime time
    UI (View.HoursBefore hours) ->
      { model
      | hoursPeriod = hours
      }
        |> jumpTime (relativeStartTime hours model.time)
    UI (View.HoursAfter hours) ->
      ( { model
        | hoursPeriod = hours
        }
      , Cmd.none
      )
    UI (View.ToggleEvesOnly evesOnly) ->
      ( { model | evesOnly = evesOnly }, Cmd.none )
    UI (View.ToggleUTC utc) ->
      if utc then
        ( { model | zone = Time.utc }, Cmd.none )
      else
        ( model, Time.here |> Task.perform CurrentZone )
    UI (View.ChangeTheme theme) ->
      ( { model | theme = theme }
      , changeTheme theme
      )
       |> addCommand saveState
    UI (View.ToggleAnimated animated) ->
      toggleAnimated animated model
    UI (View.GameSecondsPerSecond seconds) ->
      ( { model
        | gameSecondsPerSecond = seconds
        }
      , Cmd.none
      )
    UI (View.FramesPerSecond frames) ->
      ( { model
        | framesPerSecond = frames
        }
      , Cmd.none
      )
    UI (View.MapTime time) ->
      scrubTime time model
    UI (View.Play) ->
      ( { model
        | player = Starting
        , mapTime =
          if model.mapTime == (model.timeRange |> Maybe.map Tuple.second) then
            model.timeRange |> Maybe.map Tuple.first
          else
            model.mapTime
        }
      , Cmd.none
      )
    UI (View.Pause) ->
      ( { model
        | player = Stopped
        }
      , Cmd.none
      )
    UI (View.ToggleShowMonuments show) ->
      ( { model
        | monumentsVisible = show
        }
      , Leaflet.overlayVisible "Monuments" show
      )
    UI (View.ToggleShowOnlyCurrentMonuments show) ->
      ( { model
        | showOnlyCurrentMonuments = show
        }
      , Leaflet.showOnlyCurrentMonuments show
      )
    UI (View.ToggleFadeTallObjects fade) ->
      ( { model
        | fadeTallObjects = fade
        }
      , Leaflet.fadeTallObjects fade
      )
    UI (View.SelectNaturalObjectZoom zoom) ->
      ( { model
        | showNaturalObjectsAboveZoom = zoom
        }
      , Leaflet.showNaturalObjectsAboveZoom zoom
      )
        |> addCommand saveState
    UI (View.ToggleShowActivityMap show) ->
      ( { model
        | activityMapVisible = show
        }
      , Leaflet.overlayVisible "Activity Map" show
      )
    UI (View.SelectActivityMapSampleSize sampleSize) ->
      ( { model
        | activityMapSampleSize = sampleSize
        }
      , Leaflet.activityMapSampleSize sampleSize
      )
    UI (View.SelectActivityMapZoom zoom) ->
      ( { model
        | showActivityMapBelowZoom = zoom
        }
      , Leaflet.showActivityMapBelowZoom zoom
      )
        |> addCommand saveState
    UI (View.ToggleShowLifeData show) ->
      if show then
        requireLives { model | lifeDataVisible = show }
      else
        ({model | lifeDataVisible = show}
        , Leaflet.dataLayerVisible False)
    UI (View.SelectPointColor color) ->
      ( { model
        | pointColor = color
        }
      , Leaflet.pointColor color
      )
    UI (View.SelectPointLocation location) ->
      ( { model
        | pointLocation = location
        }
      , Leaflet.pointLocation location
      )
    UI (View.SelectMatchingLife life) ->
      ( { model
        | focusLife = Just life
        , timeRange = Just (lifeToRange life)
        , mapTime = Just life.birthTime
        }
      , Cmd.batch
        [ Leaflet.currentTime life.birthTime
        , Time.now |> Task.perform (ShowTimeNotice life.birthTime)
        , Leaflet.focusLife (serverLife life)
        ]
      )
       |> setServer life.serverId
    UI (View.ToggleMatchingObject id checked) ->
      let
        selectedMatchingObjects = if checked then
           Set.insert id model.selectedMatchingObjects
         else
           Set.remove id model.selectedMatchingObjects
      in
      { model
      | selectedMatchingObjects = selectedMatchingObjects
      }
        |> andHighlightObjects
    UI (View.SelectMatchingObject id) ->
      ( { model | focusObject = Just id }
      , Cmd.none
      )
        |> addUpdate requireObjectSearch
    UI (View.ExitBrowseLocations) ->
      ( { model | focusObject = Nothing }
      , Leaflet.focusNone
      )
    UI (View.SelectBrowseLocation location) ->
      model
        |> mapFocusBrowseLocations (Zipper.goto location)
        |> focusLocation location
    UI (View.SelectBrowsePlacement placement) ->
      model
        |> mapFocusBrowsePlacements (Zipper.goto placement)
        |> focusPlacement placement
    UI (View.Previous) ->
      previousBrowseItem model
    UI (View.Next) ->
      nextBrowseItem model
    UI (View.ToggleBrowseProbablyTutorial checked) ->
      ( { model | browseProbablyTutorial = checked }, Cmd.none )
    UI (View.ToggleAllObjects checked) ->
      let
        selectedMatchingObjects = if checked then
           Set.fromList model.matchingObjects
         else
           Set.empty
      in
      { model
      | selectedMatchingObjects = selectedMatchingObjects
      }
        |> andHighlightObjects
    UI (View.SelectMaximumObjects maximum) ->
      { model | maxiumMatchingObjects = maximum }
        |> updateObjectSearch
        |> requireObjectSearchIndex
    UI (View.LockObjects) ->
      { model | lockedObjects = Set.union model.selectedMatchingObjects model.lockedObjects }
        |> andHighlightObjects
    UI (View.DownloadLocked) ->
      ( model
      , model
        |> exportLocked
        |> File.Download.string "locked_objects.txt" "text/plain"
      )
    UI (View.ClearLocked) ->
      { model | lockedObjects = Set.fromList [] }
        |> andHighlightObjects
    UI (View.ToggleLockObject id checked) ->
      let
        lockedObjects = if checked then
           Set.insert id model.lockedObjects
         else
           Set.remove id model.lockedObjects
      in
      { model
      | lockedObjects = lockedObjects
      }
        |> toggleIconDisplay id (imageWhenLocked id model)
        |> andHighlightObjects
    UI (View.ToggleIconDisplay id checked) ->
      toggleIconDisplay id checked model
        |> andHighlightObjects
    UI (View.SelectLineage life) ->
      ( model
      , fetchLineage model.apiUrl life
      )
    UI (View.SelectSidebarMode mode) ->
      ( { model | sidebarMode = mode }
      , Cmd.none
      )
        |> addCommand sidebarCommand
    UI (View.SelectSearchMode mode) ->
      ( { model | searchMode = mode }
      , Cmd.none
      )
        |> addCommand sidebarCommand
    UI (View.SelectObjectListMode mode) ->
      ( { model | objectListMode = mode }
      , Cmd.none
      )
    UI (View.SelectServer serverId) ->
      ( model, Cmd.none )
        |> setServer serverId
    UI (View.SelectArc marc) ->
      selectArc model marc model.coarseArc
    UI (View.SelectArcCoarse marc) ->
      selectArc model marc marc
    UI (View.SelectShow) ->
      ( {model | dataLayer = Loading}
      , fetchDataForTime model
      )
    Event (Ok (Leaflet.MoveEnd point)) ->
      ( {model|center = SetCenter point}
      , Cmd.none
      )
        |> replaceUrl "MoveEnd"
    Event (Ok (Leaflet.OverlayAdd "Life Data" _)) ->
      requireLives model
    Event (Ok (Leaflet.OverlayRemove "Life Data")) ->
      ({model | lifeDataVisible = False}, Cmd.none)
    Event (Ok (Leaflet.OverlayAdd "graticule" _)) ->
      ({model | graticuleVisible = True}, Cmd.none)
    Event (Ok (Leaflet.OverlayRemove "graticule")) ->
      ({model | graticuleVisible = False}, Cmd.none)
    Event (Ok (Leaflet.OverlayAdd "Monuments" _)) ->
      ({model | monumentsVisible = True}, Cmd.none)
    Event (Ok (Leaflet.OverlayRemove "Monuments")) ->
      ({model | monumentsVisible = False}, Cmd.none)
    Event (Ok (Leaflet.OverlayAdd "Activity Map" _)) ->
      ({model | activityMapVisible = True}, Cmd.none)
    Event (Ok (Leaflet.OverlayRemove "Activity Map")) ->
      ({model | activityMapVisible = False}, Cmd.none)
    Event (Ok (Leaflet.OverlayAdd name (Just serverId))) ->
      case Dict.get serverId model.servers of
        Just server ->
          let
            (s2, cmd) = requireMonuments model server
          in
            ({model | servers = Dict.insert serverId s2 model.servers}, cmd)
        Nothing ->
          (model, Cmd.none)
    Event (Ok (Leaflet.OverlayAdd name _)) ->
      (model, Cmd.none)
    Event (Ok (Leaflet.OverlayRemove name)) ->
      (model, Cmd.none)
    Event (Ok (Leaflet.SelectPoints lives)) ->
      ( { model
        | lives = lives |> List.map myLife |> Data
        , sidebar = OpenSidebar
        , sidebarMode = Search
        , lifeSearchTerm = ""
        }
      , Cmd.batch
        [ Leaflet.displayResults lives
        , Leaflet.searchOverlay True
        ]
      )
        |> addCommand sidebarCommand
    Event (Ok (Leaflet.DataRange min max)) ->
      let
        mapTime = model.mapTime
            |> Maybe.map (inRange (min, max))
      in
      ( { model
        | timeRange = Just (min, max)
        , mapTime = mapTime
        }
      , Cmd.batch
        [ mapTime
          |> Maybe.map Leaflet.currentTime
          |> Maybe.withDefault Cmd.none
        , if isYesterday model then
            Time.now |> Task.perform (PlayRelativeTo 24)
          else
            Cmd.none
        ]
      )
    Event (Ok (Leaflet.SidebarToggle)) ->
      ( { model | sidebar = case model.sidebar of
          ClosedSidebar -> OpenSidebar
          OpenSidebar -> ClosedSidebar
        }
      , Cmd.none
      )
        |> addCommand sidebarCommand
    Event (Ok (Leaflet.AnimToggle)) ->
      toggleAnimated (not model.dataAnimated) model
    Event (Err err) ->
      (model, Log.decodeError "error" err)
    MatchingLives (Ok lives) ->
      ( {model | lives = lives |> List.map myLife |> Data}
      , Cmd.batch
        [ Leaflet.displayResults lives
        , Leaflet.searchOverlay True
        ]
      )
    MatchingLives (Err error) ->
      ({model | lives = Failed error}, Log.httpError "fetch lives failed" error)
    LineageLives serverId (Ok value) ->
      ( { model
        | lives = case value |> Json.Decode.decodeValue Decode.lives of
          Ok lives ->
            lives |> List.map myLife |> Data
          Err error ->
            error
              |> Json.Decode.errorToString
              |> Http.BadBody
              |> Failed
        , dataLayer = Data serverId
        }
      , Cmd.batch
        [ Leaflet.dataLayer value
        ]
      )
    LineageLives serverId (Err error) ->
      ({model | lives = Failed error}, Log.httpError "fetch lives failed" error)
    ServerList (Ok serverList) ->
      let
        servers = serverList
          |> List.map (myServer model.versions model.objects model.objectIndex)
          |> (++) [crucible NotAvailable NotAvailable NotAvailable model.time]
          |> (++) [twoHoursOneLife NotAvailable NotAvailable NotAvailable model.time]
          |> (++) [future model.versions model.objects model.objectIndex model.time]
        current = case model.selectedServer of
          Just sid ->
            servers
              |> List.filter (\s -> s.id == sid)
              |> List.head
          Nothing ->
            servers
              |> List.filter (\s -> s.serverName == "bigserver2.onehouronelife.com")
              |> List.head
        id = current |> Maybe.map .id |> Maybe.withDefault 17
        startTime = current
          |> Maybe.map .maxTime
          |> Maybe.map (relativeEndTime model.hoursPeriod)
          |> Maybe.withDefault (Time.millisToPosix 0)
      in
        ( { model
          | serverList = serverList |> Data
          , servers = servers |> List.map (\s -> (s.id, s)) |> Dict.fromList
          , coarseStartTime = startTime
          , startTime = startTime
          }
            |> timeSelectionForTime
        , Cmd.none
        )
          |> setServer id
          |> checkServerLoaded
          |> addUpdate requireObjectSearchIndex
          |> addUpdate requireObjectSearch
          |> addUpdate requireNotableObjects
    ServerList (Err error) ->
      ({model | serverList = Failed error, servers = Dict.empty}, Log.httpError "fetch servers failed" error)
    ArcList serverId (Ok arcs) ->
      let
        lastArc = arcs |> List.reverse |> List.head
        rspans = model.servers
          |> Dict.get serverId
          |> Maybe.map .spans
          |> Maybe.withDefault NotAvailable
        assignDataTime =
          case rspans of
            Data spans -> Data.assignArcDataTime spans
            _ -> identity
        marc = model.mapTime
          |> Maybe.andThen (\time -> arcForTime time (Data arcs))
          |> Maybe.map assignDataTime
          |> (\ma -> case ma of
            Just _ -> ma
            Nothing -> lastArc
          )
      in
        ( { model
          | servers = model.servers |> Dict.update serverId (Maybe.map (\server -> {server | arcs = Data arcs} |> rebuildArcs))
          , currentArc = marc
          , coarseArc = marc
          , timeRange = marc |> Maybe.map (arcToRange model.time)
          }
        , Cmd.none
        )
        |> rebuildWorlds
        |> updateMonuments serverId
        |> checkServerLoaded
    ArcList serverId (Err error) ->
      ( { model
        | servers = model.servers |> Dict.update serverId (Maybe.map (\server -> {server | arcs = Failed error}))
        , currentArc = Nothing
        , coarseArc = Nothing
        , timeRange = Nothing}
      , Log.httpError "fetch arcs failed" error
      )
    SpanList serverId (Ok spans) ->
      let
        lastSpan = spans |> List.reverse |> List.head
        assignDataTime = Data.assignArcDataTime spans
        mspan = model.mapTime
          |> Maybe.andThen (\time -> spanForTime time (Data spans))
          |> (\ms -> case ms of
            Just _ -> ms
            Nothing -> lastSpan
          )
        mlastTime = lastSpan |> Maybe.map .end
      in
        ( { model
          | servers = model.servers |> Dict.update serverId (Maybe.map (\server -> {server | spans = Data spans} |> rebuildArcs))
          , timeRange = mspan |> Maybe.map spanToRange
          , spanData = mspan |> Maybe.map asSpanData
          , coarseArc = model.coarseArc |> Maybe.map assignDataTime
          , currentArc = model.currentArc |> Maybe.map assignDataTime
          }
        , Cmd.none
        )
        |> maybeSetTime mlastTime
        |> rebuildWorlds
        |> checkServerLoaded
        |> addUpdate requireObjectSearchIndex
        |> addUpdate requireObjectSearch
        |> addUpdate requireNotableObjects
    SpanList serverId (Err error) ->
      ( { model
        | servers = model.servers |> Dict.update serverId (Maybe.map (\server -> {server | spans = Failed error}))
        }
      , Log.httpError "fetch spans failed" error
      )
    KeyObjectSearchIndexReceived serverId datatime (Ok index) ->
      ( { model
        | spanData = model.spanData |> Maybe.map (\spanData -> {spanData | keyObjectSearchIndex = Data index})
        }
          |> updateObjectSearch
        , Cmd.none
      )
    KeyObjectSearchIndexReceived serverId datatime (Err error) ->
      ( { model
        | spanData = model.spanData |> Maybe.map (\spanData -> {spanData | keyObjectSearchIndex = Failed error})
        }
      , Log.httpError "fetch object search index failed" error
      )
    LogObjectSearchIndexReceived serverId datatime (Ok index) ->
      ( { model
        | spanData = model.spanData |> Maybe.map (\spanData -> {spanData | logObjectSearchIndex = Data index})
        }
          |> updateObjectSearch
        , Cmd.none
      )
    LogObjectSearchIndexReceived serverId datatime (Err error) ->
      ( { model
        | spanData = model.spanData |> Maybe.map (\spanData -> {spanData | logObjectSearchIndex = Failed error})
        }
      , Log.httpError "fetch object search index failed" error
      )
    KeyObjectSearchReceived serverId datatime id (Ok (head :: tail)) ->
      model
        |> mapBrowseLocations (Dict.insert id (Data (Zipper.construct head tail)))
        |> focusLocation head
    KeyObjectSearchReceived serverId datatime id (Ok []) ->
      ( model, Cmd.none)
    KeyObjectSearchReceived serverId datatime id (Err error) ->
      ( model
          |> mapBrowseLocations (Dict.insert id (Failed error))
      , Log.httpError "fetch key object search failed" error
      )
    LogObjectSearchReceived serverId datatime id (Ok (head :: tail)) ->
      model
        |> mapBrowsePlacements (Dict.insert id (Data (Zipper.construct head tail)))
        |> focusPlacement head
    LogObjectSearchReceived serverId datatime id (Ok []) ->
      ( model, Cmd.none)
    LogObjectSearchReceived serverId datatime id (Err error) ->
      ( model
          |> mapBrowsePlacements (Dict.insert id (Failed error))
      , Log.httpError "fetch log object search failed" error
      )
    NotableObjectsReceived serverId datatime (Ok data) ->
      case model.spanData of
        Just spanData ->
          ( { model
            | spanData = Just { spanData | notableLocations = Data data}
            }
          , Leaflet.notableObjects data
          )
            |> checkDefaultCenter
        Nothing ->
          ( model, Cmd.none )
    NotableObjectsReceived serverId datatime (Err error) ->
      ( model
        |> mapNotableLocations (\previous -> Failed error)
      , Cmd.batch
        [ Leaflet.notableObjects []
        , Log.httpError "fetch notable object failed" error
        ]
      )
        |> checkDefaultCenter
    ObjectsReceived (Ok objects) ->
      let
        versions = Data.completeVersions objects.spawnChanges
        objectMap = makeObjectMap objects.ids objects.names
        objectIndex = makeObjectIndex objects.ids objects.names
      in
      ( { model
        | versions = Data versions
        , objects = Data objectMap
        , objectIndex = Data objectIndex
        , servers = model.servers
          |> Dict.map (\_ server ->
            { server
            | versions = Data versions
            , objects = objectMap
            , objectIndex = objectIndex
            })
        }
      , Leaflet.objectBounds objects.idsValue objects.boundsValue
      )
        |> rebuildWorlds
        |> addUpdate requireNotableObjects
    ObjectsReceived (Err error) ->
      (model, Log.httpError "fetch objects failed" error)
    MonumentList serverId (Ok monuments) ->
      ( { model
        | servers = model.servers |> Dict.update serverId (Maybe.map (\server -> {server | monuments = Data monuments}))
        }
      , Cmd.none
      )
        |> updateMonuments serverId
        |> checkDefaultCenter
        |> checkServerLoaded
    MonumentList serverId (Err error) ->
      (model, Log.httpError "fetch monuments failed" error)
    DataLayer serverId (Ok lives) ->
      ( { model
        | dataLayer = Data serverId
        , player = if model.dataAnimated then Starting else Stopped
        }
      , Cmd.batch
        [ Leaflet.dataLayer lives
        ]
      )
    DataLayer serverId (Err error) ->
      ( {model | dataLayer = Failed error}
      , Log.httpError "fetch data failed" error
      )
    NoDataLayer ->
      ( { model
        | dataLayer = NotRequested
        , player = Stopped
        }
      , Cmd.batch
        [ Leaflet.dataLayer (Encode.lives [])
        ]
      )
    FetchUpTo time ->
      ( model
      , fetchDataLayer model.apiUrl
          (model.selectedServer |> Maybe.withDefault 17)
          (relativeStartTime model.hoursPeriod time)
          time
          model.evesOnly
      )
    FetchBetween start now ->
      ( { model | hoursPeriod = 1 }
      , fetchDataLayer model.apiUrl
          (model.selectedServer |> Maybe.withDefault 17)
          start
          now
          model.evesOnly
      )
    PlayRelativeTo hoursBefore time ->
      { model
      | player = Starting
      }
        |> jumpTime (relativeStartTime hoursBefore time)
    ShowTimeNotice show time ->
      let
        until = time
          |> Time.posixToMillis
          |> (\t -> t + timeNoticeDuration)
          |> Time.millisToPosix
      in
      ({model | time = time, notice = TimeNotice show until}, Cmd.none)
    CurrentTimeNotice time ->
      update (ShowTimeNotice time time) model
    CurrentTime time ->
      ( { model
        | time = time
        , notice = case model.notice of
          TimeNotice _ until ->
            if ((until |> Time.posixToMillis) - (model.time |>  Time.posixToMillis)) > 0 then
              model.notice
            else
              NoNotice
          NoNotice ->
            NoNotice
        }
      , Cmd.none
      )
    Playback next ->
      case model.player of
        Stopped -> (model, Cmd.none)
        Starting -> ({model | player = Playing next}, Cmd.none)
        Playing last ->
          case model.mapTime of
            Nothing -> ({model | player = Playing next}, Cmd.none)
            Just time ->
              let
                dt = ((next |> Time.posixToMillis) - (last |> Time.posixToMillis))

                msNewTime = time
                  |> Time.posixToMillis
                  |> (\t -> t + dt * model.gameSecondsPerSecond)
                msMaxTime = model.timeRange
                  |> Maybe.map (Tuple.second >> Time.posixToMillis)
                msCappedTime = msMaxTime
                  |> Maybe.map (min msNewTime)
                  |> Maybe.withDefault msNewTime
                  |> Time.millisToPosix
                over = msMaxTime
                  |> Maybe.map (\max -> msNewTime >= max)
                  |> Maybe.withDefault False
              in
                ( { model
                  | mapTime = Just msCappedTime
                  , player = if over then Stopped else Playing next
                  }
                , Leaflet.currentTime msCappedTime
                )
    UpdateUrl _ ->
      ({model | urlTime = model.mapTime}, replaceUrlCommand model)
    UpdateHighlightedObjects _ ->
      model |> andHighlightObjects
    CurrentZone zone ->
      ({model | zone = zone}, Cmd.none)
    CurrentUrl location ->
      changeRouteTo location model
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
    Navigate (Browser.External url) ->
      (model, Navigation.load url)

saveState : Model -> Cmd Msg
saveState model =
  Persist
    model.theme
    model.showNaturalObjectsAboveZoom
    model.showActivityMapBelowZoom
      |> Persist.Encode.persist
      |> LocalStorage.saveJson

resolveLoaded : Model -> (Model, Cmd Msg)
resolveLoaded model =
  ( model
  , Cmd.batch
    [ changeTheme model.theme
    , Leaflet.showNaturalObjectsAboveZoom model.showNaturalObjectsAboveZoom
    , Leaflet.showActivityMapBelowZoom model.showActivityMapBelowZoom
    ]
  )

exportLocked : Model -> String
exportLocked model =
  model
    |> lockedObjectList
    |> Persist.Serialize.lockedObjects

sidebarCommand : Model -> Cmd Msg
sidebarCommand model =
  let
    sidebar = case model.sidebar of
      ClosedSidebar -> "closed"
      OpenSidebar -> "life"
  in
    Cmd.batch
      [ Leaflet.sidebar sidebar
      , Leaflet.searchOverlay
        (model.sidebar == OpenSidebar && model.sidebarMode == Search && model.searchMode == SearchLives)
      ]

rebuildWorlds : (Model, Cmd Msg) -> (Model, Cmd Msg)
rebuildWorlds =
  addCommand rebuildWorldsCommand

rebuildWorldsCommand : Model -> Cmd Msg
rebuildWorldsCommand model =
  let
    mServer = currentServer model
    prop = \field -> Maybe.map field >> Maybe.withDefault NotRequested >> RemoteData.withDefault []
    worlds = Data.rebuildWorlds
      (mServer |> Maybe.map .codeChanges |> Maybe.withDefault Data.oholCodeChanges)
      (mServer |> prop .versions)
      (mServer |> prop .arcs)
      (mServer |> prop .spans)
  in
    Leaflet.worldList worlds

updateMonuments : Int -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateMonuments serverId =
  addCommand (updateMonumentsCommand serverId)

updateMonumentsCommand : Int -> Model -> Cmd Msg
updateMonumentsCommand serverId model =
  let
    mServer = model.servers |> Dict.get serverId
    prop = \field -> Maybe.map field >> Maybe.withDefault NotRequested >> RemoteData.withDefault []
    monuments = Data.terminateMonuments
      (mServer |> prop .arcs)
      (mServer |> prop .monuments)
  in
    Leaflet.monumentList serverId (Encode.monuments monuments)

checkDefaultCenter : (Model, Cmd Msg) -> (Model, Cmd Msg)
checkDefaultCenter =
  addUpdate checkDefaultCenterUpdate

checkDefaultCenterUpdate : Model -> (Model, Cmd Msg)
checkDefaultCenterUpdate model =
  if model.center == DefaultCenter then
    case notablePoint model of
      Just point ->
        ( { model | center = SetCenter point }
        , Leaflet.setView point
        )
          |> replaceUrl "checkDefaultCenter"
      Nothing ->
        (model, Cmd.none)
  else
    (model, Cmd.none)

notablePoint : Model -> Maybe Point
notablePoint model =
  model.spanData
    |> Maybe.map .notableLocations
    |> Maybe.withDefault NotAvailable
    |> notableLocation model

notableLocation : Model -> RemoteData (List Parse.Key) -> Maybe Point
notableLocation model locations =
  case locations of
    Data ((Parse.Key _ x y)::_) -> Just <| Point x y defaultCenter.z
    Data _ -> monumentCenter model
    Failed _ -> monumentCenter model
    _ -> Nothing

monumentCenter : Model -> Maybe Point
monumentCenter model =
  currentServer model
    |> Maybe.map .monuments
    |> Maybe.withDefault NotAvailable
    |> RemoteData.toMaybe
    |> Maybe.andThen (recentMonumentLocation model.mapTime)

recentMonumentLocation : Maybe Posix -> List Monument -> Maybe Point
recentMonumentLocation mmapTime monuments =
  monuments
    |> dropWhile (\{date} ->
        case mmapTime of
          Just time ->
            let
              msTime = Time.posixToMillis time
              msStart = Time.posixToMillis date
            in
              msTime < msStart
          Nothing -> False
      )
    |> List.head
    |> Maybe.map (\{x,y} -> Point x y defaultCenter.z)

type alias ArcSpan r = {r|arcs: RemoteData (List Arc), spans: RemoteData (List Span)}

rebuildArcs : ArcSpan r -> ArcSpan r
rebuildArcs arcspan =
  case (arcspan.arcs, arcspan.spans) of
    (Data arcs, Data spans) ->
      {arcspan | arcs = Data (Data.assignDataTime spans arcs)}
    _ -> arcspan

selectArc : Model -> Maybe Arc -> Maybe Arc -> (Model, Cmd Msg)
selectArc model marc mcourseArc =
  if marc == model.currentArc && mcourseArc == model.coarseArc then
    (model, Cmd.none)
  else
    case marc of
      Just arc ->
        let
          start = increment arc.start
          end = arc.dataTime
            |> Maybe.withDefault (arc.end
              |> Maybe.withDefault model.time
              )
        in
        { model
        | currentArc = marc
        , coarseArc = mcourseArc
        , coarseStartTime = start
        , startTime = start
        }
          |> jumpTime end
      Nothing ->
        ( { model
          | currentArc = Nothing
          , coarseArc = Nothing
          }
        , Cmd.none
        )

requireLives : Model -> (Model, Cmd Msg)
requireLives model =
  let m2 = {model | lifeDataVisible = True} in
  if model.mapTime == Nothing then
    requireRecentLives m2
  else
    requireSelectedLives m2

requireRecentLives : Model -> (Model, Cmd Msg)
requireRecentLives model =
  case model.dataLayer of
    NotRequested ->
      ( {model | dataLayer = Loading}
      , fetchRecentLives model.cachedApiUrl (model.selectedServer |> Maybe.withDefault 17)
      )
    Failed _ ->
      ( {model | dataLayer = Loading}
      , fetchRecentLives model.cachedApiUrl (model.selectedServer |> Maybe.withDefault 17)
      )
    _ ->
      (model, Leaflet.dataLayerVisible True)

requireSelectedLives : Model -> (Model, Cmd Msg)
requireSelectedLives model =
  case model.dataLayer of
    NotRequested ->
      ( {model | dataLayer = Loading}
      , fetchDataForTime model
      )
    Failed _ ->
      ( {model | dataLayer = Loading}
      , fetchDataForTime model
      )
    _ ->
      (model, Leaflet.dataLayerVisible True)

toggleAnimated : Bool -> Model -> (Model, Cmd Msg)
toggleAnimated animated model =
  ( { model
    | dataAnimated = animated
    , player = if animated == False then Stopped else model.player
    }
      |> updateObjectSearch
  , Leaflet.animOverlay animated
  )
    |> addUpdate requireObjectSearchIndex
    |> addUpdate requireObjectSearch

makeObjectMap : List ObjectId -> List String -> Dict ObjectId String
makeObjectMap ids names =
  List.map2 Tuple.pair ids names
    |> Dict.fromList

makeObjectIndex : List ObjectId -> List String -> List (ObjectId, String)
makeObjectIndex ids names =
  List.map2 Tuple.pair ids (List.map String.toLower names)
    |> List.sortBy (Tuple.second>>String.length)

updateObjectSearch : Model -> Model
updateObjectSearch model =
  let
    lower = String.toLower model.objectSearchTerm
    test = case String.toInt model.objectSearchTerm of
      Just number ->
        \(id, name) a -> if id == number then id :: a else a
      Nothing ->
        \(id, name) a -> if String.contains lower name then id :: a else a
    total = model.selectedServer
      |> Maybe.andThen (\x -> if lower == "" then Nothing else Just x)
      |> Maybe.andThen (\id -> Dict.get id model.servers)
      |> Maybe.map .objectIndex
      |> Maybe.withDefault []
      |> List.foldr test []
      |> sortDescendingCount model
    ids = case model.maxiumMatchingObjects of
      Just n -> List.take n total
      Nothing -> total
    initialSelected =
      if List.length ids < 100 then
        Set.fromList ids
      else
        Set.empty
  in
  { model
  | totalMatchingObjects = List.length total
  , matchingObjects = ids
  , defaultImageObjects = pickDefaultImageObjects model (Set.fromList ids)
  , selectedMatchingObjects = initialSelected
  }

sortDescendingCount : Model -> List ObjectId -> List ObjectId
sortDescendingCount model list =
  let
    counts = model.spanData
      |> Maybe.map (\spanData ->
        if model.dataAnimated then
          spanData.logObjectSearchIndex
        else
          spanData.keyObjectSearchIndex
        )
      |> Maybe.withDefault NotRequested
      |> RemoteData.withDefault Dict.empty
    comp = \(aCount, aIndex) (bCount, bIndex) ->
      if aIndex && not bIndex then
        LT
      else if bIndex && not aIndex then
        GT
      else
        compare bCount aCount
    descendingCount = \a b ->
      comp
        (Dict.get a counts |> Maybe.withDefault (0, False))
        (Dict.get b counts |> Maybe.withDefault (0, False))
  in
    List.sortWith descendingCount list

pickDefaultImageObjects : Model -> Set ObjectId -> Set ObjectId
pickDefaultImageObjects model set =
  let
    counts = model.spanData
      |> Maybe.map (\spanData ->
        if model.dataAnimated then
          spanData.logObjectSearchIndex
        else
          spanData.keyObjectSearchIndex
        )
      |> Maybe.withDefault NotRequested
      |> RemoteData.withDefault Dict.empty
    defaultImage = \a ->
      Dict.get a counts
        |> Maybe.withDefault (0, False)
        |> (\(count, index) -> index && count < 40)
  in
    Set.filter defaultImage set

imageWhenLocked : ObjectId -> Model -> Bool
imageWhenLocked id model =
  model.spanData
    |> Maybe.map (\spanData ->
      if model.dataAnimated then
        spanData.logObjectSearchIndex
      else
        spanData.keyObjectSearchIndex
      )
    |> Maybe.withDefault NotRequested
    |> RemoteData.withDefault Dict.empty
    |> Dict.get id
    |> Maybe.withDefault (0, False)
    |> (\(count, index) -> index && count < 200)

yesterday : Model -> (Model, Cmd Msg)
yesterday model =
  ( { model
    | dataLayer = Loading
    , timeMode = FromNow
    , dataAnimated = True
    , hoursPeriod = 24
    , gameSecondsPerSecond = 1
    , framesPerSecond = 1
    }
  , Cmd.batch
    [ fetchRecentLives model.cachedApiUrl (model.selectedServer |> Maybe.withDefault 17)
    , Leaflet.animOverlay True
    ]
  )

dailyReview : Model -> (Model, Cmd Msg)
dailyReview model =
  ( { model
    | dataLayer = Loading
    , timeMode = FromNow
    , pointColor = CauseOfDeathColor
    , pointLocation = DeathLocation
    }
  , Cmd.batch
    [ Leaflet.pointColor CauseOfDeathColor
    , Leaflet.pointLocation DeathLocation
    , fetchDailyReview model
    ]
  )

oneDayAgo : Posix -> Posix
oneDayAgo =
  Time.posixToMillis
    >> (\x -> x - (24*60*60*1000))
    >> Time.millisToPosix

fetchDailyReview : Model -> Cmd Msg
fetchDailyReview model =
  let
    mserver = model |> currentServer
    mspanStart = mserver
      |> Maybe.map (.spans >> RemoteData.withDefault [])
      |> Maybe.andThen (List.reverse >> List.head)
      |> Maybe.map .start
    mserverEnd = mserver
      |> Maybe.map .maxTime
      |> Maybe.map oneDayAgo
    mstart = case (mspanStart, mserverEnd) of
      (Just span, Just lives) ->
        if Time.posixToMillis span < Time.posixToMillis lives then
          Just span
        else
          Just lives
      (Just span, Nothing) -> Just span
      (Nothing, Just lives) -> Just lives
      (Nothing, Nothing) -> Nothing
  in
    mstart
      |> Maybe.map (\start -> Time.now |> Task.perform (FetchBetween start))
      |> Maybe.withDefault Cmd.none

jumpTime : Posix -> Model -> (Model, Cmd Msg)
jumpTime time model =
  model
    |> scrubTime time
    |> mapModel setTimeRange
    |> appendCommand (Time.now |> Task.perform (ShowTimeNotice time))

scrubTime : Posix -> Model -> (Model, Cmd Msg)
scrubTime time model =
  if model.mapTime /= Just time then
    let
      marc = arcForTime time (currentArcs model)
      mspan = spanForTime time (currentSpans model)
      mspanData =
        if (Maybe.map .end model.spanData) /= (Maybe.map .end mspan) then
          Maybe.map asSpanData mspan
        else
          model.spanData
    in
    ( { model
      | mapTime = Just time
      , coarseArc = marc
      , currentArc = marc
      , spanData = mspanData
      }
    , Leaflet.currentTime time
    )
      |> addUpdate requireObjectSearchIndex
      |> addUpdate requireObjectSearch
      |> addUpdate requireNotableObjects
  else
    (model, Cmd.none)

setTimeRange : Model -> Model
setTimeRange model =
  let
    timeRange =
      if model.lifeDataVisible then
        model.timeRange
      else
        model.currentArc
          |> Maybe.map (arcToRange model.time)
    animatable = timeRange
      |> Maybe.map2 (\time range -> isInRange range time) model.mapTime
      |> Maybe.withDefault False
  in
    { model
    | timeRange = timeRange
    , dataAnimated = model.dataAnimated && animatable
    }

arcForTime : Posix -> RemoteData (List Arc) -> Maybe Arc
arcForTime time marcs =
  case marcs of
    Data arcs ->
      arcs
        |> List.filter (\arc -> isInRange (arcToRange time arc) time)
        |> List.head
    _ -> Nothing

spanForTime : Posix -> RemoteData (List Span) -> Maybe Span
spanForTime time mspans =
  case mspans of
    Data spans ->
      spans
        |> List.filter (\span -> isInRange (spanToRange span) time)
        |> List.head
    _ -> Nothing

maybeSetTime : Maybe Posix -> (Model, Cmd Msg) -> (Model, Cmd Msg)
maybeSetTime mtime =
  addUpdate (maybeSetTimeUpdate mtime)

maybeSetTimeUpdate : Maybe Posix -> Model -> (Model, Cmd Msg)
maybeSetTimeUpdate mtime model =
  case model.mapTime of
    Just _ ->
      ( model
      , Leaflet.currentTime (model.mapTime |> Maybe.withDefault model.time)
      )
    Nothing ->
      let
        time = mtime |> Maybe.withDefault model.time
      in
      ( { model
        | mapTime = mtime
        }
      , Cmd.batch
        [ Leaflet.currentTime time
        , Time.now |> Task.perform (ShowTimeNotice time)
        ]
      )

setYesterday : Model -> (Model, Cmd Msg)
setYesterday model =
  if isYesterday model then
    (model, Cmd.none)
  else
    yesterday model

isYesterday : Model -> Bool
isYesterday model =
  model.dataLayer /= NotRequested
    && model.timeMode == FromNow
    && model.hoursPeriod == 24
    && model.gameSecondsPerSecond == 1
    && model.framesPerSecond == 1
    && model.dataAnimated

setDailyReview : Model -> (Model, Cmd Msg)
setDailyReview model =
  if isDailyReview model then
    (model, Cmd.none)
  else
    dailyReview model

isDailyReview : Model -> Bool
isDailyReview model =
  model.dataLayer /= NotRequested
    && model.timeMode == FromNow
    && model.pointLocation == DeathLocation
    && model.pointColor == CauseOfDeathColor

detectPreset : Model -> Preset
detectPreset model =
  if isYesterday model then
    Yesterday
  else if isDailyReview model then
    DailyReview
  else
    NoPreset

setServer : Int -> (Model, Cmd Msg) -> (Model, Cmd Msg)
setServer serverId =
  addUpdate (setServerUpdate serverId)

setServerUpdate : Int -> Model -> (Model, Cmd Msg)
setServerUpdate serverId model =
  if Just serverId /= model.displayServer then
    case Dict.get serverId model.servers of
      Just server ->
        let
          (s2, c2) = (server, Cmd.none)
            |> addUpdate (requireMonuments model)
            |> addUpdate (requireArcs model)
            |> addUpdate (requireSpans model)
          range = (server.minTime, server.maxTime)
        in
        ( { model
          | selectedServer = Just serverId
          , servers = Dict.insert serverId s2 model.servers
          , coarseStartTime = inRange range model.coarseStartTime
          , startTime = inRange range model.startTime
          , dataLayer = NotRequested
          , player = Stopped
          }
        , Cmd.batch
          [ c2
          , case model.dataLayer of
            Data x ->
              if x == serverId then
                Cmd.none
              else
                Leaflet.dataLayer (Encode.lives [])
            _ -> Cmd.none
          , Leaflet.currentServer serverId
          ]
        )
          |> rebuildWorlds
          |> addUpdate requireObjectSearchIndex
          |> addUpdate requireObjectSearch
          |> addUpdate requireNotableObjects
          |> replaceUrl "setServerUpdate"
      Nothing ->
        ( { model
          | selectedServer = Just serverId
          , displayServer = Nothing
          }
        , Leaflet.currentServer serverId
        )
  else
    (model, Cmd.none)

checkServerLoaded : (Model, Cmd Msg) -> (Model, Cmd Msg)
checkServerLoaded (model, msg) =
  case currentServer model of
    Just server ->
      if not (serverLoading server) then
        case model.pendingPreset of
          Yesterday ->
            ({model|pendingPreset = NoPreset}, msg)
              |> addUpdate setYesterday
          DailyReview ->
            ({model|pendingPreset = NoPreset}, msg)
              |> addUpdate setDailyReview
          NoPreset -> (model, msg)
      else
        (model, msg)
    Nothing ->
      (model, msg)

changeTheme : Theme -> Cmd Msg
changeTheme theme =
  theme
    |> Theme.toString
    |> Leaflet.changeTheme

andHighlightObjects : Model -> (Model, Cmd Msg)
andHighlightObjects model =
  ( { model | debouncedMatchingObjects = model.matchingObjects }
  , highlightObjectsCommand model
  )

highlightObjectsCommand : Model -> Cmd Msg
highlightObjectsCommand model =
  Leaflet.highlightObjects
    (highlightObjectSwatches model |> Set.toList)
    (highlightObjectImages model |> Set.toList)

replaceUrl : String -> (Model, Cmd Msg) -> (Model, Cmd Msg)
replaceUrl reason =
  --let _ = Debug.log reason "" in
  addCommand replaceUrlCommand

replaceUrlCommand : Model -> Cmd Msg
replaceUrlCommand model =
  Navigation.replaceUrl model.navigationKey <|
    centerUrl model.location model.mapTime (detectPreset model) model.selectedServer model.center

combineRoute = addUpdate

changeRouteTo : Url -> Model -> (Model, Cmd Msg)
changeRouteTo location model =
  ({model | location = location}, Cmd.none)
    |> combineRoute (presetRoute location)
    |> combineRoute (timeRoute location)
    |> combineRoute (coordinateRoute location)
    |> combineRoute (serverRoute location)
    |> checkServerLoaded

presetRoute : Url -> Model -> (Model, Cmd Msg)
presetRoute location model =
  let
    mpreset = extractHashString "preset" location
  in
    case mpreset of
      Just "yesterday" -> ({model | pendingPreset = Yesterday }, Cmd.none)
      Just "daily-review" -> ({model | pendingPreset = DailyReview }, Cmd.none)
      _ -> (model, Cmd.none)

timeRoute : Url -> Model -> (Model, Cmd Msg)
timeRoute location model =
  let
    msmapTime = model.mapTime
      |> Maybe.map (\t -> (t |> Time.posixToMillis) // 1000)
    mst = extractHashInt "t" location
    mt = extractHashInt "t" location
      |> Maybe.map (\t -> t * 1000 |> Time.millisToPosix)
  in
    case mt of
      Just t ->
        if msmapTime /= mst then
          jumpTime t model
        else
          (model, Cmd.none)
      Nothing ->
        ( model, Cmd.none)

coordinateRoute : Url -> Model -> (Model, Cmd Msg)
coordinateRoute location model =
  let
    mx = extractHashInt "x" location
    my = extractHashInt "y" location
    mz = extractHashInt "z" location
  in
    case (mx, my, mz) of
      (Just x, Just y, Just z) ->
        setViewFromRoute (SetCenter (Point x y z)) model
      (Just x, Just y, Nothing) ->
        setViewFromRoute (SetCenter (Point x y defaultCenter.z)) model
      _ ->
        setViewFromRoute DefaultCenter model

setViewFromRoute : Center -> Model -> (Model, Cmd Msg)
setViewFromRoute center model =
  if model.center /= center then
    ( {model|center = center}
    , case center of
      DefaultCenter -> Leaflet.setView defaultCenter
      SetCenter point -> Leaflet.setView point
    )
  else
    (model, Cmd.none)

serverRoute : Url -> Model -> (Model, Cmd Msg)
serverRoute location model =
  let
    ms = extractHashInt "s" location
  in
    if model.selectedServer /= ms then
      case ms of
        (Just id) ->
          setServer id (model, Cmd.none)
        _ ->
          (model, Cmd.none)
    else
      (model, Cmd.none)

timeSelectionForTime : Model -> Model
timeSelectionForTime model =
  case model.mapTime of
    Nothing ->
      model
    Just time ->
      case currentArcs model of
        Data arcs ->
          let
            newArc = arcForTime time (Data arcs)
            mspan = spanForTime time (currentSpans model)
          in
            case newArc of
              Just arc ->
                { model
                | timeMode = ArcRange
                , currentArc = newArc
                , coarseArc = newArc
                , spanData = mspan |> Maybe.map asSpanData
                , timeRange = Just (arcToRange model.time arc)
                }
              Nothing ->
                timeSelectionAround model
        _ ->
          timeSelectionAround model

timeSelectionAround : Model -> Model
timeSelectionAround model =
  let
    time = relativeStartTime
      (model.hoursPeriod//2)
      (model.mapTime |> Maybe.withDefault model.time)
  in
    { model
    | timeMode = ServerRange
    , coarseStartTime = time
    , startTime = time
    }

fetchDataForTime : Model -> Cmd Msg
fetchDataForTime model =
  case model.timeMode of
    ServerRange ->
      fetchDataLayer model.apiUrl
        (model.selectedServer |> Maybe.withDefault 17)
        model.startTime
        (relativeEndTime model.hoursPeriod model.startTime)
        model.evesOnly
    FromNow ->
      Task.perform FetchUpTo Time.now
    ArcRange ->
      case model.currentArc of
        Just arc ->
          fetchDataLayer model.apiUrl
            arc.serverId
            arc.start
            (arc.end |> Maybe.withDefault model.time)
            model.evesOnly
        Nothing ->
          Task.succeed () |> Task.perform (always NoDataLayer)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Leaflet.event Event
    , LocalStorage.loadedJson Persist.Decode.persist Loaded
    , case model.notice of
        TimeNotice _ _ ->
          Browser.Events.onAnimationFrame CurrentTime
        NoNotice ->
          Sub.none
    , case model.player of
        Stopped ->
          Sub.none
        Starting ->
          Time.every (1000 / (toFloat model.framesPerSecond)) Playback
        Playing _ ->
          Time.every (1000 / (toFloat model.framesPerSecond)) Playback
    -- Attempt to prevent "Too many calls to Location or History APIs within a short timeframe."
    , if model.urlTime /= model.mapTime then
        Time.every 100 UpdateUrl
      else
        Sub.none
    , if model.debouncedMatchingObjects /= model.matchingObjects then
        Time.every 100 UpdateHighlightedObjects
      else
        Sub.none
    ]

fetchServers : String -> Cmd Msg
fetchServers baseUrl =
  Http.get
    { url = Url.relative ["data/servers.json"] []
    , expect = Http.expectJson ServerList Decode.servers
    }

requireArcs : Model -> Server -> (Server, Cmd Msg)
requireArcs model server =
  if server.arcs == NotRequested then
    ({server|arcs = Loading}
    , fetchArcs model.seedsUrl server.id)
  else
    (server, Cmd.none)

fetchArcs : String -> Int -> Cmd Msg
fetchArcs seedsUrl serverId =
  --slowGet (2 * 1000)
  fastGet
    { url = Url.relative [seedsUrl |> String.replace "{server}" (String.fromInt serverId)] []
    , decoder = Decode.arcs
    , tagger = (ArcList serverId)
    }

type alias HttpGet a =
  { url : String
  , decoder : Json.Decode.Decoder a
  , tagger : Result Http.Error a -> Msg
  }

fastGet : HttpGet a -> Cmd Msg
fastGet {url, decoder, tagger} =
  Http.get
    { url = url
    , expect = Http.expectJson tagger decoder
    }

slowGet : Float -> HttpGet a -> Cmd Msg
slowGet time {url, decoder, tagger} =
  Process.sleep time
    |> Task.andThen (\_ ->
      Http.task
        { url = url
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , resolver = resolveJson decoder
        , timeout = Nothing
        }
      )
    |> Task.attempt tagger

resolveJson : Json.Decode.Decoder a -> Http.Resolver Http.Error a
resolveJson decoder =
  Http.stringResolver <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err (Http.BadUrl url)
        Http.Timeout_ ->
          Err Http.Timeout
        Http.NetworkError_ ->
          Err Http.NetworkError
        Http.BadStatus_ metadata body ->
          Err (Http.BadStatus metadata.statusCode)
        Http.GoodStatus_ _ body ->
          case Json.Decode.decodeString decoder body of
            Ok value ->
              Ok value
            Err err ->
              Err (Http.BadBody (Json.Decode.errorToString err))

requireSpans : Model -> Server -> (Server, Cmd Msg)
requireSpans model server =
  if server.spans == NotRequested then
    ({server|spans = Loading}
    , fetchSpans model.spansUrl server.id)
  else
    (server, Cmd.none)

fetchSpans : String -> Int -> Cmd Msg
fetchSpans spansUrl serverId =
  Http.get
    { url = Url.relative [spansUrl |> String.replace "{server}" (String.fromInt serverId)] []
    , expect = Http.expectJson (SpanList serverId) Decode.spans
    }

requireObjectSearchIndex : Model -> (Model, Cmd Msg)
requireObjectSearchIndex model =
  if List.isEmpty model.matchingObjects && Set.isEmpty model.lockedObjects then
    (model, Cmd.none)
  else
    Maybe.map2 (\spanData serverId ->
      if model.dataAnimated then
        if spanData.logObjectSearchIndex == NotRequested then
          ( {model | spanData = Just {spanData | logObjectSearchIndex = Loading }}
          , fetchLogObjectSearchIndex model.logSearchIndex serverId spanData.end
          )
        else
          (model, Cmd.none)
      else
        if spanData.keyObjectSearchIndex == NotRequested then
          ( {model | spanData = Just {spanData | keyObjectSearchIndex = Loading }}
          , fetchKeyObjectSearchIndex model.keySearchIndex serverId spanData.end
          )
        else
          (model, Cmd.none)
      )
      model.spanData model.selectedServer
      |> Maybe.withDefault (model, Cmd.none)

fetchKeyObjectSearchIndex : String -> Int -> Posix -> Cmd Msg
fetchKeyObjectSearchIndex indexUrl serverId datatime =
  fetchObjectSearchIndex (KeyObjectSearchIndexReceived serverId datatime) indexUrl serverId datatime

fetchLogObjectSearchIndex : String -> Int -> Posix -> Cmd Msg
fetchLogObjectSearchIndex indexUrl serverId datatime =
  fetchObjectSearchIndex (LogObjectSearchIndexReceived serverId datatime) indexUrl serverId datatime

fetchObjectSearchIndex : ((Result Http.Error Data.ObjectSearchIndex) -> Msg) -> String -> Int -> Posix -> Cmd Msg
fetchObjectSearchIndex tagger indexUrl serverId datatime =
  Http.get
    { url = Url.relative [
      indexUrl
        |> String.replace "{server}" (String.fromInt serverId)
        |> String.replace "{time}" (String.fromInt ((Time.posixToMillis datatime) // 1000))
      ] []
    , expect = Http.expectString (parseObjectSearchIndex >> tagger)
    }

parseObjectSearchIndex : Result Http.Error String -> Result Http.Error Data.ObjectSearchIndex
parseObjectSearchIndex =
  Result.andThen
    (Parser.run Parse.objectSearchIndex
      >> Result.mapError (Http.BadBody << Parse.deadEndsToString))

requireObjectSearch : Model -> (Model, Cmd Msg)
requireObjectSearch model =
  Maybe.map3 (\spanData serverId id ->
    if model.dataAnimated then
      case Dict.get id spanData.browsePlacements of
        Nothing ->
          ( { model
            | spanData = Just {spanData | browsePlacements = Dict.insert id Loading spanData.browsePlacements }
            }
          , fetchLogObjectSearch model.logSearch serverId spanData.end id
          )
        Just (Data placements) ->
          focusPlacement (Zipper.current placements) model
        Just _ ->
          (model, Cmd.none)
    else
      case Dict.get id spanData.browseLocations of
        Nothing ->
          ( { model
            | spanData = Just {spanData | browseLocations = Dict.insert id Loading spanData.browseLocations }
            }
          , fetchKeyObjectSearch model.keySearch serverId spanData.end id
          )
        Just (Data locations) ->
          focusLocation (Zipper.current locations) model
        Just _ ->
          (model, Cmd.none)
    )
    model.spanData model.selectedServer model.focusObject
    |> Maybe.withDefault (model, Cmd.none)

previousBrowseItem : Model -> (Model, Cmd Msg)
previousBrowseItem model =
  if model.dataAnimated then
    model
      |> mapFocusBrowsePlacements (
        if model.browseProbablyTutorial then
          Zipper.previous
        else
          Zipper.previousMatching (not << browsePlacementInTutorial)
        )
      |> focusCurrentPlacement
  else
    model
      |> mapFocusBrowseLocations (
        if model.browseProbablyTutorial then
          Zipper.previous
        else
          Zipper.previousMatching (not << browseLocationInTutorial)
        )
      |> focusCurrentLocation

nextBrowseItem : Model -> (Model, Cmd Msg)
nextBrowseItem model =
  if model.dataAnimated then
    model
      |> mapFocusBrowsePlacements (
        if model.browseProbablyTutorial then
          Zipper.next
        else
          Zipper.nextMatching (not << browsePlacementInTutorial)
        )
      |> focusCurrentPlacement
  else
    model
      |> mapFocusBrowseLocations (
        if model.browseProbablyTutorial then
          Zipper.next
        else
          Zipper.nextMatching (not << browseLocationInTutorial)
        )
      |> focusCurrentLocation

focusLocation : BrowseLocation -> Model -> (Model, Cmd Msg)
focusLocation (BrowseLocation x y) model =
  ( model
  , Leaflet.focusPoint x y
  )

focusCurrentLocation : Model -> (Model, Cmd Msg)
focusCurrentLocation model =
  currentLocation model
    |> Maybe.map (\location -> focusLocation location model)
    |> Maybe.withDefault (model, Cmd.none)

focusPlacement : BrowsePlacement -> Model -> (Model, Cmd Msg)
focusPlacement (BrowsePlacement x y t) model =
  ( { model
    | mapTime = Just t
    }
  , Cmd.batch
    [ Leaflet.focusPlacement x y t
    , Time.now |> Task.perform (ShowTimeNotice t)
    ]
  )

focusCurrentPlacement : Model -> (Model, Cmd Msg)
focusCurrentPlacement model =
  currentPlacement model
    |> Maybe.map (\placement -> focusPlacement placement model)
    |> Maybe.withDefault (model, Cmd.none)

fetchKeyObjectSearch : String -> Int -> Posix -> ObjectId -> Cmd Msg
fetchKeyObjectSearch listUrl serverId datatime id =
  Http.get
    { url = fetchObjectSearchUrl listUrl serverId datatime id
    , expect = Http.expectString (parseKeyObjectSearch >> (KeyObjectSearchReceived serverId datatime id))
    }

parseKeyObjectSearch : Result Http.Error String -> Result Http.Error (List BrowseLocation)
parseKeyObjectSearch =
  Result.andThen
    (Parser.run Parse.keyValueYXFirst
      >> Result.map (List.map keyToBrowseLocation)
      >> Result.mapError (Http.BadBody << Parse.deadEndsToString))

fetchLogObjectSearch : String -> Int -> Posix -> ObjectId -> Cmd Msg
fetchLogObjectSearch listUrl serverId datatime id =
  Http.get
    { url = fetchObjectSearchUrl listUrl serverId datatime id
    , expect = Http.expectString (parseLogObjectSearch >> (LogObjectSearchReceived serverId datatime id))
    }

parseLogObjectSearch : Result Http.Error String -> Result Http.Error (List BrowsePlacement)
parseLogObjectSearch =
  Result.andThen
    (Parser.run Parse.logValueYXTFirst
      >> Result.map (List.map logToBrowsePlacement)
      >> Result.mapError (Http.BadBody << Parse.deadEndsToString))

requireNotableObjects : Model -> (Model, Cmd Msg)
requireNotableObjects model =
  Maybe.map3 (\spanData serverId _ ->
    if spanData.notableLocations == NotRequested then
      ( {model | spanData = Just {spanData | notableLocations = Loading }}
      , fetchNotableObjects model.keySearchNotable serverId spanData.end
      )
    else
      (model, Cmd.none)
    )
    model.spanData model.selectedServer (RemoteData.toMaybe model.objects)
    |> Maybe.withDefault (model, Cmd.none)

fetchNotableObjects : String -> Int -> Posix -> Cmd Msg
fetchNotableObjects listUrl serverId datatime =
  Http.get
    { url =
      listUrl
        |> String.replace "{server}" (String.fromInt serverId)
        |> String.replace "{time}" (String.fromInt ((Time.posixToMillis datatime) // 1000))
    , expect = Http.expectString (parseNotableObject >> (NotableObjectsReceived serverId datatime))
    }

parseNotableObject : Result Http.Error String -> Result Http.Error (List Parse.Key)
parseNotableObject =
  Result.andThen
    (Parser.run Parse.keyValueYXFirst
      >> Result.mapError (Http.BadBody << Parse.deadEndsToString))

fetchObjectSearchUrl : String -> Int -> Posix -> ObjectId -> String
fetchObjectSearchUrl listUrl serverId datatime id =
  Url.relative [
    listUrl
      |> String.replace "{server}" (String.fromInt serverId)
      |> String.replace "{time}" (String.fromInt ((Time.posixToMillis datatime) // 1000))
      |> String.replace "{id}" (String.fromInt id)
    ] []

fetchObjects : Cmd Msg
fetchObjects =
  Http.get
    { url = Url.relative ["static/objects.json"] []
    , expect = Http.expectJson ObjectsReceived Decode.objects
    }

myLife : Data.Life -> Life
myLife life =
  { birthTime = life.birthTime
  , generation = life.chain
  , lineage = life.lineage
  , playerid = life.playerid
  , name = life.name
  , serverId = life.serverId
  , epoch = life.epoch
  , age = life.age
  , birthX = life.birthX
  , birthY = life.birthY
  , deathTime = life.deathTime
  , deathX = life.deathX
  , deathY = life.deathY
  }

serverLife : Life -> Data.Life
serverLife life =
  { birthTime = life.birthTime
  , chain = life.generation
  , lineage = life.lineage
  , playerid = life.playerid
  , name = life.name
  , serverId = life.serverId
  , epoch = life.epoch
  , age = life.age
  , birthX = life.birthX
  , birthY = life.birthY
  , deathTime = life.deathTime
  , deathX = life.deathX
  , deathY = life.deathY
  }

myServer : RemoteData (List Version) -> RemoteData (Dict ObjectId String) -> RemoteData (List (ObjectId, String)) -> Data.Server -> Server
myServer versions objects objectIndex server =
  { id = server.id
  , serverName = server.serverName
  , minTime = server.minTime
  , maxTime = server.maxTime
  , codeChanges = Data.oholCodeChanges
  , arcs = NotRequested
  , spans = NotRequested
  , versions = versions
  , worlds = Data.rebuildWorlds Data.oholCodeChanges [] [] []
  , objects = RemoteData.withDefault Dict.empty objects
  , objectIndex = RemoteData.withDefault [] objectIndex
  , monuments = NotRequested
  }

keyToBrowseLocation : Parse.Key -> BrowseLocation
keyToBrowseLocation (Parse.Key _ x y) =
  BrowseLocation x y

logToBrowsePlacement : Parse.Log -> BrowsePlacement
logToBrowsePlacement (Parse.Log _ x y t) =
  BrowsePlacement x y t

future : RemoteData (List Version) -> RemoteData (Dict ObjectId String) -> RemoteData (List (ObjectId, String)) -> Posix -> Server
future versions objects objectIndex currentTime =
  { id = 20
  , serverName = "Band"
  , minTime = List.head Data.futureCodeChanges |> Maybe.map .start |> Maybe.withDefault (Time.millisToPosix 0)
  , maxTime = currentTime
  , codeChanges = Data.futureCodeChanges
  , arcs = NotAvailable
  , spans = NotAvailable
  , versions = versions
  , worlds = Data.rebuildWorlds Data.futureCodeChanges [] [] []
  , objects = RemoteData.withDefault Dict.empty objects
  , objectIndex = RemoteData.withDefault [] objectIndex
  , monuments = NotAvailable
  }

crucible : RemoteData (List Version) -> RemoteData (Dict ObjectId String) -> RemoteData (List (ObjectId, String)) -> Posix -> Server
crucible versions objects objectIndex currentTime =
  { id = 18
  , serverName = "server1.oho.life"
  , minTime = List.head Data.crucibleCodeChanges |> Maybe.map .start |> Maybe.withDefault (Time.millisToPosix 0)
  , maxTime = currentTime
  , codeChanges = Data.crucibleCodeChanges
  , arcs = NotAvailable
  , spans = NotAvailable
  , versions = versions
  , worlds = Data.rebuildWorlds Data.crucibleCodeChanges [] [] []
  , objects = RemoteData.withDefault Dict.empty objects
  , objectIndex = RemoteData.withDefault [] objectIndex
  , monuments = NotAvailable
  }

twoHoursOneLife : RemoteData (List Version) -> RemoteData (Dict ObjectId String) -> RemoteData (List (ObjectId, String)) -> Posix -> Server
twoHoursOneLife versions objects objectIndex currentTime =
  { id = 19
  , serverName = "play.twohoursonelife.com"
  , minTime = List.head Data.tholCodeChanges |> Maybe.map .start |> Maybe.withDefault (Time.millisToPosix 0)
  , maxTime = currentTime
  , codeChanges = Data.tholCodeChanges
  , arcs = NotAvailable
  , spans = NotAvailable
  , versions = versions
  , worlds = Data.rebuildWorlds Data.tholCodeChanges [] [] []
  , objects = RemoteData.withDefault Dict.empty objects
  , objectIndex = RemoteData.withDefault [] objectIndex
  , monuments = NotAvailable
  }

fetchMatchingLives : String -> String -> Cmd Msg
fetchMatchingLives baseUrl term =
  Http.get
    { url = Url.crossOrigin baseUrl ["lives"]
      [ lifeSearchParameter term
      , Url.int "limit" 100
      ]
    , expect = Http.expectJson MatchingLives Decode.lives
    }

lifeSearchParameter : String -> Url.QueryParameter
lifeSearchParameter term =
  case String.toInt term of
    Just number -> Url.int "playerid" number
    Nothing -> Url.string "q" term

fetchLineage : String -> Life -> Cmd Msg
fetchLineage baseUrl life =
  Http.get
    { url = Url.crossOrigin baseUrl ["lives"]
      [ Url.int "server_id" life.serverId
      , Url.int "epoch" life.epoch
      , Url.int "lineage" life.lineage
      ]
    , expect = Http.expectJson (LineageLives life.serverId) Json.Decode.value
    }

requireMonuments : Model -> Server -> (Server, Cmd Msg)
requireMonuments model server =
  case server.monuments of
    NotRequested ->
      ({server|monuments = Loading}
      , fetchMonuments model.cachedApiUrl server.id)
    NotAvailable ->
      (server, Leaflet.monumentList server.id (Encode.monuments []))
    Loading ->
      (server, Cmd.none)
    Data monuments ->
      ( server
      , monuments
        |> Data.terminateMonuments (server.arcs |> RemoteData.withDefault [])
        |> Encode.monuments
        |> Leaflet.monumentList server.id
      )
    Failed _ ->
      (server, Cmd.none)

fetchMonuments : String -> Int -> Cmd Msg
fetchMonuments baseUrl serverId =
  Http.get
    { url = Url.relative ["data/monuments/" ++ (String.fromInt serverId) ++ ".json"] []
    , expect = Http.expectJson (MonumentList serverId) Decode.monuments
    }

fetchRecentLives : String -> Int -> Cmd Msg
fetchRecentLives baseUrl serverId =
  Http.get
    { url = Url.crossOrigin baseUrl ["lives"]
      [ Url.int "server_id" serverId
      , Url.string "period" "P2D"
      ]
    , expect = Http.expectJson (DataLayer serverId) Json.Decode.value
    }

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

fetchDataLayer : String -> Int -> Posix -> Posix -> Bool -> Cmd Msg
fetchDataLayer baseUrl serverId startTime endTime evesOnly =
  Http.get
    { url = Url.crossOrigin baseUrl ["lives"]
      ( List.concat
        [ [ Url.int "server_id" serverId
          , Url.int "start_time" (startTime |> Time.posixToMillis |> (\x -> x // 1000))
          , Url.int "end_time" (endTime |> Time.posixToMillis |> (\x -> x // 1000))
          , Url.string "limit" "70000"
          ]
        , if evesOnly then [ Url.int "chain" 1 ] else []
        ]
      )
    , expect = Http.expectJson (DataLayer serverId) Json.Decode.value
    }

extractHashInt : String -> Url -> Maybe Int
extractHashInt key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.int key))
    |> Maybe.withDefault Nothing

extractHashString : String -> Url -> Maybe String
extractHashString key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing

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

arcToRange : Posix -> Arc -> (Posix, Posix)
arcToRange default {start,end} =
  (start, end |> Maybe.withDefault default)

spanToRange : Span -> (Posix, Posix)
spanToRange {start,end} =
  (start, end)

lifeToRange : Life -> (Posix, Posix)
lifeToRange {birthTime, deathTime} =
  (birthTime, deathTime |> Maybe.withDefault (relativeEndTime 1 birthTime))

increment : Posix -> Posix
increment =
  Time.posixToMillis
  >> (\x -> x + 1000)
  >> Time.millisToPosix

dropWhile : (a -> Bool) -> List a -> List a
dropWhile test list =
  case list of
    head :: rest ->
      if test head then
        dropWhile test rest
      else
        list
    [] -> []

andCommand : (model -> Cmd msg) -> model -> (model, Cmd msg)
andCommand f model =
  ( model, f model )

addCommand : (model -> Cmd msg) -> (model, Cmd msg) -> (model, Cmd msg)
addCommand f (model, cmd) =
  ( model
  , Cmd.batch
    [ cmd
    , f model
    ]
  )

addUpdate : (model -> (model, Cmd msg)) -> (model, Cmd msg) -> (model, Cmd msg)
addUpdate f (model, cmd) =
  let
    (m2, c2) = f model
  in
    (m2, Cmd.batch [ cmd, c2 ])

mapModel : (model -> model) -> (model, Cmd msg) -> (model, Cmd msg)
mapModel f (model, cmd) =
  ( f model
  , cmd
  )

appendCommand : Cmd msg -> (model, Cmd msg) -> (model, Cmd msg)
appendCommand newCmd (model, cmd) =
  ( model
  , Cmd.batch
    [ cmd
    , newCmd
    ]
  )
