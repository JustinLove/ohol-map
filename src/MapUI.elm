module MapUI exposing (..)

import Leaflet exposing (Point, PointColor(..), PointLocation(..))
import Model exposing (..)
import OHOLData as Data
import OHOLData.Decode as Decode
import OHOLData.Encode as Encode
import RemoteData exposing (RemoteData(..))
import View exposing (timeNoticeDuration)

import Browser
import Browser.Events
--import Browser.Dom
import Browser.Navigation as Navigation
import Http
import Json.Decode
import Dict exposing(Dict)
import Process
import Task
import Time exposing (Posix)
import Tuple
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser
import Url.Parser.Query

type Msg
  = UI View.Msg
  | Event (Result Json.Decode.Error Leaflet.Event)
  | MatchingLives (Result Http.Error (List Data.Life))
  | LineageLives Int (Result Http.Error Json.Decode.Value)
  | ServerList (Result Http.Error (List Data.Server))
  | ArcList Int (Result Http.Error (List Data.Arc))
  | SpanList Int (Result Http.Error (List Data.Span))
  | ObjectsReceived (Result Http.Error Data.Objects)
  | MonumentList Int (Result Http.Error (List Data.Monument))
  | DataLayer Int (Result Http.Error Json.Decode.Value)
  | NoDataLayer
  | FetchUpTo Posix
  | PlayRelativeTo Int Posix
  | ShowTimeNotice Posix Posix
  | CurrentTimeNotice Posix
  | CurrentTime Posix
  | Playback Posix
  | CurrentZone Time.Zone
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

main = Browser.application
  { init = init
  , update = update
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
      , Leaflet.animOverlay model.dataAnimated
      , Leaflet.worldList (Data.rebuildWorlds Data.oholCodeChanges [] [] [])
      ]
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI (View.None) -> (model, Cmd.none)
    UI (View.Search term) ->
      ( { model
        | searchTerm = term
        , lives = Loading
        }
      , fetchMatchingLives model.apiUrl term
      )
    UI (View.Typing term) ->
      ( { model
        | searchTerm = term
        }
      , Cmd.none
      )
    UI (View.SelectTimeMode mode) ->
      ( { model | timeMode = mode }
      , Cmd.none
      )
    UI (View.CoarseStartTime time) ->
      { model
      | coarseStartTime = time
      , startTime = time
      }
        |> setTime time
    UI (View.StartTime time) ->
      { model
      | startTime = time
      }
        |> setTime time
    UI (View.HoursBefore hours) ->
      { model
      | hoursPeriod = hours
      }
        |> setTime (relativeStartTime hours model.time)
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
    UI (View.ToggleAnimated animated) ->
      ( { model
        | dataAnimated = animated
        , player = if animated == False then Stopped else model.player
        }
      , Leaflet.animOverlay animated
      )
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
      ( {model|mapTime = Just time}
      , Leaflet.currentTime time
      )
       |> replaceUrl
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
        | focus = Just life
        , timeRange = Just (lifeToRange life)
        , mapTime = Just life.birthTime
        }
      , Cmd.batch
        [ Leaflet.currentTime life.birthTime
        , Time.now |> Task.perform (ShowTimeNotice life.birthTime)
        , Leaflet.focus (serverLife life)
        ]
      )
       |> setServer life.serverId
    UI (View.SelectLineage life) ->
      ( model
      , fetchLineage model.apiUrl life
      )
    UI (View.SelectMode mode) ->
      ( { model | sidebarMode = mode }
      , Leaflet.searchOverlay (mode == LifeSearch)
      )
    UI (View.SelectServer serverId) ->
      ( model, Cmd.none )
        |> setServer serverId
    UI (View.SelectArc index) ->
      let
        marc = case currentArcs model of
          Data list ->
            list
              |> List.drop index
              |> List.head
          _ -> Nothing
      in
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
          , coarseStartTime = start
          , startTime = start
          , timeRange = Just (arcToRange model.time arc)
          }
            |> setTime end
        Nothing ->
          ( { model
            | currentArc = marc
            , coarseArc = marc
            , timeRange = Nothing
            }
          , Cmd.none
          )
    UI (View.SelectArcCoarse index) ->
      let
        marc = case currentArcs model of
          Data list ->
            list
              |> List.drop index
              |> List.head
          _ -> Nothing
      in
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
          , coarseArc = marc
          , coarseStartTime = start
          , startTime = start
          , timeRange = Just (arcToRange model.time arc)
          }
            |> setTime end
        Nothing ->
          ( { model
            | currentArc = marc
            , coarseArc = marc
            , timeRange = Nothing
            }
          , Cmd.none
          )
    UI (View.SelectShow) ->
      ( {model | dataLayer = Loading}
      , fetchDataForTime model
      )
    Event (Ok (Leaflet.MoveEnd point)) ->
      ( {model|center = point}
      , Cmd.none
      )
        |> replaceUrl
    Event (Ok (Leaflet.OverlayAdd "Life Data" _)) ->
      requireLives model
    Event (Ok (Leaflet.OverlayRemove "Life Data")) ->
      ({model | lifeDataVisible = False}, Cmd.none)
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
        , sidebarOpen = True
        , sidebarMode = LifeSearch
        , searchTerm = ""
        }
      , Cmd.batch
        [ Leaflet.displayResults lives
        , Leaflet.searchOverlay True
        ]
      )
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
      ( { model | sidebarOpen = not model.sidebarOpen }
      , Leaflet.searchOverlay
        (model.sidebarOpen == False && model.sidebarMode == LifeSearch)
      )
    Event (Ok (Leaflet.AnimToggle)) ->
      ( { model
        | dataAnimated = not model.dataAnimated
        , player = if (not model.dataAnimated) == False then Stopped else model.player
        }
      , Leaflet.animOverlay
        (model.dataAnimated == False)
      )
    Event (Err err) ->
      let _ = Debug.log "error" err in
      (model, Cmd.none)
    MatchingLives (Ok lives) ->
      ( {model | lives = lives |> List.map myLife |> Data}
      , Cmd.batch
        [ Leaflet.displayResults lives
        , Leaflet.searchOverlay True
        ]
      )
    MatchingLives (Err error) ->
      let _ = Debug.log "fetch lives failed" error in
      ({model | lives = Failed error}, Cmd.none)
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
      let _ = Debug.log "fetch lives failed" error in
      ({model | lives = Failed error}, Cmd.none)
    ServerList (Ok serverList) ->
      let
        servers = serverList
          |> List.map (myServer model.versions)
          |> (++) [crucible NotAvailable model.time]
          |> (++) [twoHoursOneLife NotAvailable model.time]
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
    ServerList (Err error) ->
      let _ = Debug.log "fetch servers failed" error in
      ({model | serverList = Failed error, servers = Dict.empty}, Cmd.none)
    ArcList serverId (Ok arcs) ->
      let
        lastArc = arcs |> List.reverse |> List.head
      in
        ( { model
          | servers = model.servers |> Dict.update serverId (Maybe.map (\server -> {server | arcs = Data arcs} |> rebuildArcs))
          , currentArc = lastArc
          , coarseArc = lastArc
          , timeRange = lastArc |> Maybe.map (arcToRange model.time)
          }
        , Cmd.none
        )
        |> rebuildWorlds
        |> updateMonuments serverId
    ArcList serverId (Err error) ->
      let _ = Debug.log "fetch arcs failed" error in
      ( { model
        | servers = model.servers |> Dict.update serverId (Maybe.map (\server -> {server | arcs = Failed error}))
        , currentArc = Nothing
        , coarseArc = Nothing
        , timeRange = Nothing}
        , Cmd.none
      )
    SpanList serverId (Ok spans) ->
      let
        lastSpan = spans |> List.reverse |> List.head
        mlastTime = lastSpan |> Maybe.map .end
      in
        ( { model
          | servers = model.servers |> Dict.update serverId (Maybe.map (\server -> {server | spans = Data spans} |> rebuildArcs))
          , timeRange = lastSpan |> Maybe.map spanToRange
          }
        , Cmd.none
        )
        |> maybeSetTime mlastTime
        |> rebuildWorlds
    SpanList serverId (Err error) ->
      let _ = Debug.log "fetch spans failed" error in
      ( { model
        | servers = model.servers |> Dict.update serverId (Maybe.map (\server -> {server | spans = Failed error}))
        }
        , Cmd.none
      )
    ObjectsReceived (Ok objects) ->
      let versions = Data.completeVersions objects.spawnChanges in
      ( { model
        | versions = Data versions
        , servers = model.servers
          |> Dict.map (\_ server -> {server | versions = Data versions})
        }
      , Leaflet.objectBounds objects.ids objects.bounds
      )
        |> rebuildWorlds
    ObjectsReceived (Err error) ->
      let _ = Debug.log "fetch objects failed" error in
      (model, Cmd.none)
    MonumentList serverId (Ok monuments) ->
      ( { model
        | servers = model.servers |> Dict.update serverId (Maybe.map (\server -> {server | monuments = Data monuments}))
        }
      , Cmd.none
      )
        |> updateMonuments serverId
        |> checkDefaultCenter monuments
    MonumentList serverId (Err error) ->
      let _ = Debug.log "fetch monuments failed" error in
      (model, Cmd.none)
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
      let _ = Debug.log "fetch data failed" error in
      ({model | dataLayer = Failed error}, Cmd.none)
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
    PlayRelativeTo hoursBefore time ->
      { model
      | player = Starting
      }
        |> setTime (relativeStartTime hoursBefore time)
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
                  |> replaceUrl
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

checkDefaultCenter : List Monument -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkDefaultCenter monuments =
  addUpdate (checkDefaultCenterUpdate monuments)

checkDefaultCenterUpdate : List Monument -> Model -> (Model, Cmd Msg)
checkDefaultCenterUpdate monuments model =
  if model.center == defaultCenter then
    let
      recent = monuments
        |> dropWhile (\{date} ->
            case model.mapTime of
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
        |> Maybe.withDefault defaultCenter
    in
    if recent /= defaultCenter then
      ( { model | center = recent }
      , Leaflet.setView recent
      )
       |> replaceUrl
    else
      (model, Cmd.none)
  else
    (model, Cmd.none)

type alias ArcSpan r = {r|arcs: RemoteData (List Arc), spans: RemoteData (List Span)}

rebuildArcs : ArcSpan r -> ArcSpan r
rebuildArcs arcspan =
  case (arcspan.arcs, arcspan.spans) of
    (Data arcs, Data spans) ->
      {arcspan | arcs = Data (Data.assignDataTime spans arcs)}
    _ -> arcspan

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
      (model, Cmd.none)

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
      (model, Cmd.none)


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

setTime : Posix -> Model -> (Model, Cmd Msg)
setTime time model =
  if model.mapTime /= Just time then
    let
      marc = arcForTime time (currentArcs model)
      timeRange =
        if model.lifeDataVisible then
          model.timeRange
        else
          marc
            |> Maybe.map (arcToRange model.time)
      animatable = timeRange
        |> Maybe.map (\range -> isInRange range time)
        |> Maybe.withDefault False
    in
    ( { model
      | mapTime = Just time
      , currentArc = marc
      , timeRange = timeRange
      , dataAnimated = model.dataAnimated && animatable
      }
    , Cmd.batch
        [ Leaflet.currentTime time
        , Time.now |> Task.perform (ShowTimeNotice time)
        ]
    )
     |> replaceUrl
  else
    (model, Cmd.none)

arcForTime : Posix -> RemoteData (List Arc) -> Maybe Arc
arcForTime time marcs =
  case marcs of
    Data arcs ->
      arcs
        |> List.filter (\arc -> isInRange (arcToRange time arc) time)
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
          |> replaceUrl
      Nothing ->
        ( { model
          | selectedServer = Just serverId
          , displayServer = Nothing
          }
        , Leaflet.currentServer serverId
        )
  else
    (model, Cmd.none)

replaceUrl : (Model, Cmd Msg) -> (Model, Cmd Msg)
replaceUrl =
  addCommand replaceUrlCommand

replaceUrlCommand : Model -> Cmd Msg
replaceUrlCommand model =
  Navigation.replaceUrl model.navigationKey <|
    centerUrl model.location model.mapTime (isYesterday model) model.selectedServer model.center

combineRoute = addUpdate

changeRouteTo : Url -> Model -> (Model, Cmd Msg)
changeRouteTo location model =
  ({model | location = location}, Cmd.none)
    |> combineRoute (yesterdayRoute location)
    |> combineRoute (timeRoute location)
    |> combineRoute (coordinateRoute location)
    |> combineRoute (serverRoute location)

yesterdayRoute : Url -> Model -> (Model, Cmd Msg)
yesterdayRoute location model =
  let
    mpreset = extractHashString "preset" location
  in
    if mpreset == Just "yesterday" then
      setYesterday model
    else
      (model, Cmd.none)

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
          ( { model|mapTime = Just t}
          , Cmd.batch
            [ Leaflet.currentTime t
            , Time.now |> Task.perform (ShowTimeNotice t)
            ]
          )
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
        setViewFromRoute (Point x y z) model
      (Just x, Just y, Nothing) ->
        setViewFromRoute (Point x y defaultCenter.z) model
      _ ->
        setViewFromRoute defaultCenter model

setViewFromRoute : Point -> Model -> (Model, Cmd Msg)
setViewFromRoute point model =
  if model.center /= point then
    ({model|center = point}, Leaflet.setView point)
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
          in
            case newArc of
              Just arc ->
                { model
                | timeMode = ArcRange
                , currentArc = newArc
                , coarseArc = newArc
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

myServer : RemoteData (List Version) -> Data.Server -> Server
myServer versions server =
  { id = server.id
  , serverName = server.serverName
  , minTime = server.minTime
  , maxTime = server.maxTime
  , codeChanges = Data.oholCodeChanges
  , arcs = NotRequested
  , spans = NotRequested
  , versions = versions
  , worlds = Data.rebuildWorlds Data.oholCodeChanges [] [] []
  , monuments = NotRequested
  }

crucible : RemoteData (List Version) -> Posix -> Server
crucible versions currentTime =
  { id = 18
  , serverName = "server1.oho.life"
  , minTime = List.head Data.crucibleCodeChanges |> Maybe.map .start |> Maybe.withDefault (Time.millisToPosix 0)
  , maxTime = currentTime
  , codeChanges = Data.crucibleCodeChanges
  , arcs = NotAvailable
  , spans = NotAvailable
  , versions = versions
  , worlds = Data.rebuildWorlds Data.crucibleCodeChanges [] [] []
  , monuments = NotAvailable
  }

twoHoursOneLife : RemoteData (List Version) -> Posix -> Server
twoHoursOneLife versions currentTime =
  { id = 19
  , serverName = "play.twohoursonelife.com"
  , minTime = List.head Data.tholCodeChanges |> Maybe.map .start |> Maybe.withDefault (Time.millisToPosix 0)
  , maxTime = currentTime
  , codeChanges = Data.tholCodeChanges
  , arcs = NotAvailable
  , spans = NotAvailable
  , versions = versions
  , worlds = Data.rebuildWorlds Data.tholCodeChanges [] [] []
  , monuments = NotAvailable
  }

fetchMatchingLives : String -> String -> Cmd Msg
fetchMatchingLives baseUrl term =
  Http.get
    { url = Url.crossOrigin baseUrl ["lives"]
      [ Url.string "q" term
      , Url.int "limit" 100
      ]
    , expect = Http.expectJson MatchingLives Decode.lives
    }

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
