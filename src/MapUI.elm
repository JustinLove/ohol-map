module MapUI exposing (..)

import Leaflet exposing (Point, PointColor(..), PointLocation(..))
import OHOLData as Data exposing (Server, Arc, Version, World)
import OHOLData.Decode as Decode
import OHOLData.Encode as Encode
import View exposing
  ( RemoteData(..)
  , Life
  , TimeMode(..)
  , Notice(..)
  , Player(..)
  , timeNoticeDuration
  , centerUrl
  )

import Browser
import Browser.Events
--import Browser.Dom
import Browser.Navigation as Navigation
import Http
import Json.Decode
import Dict exposing(Dict)
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
  | LineageLives (Result Http.Error Json.Decode.Value)
  | ServerList (Result Http.Error (List Data.Server))
  | ArcList (Result Http.Error (List Data.Arc))
  | ObjectsReceived (Result Http.Error Data.Objects)
  | MonumentList Int (Result Http.Error Json.Decode.Value)
  | DataLayer (Result Http.Error Json.Decode.Value)
  | FetchUpTo Posix
  | ShowTimeNotice Posix Posix
  | CurrentTimeNotice Posix
  | CurrentTime Posix
  | Playback Posix
  | CurrentZone Time.Zone
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

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
  , sidebarOpen : Bool
  , sidebarMode : View.Mode
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
  , selectedServer : Maybe Server
  , servers : RemoteData (List Server)
  , arcs : RemoteData (List Arc)
  , versions : RemoteData (List Version)
  , worlds : List World
  , monuments : Dict Int Json.Decode.Value
  , dataLayer : RemoteData Bool
  , lives : RemoteData (List Life)
  , focus : Maybe Life
  }

type alias Config =
  { cachedApiUrl: String
  , apiUrl: String
  , lineageUrl: String
  }

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
    initialModel =
      { location = location
      , navigationKey = key
      , zone = Time.utc
      , time = Time.millisToPosix 0
      , notice = NoNotice
      , center = Point 0 0 17
      , cachedApiUrl = config.cachedApiUrl
      , apiUrl = config.apiUrl
      , lineageUrl = config.lineageUrl
      , sidebarOpen = False
      , sidebarMode = View.LifeSearch
      , searchTerm = ""
      , timeMode = ServerRange
      , coarseStartTime = Time.millisToPosix 0
      , startTime = Time.millisToPosix 0
      , mapTime = Nothing
      , hoursPeriod = 48
      , coarseArc = Nothing
      , currentArc = Nothing
      , dataAnimated = False
      , lifeDataVisible = False
      , gameSecondsPerSecond = 600
      , framesPerSecond = 10
      , timeRange = Nothing
      , player = Stopped
      , fadeTallObjects = False
      , showNaturalObjectsAboveZoom = 26
      , pointColor = LineageColor
      , pointLocation = BirthLocation
      , selectedServer = Nothing
      , servers = NotRequested
      , arcs = NotRequested
      , versions = NotRequested
      , worlds = Data.rebuildWorlds Data.codeChanges [] []
      , monuments = Dict.empty
      , dataLayer = NotRequested
      , lives = NotRequested
      , focus = Nothing
      }
    (model, cmd) = changeRouteTo location initialModel
  in
    ( model
    , Cmd.batch
      [ cmd
      , Time.here |> Task.perform CurrentZone
      , case model.mapTime of
        Just time -> Cmd.none
        Nothing -> Time.now |> Task.perform CurrentTimeNotice
      , fetchServers model.cachedApiUrl
      , fetchArcs
      , fetchObjects
      , Leaflet.animOverlay model.dataAnimated
      , Leaflet.worldList model.worlds
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
      , Cmd.batch
        [ Leaflet.currentTime time
        , Navigation.replaceUrl model.navigationKey <|
          centerUrl model.location (Just time) (isYesterday model) model.center
        ]
      )
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
        , timeRange = Just (life.birthTime, life.deathTime |> Maybe.withDefault (relativeEndTime 1 life.birthTime))
        , mapTime = Just life.birthTime
        }
      , Cmd.batch
        [ Leaflet.currentTime life.birthTime
        , Navigation.replaceUrl model.navigationKey <|
          centerUrl model.location (Just life.birthTime) (isYesterday model) model.center
        , Time.now |> Task.perform (ShowTimeNotice life.birthTime)
        , Leaflet.focus (serverLife life)
        ]
      )
    UI (View.SelectLineage life) ->
      ( model
      , fetchLineage model.apiUrl life
      )
    UI (View.SelectMode mode) ->
      ( { model | sidebarMode = mode }
      , Leaflet.searchOverlay (mode == View.LifeSearch)
      )
    UI (View.SelectServer server) ->
      let
        range = (server.minTime, server.maxTime)
        m2 = { model
          | selectedServer = Just server
          , coarseStartTime = inRange range model.coarseStartTime
          , startTime = inRange range model.startTime
          , mapTime = model.mapTime
            |> Maybe.map (\time -> inRange range time)
          , dataLayer = Loading
          }
      in
      ( m2, Cmd.batch
        [ fetchDataForTime m2
        , fetchMonuments model.cachedApiUrl server.id
        , Leaflet.currentServer server
        ]
      )
    UI (View.SelectArc index) ->
      let
        marc = case model.arcs of
          Data list ->
            list
              |> List.drop index
              |> List.head
          _ -> Nothing
      in
      case marc of
        Just arc ->
          let time = increment arc.start in
          { model
          | currentArc = marc
          , coarseStartTime = time
          , startTime = time
          , timeRange = Just (arc.start, arc.end)
          }
            |> setTime time
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
        marc = case model.arcs of
          Data list ->
            list
              |> List.drop index
              |> List.head
          _ -> Nothing
      in
      case marc of
        Just arc ->
          let time = increment arc.start in
          { model
          | currentArc = marc
          , coarseArc = marc
          , coarseStartTime = time
          , startTime = time
          , timeRange = Just (arc.start, arc.end)
          }
            |> setTime time
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
      , Navigation.replaceUrl model.navigationKey <|
        centerUrl model.location model.mapTime (isYesterday model) point
      )
    Event (Ok (Leaflet.OverlayAdd "Life Data" _)) ->
      requireLives model
    Event (Ok (Leaflet.OverlayRemove "Life Data")) ->
      ({model | lifeDataVisible = False}, Cmd.none)
    Event (Ok (Leaflet.OverlayAdd name (Just serverId))) ->
      if Dict.member serverId model.monuments then
        (model, Cmd.none)
      else
        (model, fetchMonuments model.cachedApiUrl serverId)
    Event (Ok (Leaflet.OverlayAdd name _)) ->
      (model, Cmd.none)
    Event (Ok (Leaflet.OverlayRemove name)) ->
      (model, Cmd.none)
    Event (Ok (Leaflet.SelectPoints lives)) ->
      ( { model
        | lives = lives |> List.map myLife |> Data
        , sidebarOpen = True
        , sidebarMode = View.LifeSearch
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
      , mapTime
        |> Maybe.map Leaflet.currentTime
        |> Maybe.withDefault Cmd.none
      )
    Event (Ok (Leaflet.SidebarToggle)) ->
      ( { model | sidebarOpen = not model.sidebarOpen }
      , Leaflet.searchOverlay
        (model.sidebarOpen == False && model.sidebarMode == View.LifeSearch)
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
    LineageLives (Ok value) ->
      ( { model
        | lives = case value |> Json.Decode.decodeValue Decode.lives of
          Ok lives ->
            lives |> List.map myLife |> Data
          Err error ->
            error
              |> Json.Decode.errorToString
              |> Http.BadBody
              |> Failed
        , dataLayer = Data True
        }
      , Cmd.batch
        [ Leaflet.dataLayer value
        ]
      )
    LineageLives (Err error) ->
      let _ = Debug.log "fetch lives failed" error in
      ({model | lives = Failed error}, Cmd.none)
    ServerList (Ok servers) ->
      let
        bs2 = servers |> List.reverse |> List.head
        startTime = bs2
          |> Maybe.map .maxTime
          |> Maybe.map (relativeEndTime model.hoursPeriod)
          |> Maybe.withDefault (Time.millisToPosix 0)
        id = bs2 |> Maybe.map .id |> Maybe.withDefault 17
      in
      ( { model
        | servers = servers |> Data
        , selectedServer = bs2
        , coarseStartTime = startTime
        , startTime = startTime
        }
          |> timeSelectionForTime
      , Cmd.batch
        [ fetchMonuments model.cachedApiUrl id
        --, Leaflet.serverList servers
        ]
      )
    ServerList (Err error) ->
      let _ = Debug.log "fetch servers failed" error in
      ({model | servers = Failed error}, Cmd.none)
    ArcList (Ok arcs) ->
      let
        lastArc = arcs |> List.reverse |> List.head
        worlds = Data.rebuildWorlds
          Data.codeChanges
          (case model.versions of
            Data versions -> versions
            _ -> []
          )
          arcs
      in
      case model.mapTime of
        Just _ ->
          ( { model
            | arcs = Data arcs
            , worlds = worlds
            , currentArc = lastArc
            , coarseArc = lastArc
            , timeRange = lastArc |> Maybe.map (\{start, end} -> (start, end))
            }
          , Cmd.batch
            [ Leaflet.currentTime (model.mapTime |> Maybe.withDefault model.time)
            , Leaflet.worldList worlds
            ]
          )
        Nothing ->
          let
            mlastTime = lastArc |> Maybe.map .end
            lastTime = mlastTime |> Maybe.withDefault model.time
          in
          ( { model
            | arcs = Data arcs
            , currentArc = lastArc
            , coarseArc = lastArc
            , timeRange = lastArc |> Maybe.map (\{start, end} -> (start, end))
            , mapTime = mlastTime
            }
          , Cmd.batch
            [ Leaflet.currentTime lastTime
            , Leaflet.worldList worlds
            , Time.now |> Task.perform (ShowTimeNotice lastTime)
            ]
          )
    ArcList (Err error) ->
      let _ = Debug.log "fetch arcs failed" error in
      ({model | arcs = Failed error, currentArc = Nothing, coarseArc = Nothing, timeRange = Nothing}, Cmd.none)
    ObjectsReceived (Ok objects) ->
      let
        versions = Data.completeVersions objects.spawnChanges
        worlds = Data.rebuildWorlds
          Data.codeChanges
          versions
          (case model.arcs of
            Data arcs -> arcs
            _ -> []
          )
      in
      ( { model
        | versions = Data versions
        , worlds = worlds
        }
      , Cmd.batch
        [ Leaflet.worldList worlds
        , Leaflet.objectBounds objects.ids objects.bounds
        ]
      )
    ObjectsReceived (Err error) ->
      let _ = Debug.log "fetch objects failed" error in
      (model, Cmd.none)
    MonumentList serverId (Ok monuments) ->
      ( {model | monuments = Dict.insert serverId monuments model.monuments}
      , Leaflet.monumentList serverId monuments
      )
    MonumentList serverId (Err error) ->
      let _ = Debug.log "fetch monuments failed" error in
      (model, Cmd.none)
    DataLayer (Ok lives) ->
      ( { model
        | dataLayer = Data True
        , player = if model.dataAnimated then Starting else Stopped
        }
      , Cmd.batch
        [ Leaflet.dataLayer lives
        ]
      )
    DataLayer (Err error) ->
      let _ = Debug.log "fetch data failed" error in
      ({model | dataLayer = Failed error}, Cmd.none)
    FetchUpTo time ->
      ( model
      , fetchDataLayer model.apiUrl
          (model.selectedServer |> Maybe.map .id |> Maybe.withDefault 17)
          (relativeStartTime model.hoursPeriod time)
          time
      )
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
                , Cmd.batch
                  [ Navigation.replaceUrl model.navigationKey <|
                      centerUrl model.location (Just msCappedTime) (isYesterday model) model.center
                  , Leaflet.currentTime msCappedTime
                  ]
                )
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
      , fetchRecentLives model.cachedApiUrl (model.selectedServer |> Maybe.map .id |> Maybe.withDefault 17)
      )
    Failed _ ->
      ( {model | dataLayer = Loading}
      , fetchRecentLives model.cachedApiUrl (model.selectedServer |> Maybe.map .id |> Maybe.withDefault 17)
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
    [ fetchRecentLives model.cachedApiUrl (model.selectedServer |> Maybe.map .id |> Maybe.withDefault 17)
    , Leaflet.animOverlay True
    ]
  )

setTime : Posix -> Model -> (Model, Cmd Msg)
setTime time model =
  if model.mapTime /= Just time then
    let
      marc = arcForTime time model.arcs
      timeRange =
        if model.lifeDataVisible then
          model.timeRange
        else
          marc
            |> Maybe.map (\arc -> (arc.start, arc.end))
      animatable = timeRange
        |> Maybe.map (\(min, max) -> isInRange min time max)
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
        , Navigation.replaceUrl model.navigationKey <|
          centerUrl model.location (Just time) (isYesterday model) model.center
        , Time.now |> Task.perform (ShowTimeNotice time)
        ]
    )
  else
    (model, Cmd.none)

arcForTime : Posix -> RemoteData (List Arc) -> Maybe Arc
arcForTime time marcs =
  case marcs of
    Data arcs ->
      arcs
        |> List.filter (\arc -> isInRange arc.start time arc.end)
        |> List.head
    _ -> Nothing

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

combineRoute : (Model -> (Model, Cmd Msg)) -> (Model, Cmd Msg) -> (Model, Cmd Msg)
combineRoute route (model, cmd) =
  let
    (m2, c2) = route model
  in
    (m2, Cmd.batch [ cmd, c2 ])

changeRouteTo : Url -> Model -> (Model, Cmd Msg)
changeRouteTo location model =
  ({model | location = location}, Cmd.none)
    |> combineRoute (yesterdayRoute location)
    |> combineRoute (timeRoute location)
    |> combineRoute (coordinateRoute location)

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
        setViewFromRoute (Point x y 24) model
      _ ->
        setViewFromRoute (Point 0 0 24) model

setViewFromRoute : Point -> Model -> (Model, Cmd Msg)
setViewFromRoute point model =
  if model.center /= point then
    ({model|center = point}, Leaflet.setView point)
  else
    (model, Cmd.none)

timeSelectionForTime : Model -> Model
timeSelectionForTime model =
  case model.mapTime of
    Nothing ->
      model
    Just time ->
      case model.arcs of
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
                , timeRange = Just (arc.start, arc.end)
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
        (model.selectedServer |> Maybe.map .id |> Maybe.withDefault 17)
        model.startTime
        (relativeEndTime model.hoursPeriod model.startTime)
    FromNow ->
      Task.perform FetchUpTo Time.now
    ArcRange ->
      case model.currentArc of
        Just arc ->
          fetchDataLayer model.apiUrl
            arc.serverId
            arc.start
            arc.end
        Nothing ->
          Cmd.none

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
    { url = Url.crossOrigin baseUrl ["servers"] []
    , expect = Http.expectJson ServerList Decode.servers
    }

fetchArcs : Cmd Msg
fetchArcs =
  Http.get
    { url = Url.relative ["kp/arcs.json"] []
    , expect = Http.expectJson ArcList Decode.arcs
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
    , expect = Http.expectJson LineageLives Json.Decode.value
    }

fetchMonuments : String -> Int -> Cmd Msg
fetchMonuments baseUrl serverId =
  Http.get
    { url = Url.crossOrigin baseUrl ["monuments"]
      [ Url.int "server_id" serverId
      ]
    , expect = Http.expectJson (MonumentList serverId) Json.Decode.value
    }

fetchRecentLives : String -> Int -> Cmd Msg
fetchRecentLives baseUrl serverId =
  Http.get
    { url = Url.crossOrigin baseUrl ["lives"]
      [ Url.int "server_id" serverId
      , Url.string "period" "P2D"
      ]
    , expect = Http.expectJson DataLayer Json.Decode.value
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

fetchDataLayer : String -> Int -> Posix -> Posix -> Cmd Msg
fetchDataLayer baseUrl serverId startTime endTime =
  Http.get
    { url = Url.crossOrigin baseUrl ["lives"]
      [ Url.int "server_id" serverId
      , Url.int "start_time" (startTime |> Time.posixToMillis |> (\x -> x // 1000))
      , Url.int "end_time" (endTime |> Time.posixToMillis |> (\x -> x // 1000))
      , Url.string "limit" "70000"
      ]
    , expect = Http.expectJson DataLayer Json.Decode.value
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

isInRange : Posix -> Posix -> Posix -> Bool
isInRange mint t maxt =
  let
    mini = Time.posixToMillis mint
    i = Time.posixToMillis t
    maxi = Time.posixToMillis maxt
  in
    mini < i && i <= maxi

increment : Posix -> Posix
increment =
  Time.posixToMillis
  >> (\x -> x + 1000)
  >> Time.millisToPosix
