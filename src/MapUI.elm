module MapUI exposing (..)

import Leaflet exposing (Point, PointColor(..), PointLocation(..))
import OHOLData as Data exposing (Server, Arc)
import OHOLData.Decode as Decode
import OHOLData.Encode as Encode
import View exposing (RemoteData(..), Life, EndTimeMode(..), centerUrl)

import Browser
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
  | ServerList (Result Http.Error (List Data.Server))
  | ArcList (Result Http.Error (List Data.Arc))
  | MonumentList Int (Result Http.Error Json.Decode.Value)
  | DataLayer (Result Http.Error Json.Decode.Value)
  | FetchUpTo Posix
  | PlayRelativeTo Posix
  | CurrentTime Posix
  | CurrentZone Time.Zone
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , zone : Time.Zone
  , time : Posix
  , center : Point
  , cachedApiUrl : String
  , apiUrl : String
  , lineageUrl: String
  , sidebarOpen : Bool
  , sidebarMode : View.Mode
  , searchTerm : String
  , endTimeMode : EndTimeMode
  , coarseEndTime : Posix
  , endTime : Posix
  , mapTime : Maybe Posix
  , hoursBefore : Int
  , currentArc : Maybe Arc
  , dataAnimated : Bool
  , gameSecondsPerFrame : Int
  , frameRate : Int
  , pointColor : PointColor
  , pointLocation : PointLocation
  , selectedServer : Maybe Server
  , servers : RemoteData (List Server)
  , arcs : RemoteData (List Arc)
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
      , center = Point 0 0 17
      , cachedApiUrl = config.cachedApiUrl
      , apiUrl = config.apiUrl
      , lineageUrl = config.lineageUrl
      , sidebarOpen = False
      , sidebarMode = View.LifeSearch
      , searchTerm = ""
      , endTimeMode = ServerRange
      , coarseEndTime = Time.millisToPosix 0
      , endTime = Time.millisToPosix 0
      , mapTime = Nothing
      , hoursBefore = 48
      , currentArc = Nothing
      , dataAnimated = False
      , gameSecondsPerFrame = 60
      , frameRate = 10
      , pointColor = LineageColor
      , pointLocation = BirthLocation
      , selectedServer = Nothing
      , servers = NotRequested
      , arcs = NotRequested
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
      , Time.now |> Task.perform CurrentTime
      , fetchServers model.cachedApiUrl
      , fetchArcs model.cachedApiUrl
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
    UI (View.SelectEndTimeMode mode) ->
      ( { model | endTimeMode = mode }
      , Cmd.none
      )
    UI (View.CoarseEndTime time) ->
      { model
      | coarseEndTime = time
      , endTime = time
      , mapTime = Just time
      }
        |> setTime time
    UI (View.EndTime time) ->
      { model
      | endTime = time
      , mapTime = Just time
      }
        |> setTime time
    UI (View.HoursBefore hours) ->
      ( { model
        | hoursBefore = hours
        }
      , Cmd.none
      )
    UI (View.ToggleAnimated animated) ->
      ( { model
        | dataAnimated = animated
        }
      , Leaflet.animOverlay animated
      )
    UI (View.GameSecondsPerFrame seconds) ->
      ( { model
        | gameSecondsPerFrame = seconds
        }
      , Leaflet.playbackScale seconds
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
        }
      , Leaflet.focus (serverLife life)
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
        m2 = { model
          | selectedServer = Just server
          , coarseEndTime = inRange server.minTime model.coarseEndTime server.maxTime
          , endTime = inRange server.minTime model.endTime server.maxTime
          , mapTime = model.mapTime
            |> Maybe.map (\time -> inRange server.minTime time server.maxTime)
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
          { model
          | currentArc = marc
          , coarseEndTime = arc.end
          , endTime = arc.end
          }
            |> setTime arc.end
        Nothing ->
          ( { model
            | currentArc = marc
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
    Event (Ok (Leaflet.TimeLoad time)) ->
      ( {model|mapTime = Just time}
      , Navigation.replaceUrl model.navigationKey <|
        centerUrl model.location (Just time) (isYesterday model) model.center
      )
    Event (Ok (Leaflet.OverlayAdd "Life Data" _)) ->
      requireLives model
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
    Event (Ok (Leaflet.SidebarToggle)) ->
      ( { model | sidebarOpen = not model.sidebarOpen }
      , Leaflet.searchOverlay
        (model.sidebarOpen == False && model.sidebarMode == View.LifeSearch)
      )
    Event (Ok (Leaflet.AnimToggle)) ->
      ( { model | dataAnimated = not model.dataAnimated }
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
    ServerList (Ok servers) ->
      let
        bs2 = servers |> List.reverse |> List.head
        endTime = bs2 |> Maybe.map .maxTime |> Maybe.withDefault (Time.millisToPosix 0)
        id = bs2 |> Maybe.map .id |> Maybe.withDefault 17
      in
      ( { model
        | servers = servers |> Data
        , selectedServer = bs2
        , coarseEndTime = endTime
        , endTime = endTime
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
      ( {model | arcs = Data arcs, currentArc = List.head arcs}
      , Leaflet.arcList arcs (model.mapTime |> Maybe.withDefault model.time)
      )
    ArcList (Err error) ->
      let _ = Debug.log "fetch arcs failed" error in
      ({model | arcs = Failed error, currentArc = Nothing}, Cmd.none)
    MonumentList serverId (Ok monuments) ->
      ( {model | monuments = Dict.insert serverId monuments model.monuments}
      , Leaflet.monumentList serverId monuments
      )
    MonumentList serverId (Err error) ->
      let _ = Debug.log "fetch monuments failed" error in
      (model, Cmd.none)
    DataLayer (Ok lives) ->
      ( {model | dataLayer = Data True}
      , Cmd.batch
        [ Leaflet.dataLayer lives
        , if model.dataAnimated then
            case model.endTimeMode of
              ServerRange -> update (PlayRelativeTo model.endTime) model |> Tuple.second
              FromNow -> Task.perform PlayRelativeTo Time.now
              ArcRange -> Task.perform PlayRelativeTo Time.now
          else
            Cmd.none
        ]
      )
    DataLayer (Err error) ->
      let _ = Debug.log "fetch data failed" error in
      ({model | dataLayer = Failed error}, Cmd.none)
    FetchUpTo time ->
      ( model
      , fetchDataLayer model.apiUrl
          (model.selectedServer |> Maybe.map .id |> Maybe.withDefault 17)
          (relativeStartTime time model.hoursBefore)
          time
      )
    PlayRelativeTo time ->
      ( model
      , Leaflet.beginPlayback model.gameSecondsPerFrame model.frameRate
        (relativeStartTime time model.hoursBefore)
      )
    CurrentTime time ->
      ({model | time = time}, Cmd.none)
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
  if model.mapTime == Nothing then
    requireRecentLives model
  else
    requireSelectedLives model

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
    , endTimeMode = FromNow
    , dataAnimated = True
    , hoursBefore = 24
    , gameSecondsPerFrame = 1
    , frameRate = 1
    }
  , Cmd.batch
    [ fetchRecentLives model.cachedApiUrl (model.selectedServer |> Maybe.map .id |> Maybe.withDefault 17)
    , Leaflet.animOverlay True
    ]
  )

setTime : Posix -> Model -> (Model, Cmd Msg)
setTime time model =
  if model.mapTime /= Just time then
    ( {model|mapTime = Just time}
    , Cmd.batch
        [ Leaflet.currentTime time
        , Navigation.replaceUrl model.navigationKey <|
          centerUrl model.location (Just time) (isYesterday model) model.center
        ]
    )
  else
    (model, Cmd.none)

setYesterday : Model -> (Model, Cmd Msg)
setYesterday model =
  if isYesterday model then
    (model, Cmd.none)
  else
    yesterday model

isYesterday : Model -> Bool
isYesterday model =
  model.dataLayer /= NotRequested
    && model.endTimeMode == FromNow
    && model.hoursBefore == 24
    && model.gameSecondsPerFrame == 1
    && model.frameRate == 1
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
    mt = extractHashInt "t" location
      |> Maybe.map (\t -> t * 1000 |> Time.millisToPosix)
  in
    case mt of
      Just t ->
        if model.mapTime /= Just t then
          ({model|mapTime = Just t} |> timeSelectionForTime, Leaflet.currentTime t)
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
        ( model, Cmd.none)

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
            newArc = arcs
              |> List.filter (\arc -> isInRange arc.start time arc.end)
              |> Debug.log "filtered"
              |> List.head
          in
            case newArc of
              Just arc ->
                { model
                | endTimeMode = ArcRange
                , currentArc = newArc
                }
              Nothing ->
                timeSelectionAround model
        _ ->
          timeSelectionAround model

timeSelectionAround : Model -> Model
timeSelectionAround model =
  let
    time = relativeEndTime (model.mapTime |> Maybe.withDefault model.time) (model.hoursBefore//2)
  in
    { model
    | endTimeMode = ServerRange
    , coarseEndTime = time
    , endTime = time
    }

fetchDataForTime : Model -> Cmd Msg
fetchDataForTime model =
  case model.endTimeMode of
    ServerRange ->
      fetchDataLayer model.apiUrl
        (model.selectedServer |> Maybe.map .id |> Maybe.withDefault 17)
        (relativeStartTime model.endTime model.hoursBefore)
        model.endTime
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
  Leaflet.event Event

fetchServers : String -> Cmd Msg
fetchServers baseUrl =
  Http.get
    { url = Url.crossOrigin baseUrl ["servers"] []
    , expect = Http.expectJson ServerList Decode.servers
    }

fetchArcs : String -> Cmd Msg
fetchArcs baseUrl =
  Http.get
    { url = Url.crossOrigin baseUrl ["arcs"] []
    , expect = Http.expectJson ArcList Decode.arcs
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
    , expect = Http.expectJson MatchingLives Decode.lives
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

relativeStartTime : Posix -> Int -> Posix
relativeStartTime endTime hoursBefore =
  endTime
    |> Time.posixToMillis
    |> (\x -> x - hoursBefore * 60 * 60 * 1000)
    |> Time.millisToPosix

relativeEndTime : Posix -> Int -> Posix
relativeEndTime endTime hoursBefore =
  endTime
    |> Time.posixToMillis
    |> (\x -> x + hoursBefore * 60 * 60 * 1000)
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

inRange : Posix -> Posix -> Posix -> Posix
inRange mint t maxt =
  let
    mini = Time.posixToMillis mint
    i = Time.posixToMillis t
    maxi = Time.posixToMillis maxt
  in
    Time.millisToPosix (min maxi (max mini i))

isInRange : Posix -> Posix -> Posix -> Bool
isInRange mint t maxt =
  let
    mini = Time.posixToMillis mint
    i = Time.posixToMillis t
    maxi = Time.posixToMillis maxt
  in
    mini <= i && i <= maxi
