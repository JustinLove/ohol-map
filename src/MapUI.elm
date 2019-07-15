module MapUI exposing (..)

import Leaflet exposing (Point)
import OHOLData as Data exposing (Server)
import OHOLData.Decode as Decode
import OHOLData.Encode as Encode
import View exposing (RemoteData(..), Life, EndTimeMode(..))

import Browser
--import Browser.Dom
import Browser.Navigation as Navigation
import Http
import Json.Decode
import Set exposing(Set)
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
  | MonumentList Int (Result Http.Error Json.Decode.Value)
  | DataLayer (Result Http.Error Json.Decode.Value)
  | FetchUpTo Posix
  | PlayRelativeTo Posix
  | CurrentZone Time.Zone
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , zone : Time.Zone
  , center : Point
  , layersVisible : Set String
  , cachedApiUrl : String
  , apiUrl : String
  , lineageUrl: String
  , sidebarOpen : Bool
  , sidebarMode : View.Mode
  , searchTerm : String
  , endTimeMode : EndTimeMode
  , coarseEndTime : Posix
  , endTime : Posix
  , hoursBefore : Int
  , gameSecondsPerFrame : Int
  , frameRate : Int
  , selectedServer : Maybe Server
  , servers : RemoteData (List Server)
  , monumentsFetched : Set Int
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
      , center = Point 0 0 17
      , layersVisible = Set.empty
      , cachedApiUrl = config.cachedApiUrl
      , apiUrl = config.apiUrl
      , lineageUrl = config.lineageUrl
      , sidebarOpen = False
      , sidebarMode = View.LifeSearch
      , searchTerm = ""
      , endTimeMode = ServerRange
      , coarseEndTime = Time.millisToPosix 0
      , endTime = Time.millisToPosix 0
      , hoursBefore = 48
      , gameSecondsPerFrame = 60
      , frameRate = 10
      , selectedServer = Nothing
      , servers = NotRequested
      , monumentsFetched = Set.empty
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
      , fetchServers model.cachedApiUrl
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
      ( { model
        | coarseEndTime = time
        , endTime = time
        }
      , Cmd.none
      )
    UI (View.EndTime time) ->
      ( { model
        | endTime = time
        }
      , Cmd.none
      )
    UI (View.HoursBefore hours) ->
      ( { model
        | hoursBefore = hours
        }
      , Cmd.none
      )
    UI (View.GameSecondsPerFrame seconds) ->
      ( { model
        | gameSecondsPerFrame = seconds
        }
      , Cmd.none
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
          , dataLayer = Loading
          }
      in
      ( m2, fetchDataForTime m2 )
    UI (View.SelectShow) ->
      ( {model | dataLayer = Loading}
      , fetchDataForTime model
      )
    UI (View.SelectYesterday) ->
      ( { model
        | dataLayer = Loading
        , endTimeMode = FromNow
        , hoursBefore = 24
        , gameSecondsPerFrame = 1
        , frameRate = 1
        }
      , fetchRecentLives model.cachedApiUrl (model.selectedServer |> Maybe.map .id |> Maybe.withDefault 17)
      )
    Event (Ok (Leaflet.MoveEnd point)) ->
      ( {model|center = point} 
      , Navigation.replaceUrl model.navigationKey <|
        centerUrl model.location point
      )
    Event (Ok (Leaflet.OverlayAdd "Life Data" _)) ->
      requireRecentLives {model | layersVisible = Set.insert "Life Data" model.layersVisible }
    Event (Ok (Leaflet.OverlayAdd "Life Data Anim" _)) ->
      requireRecentLives {model | layersVisible = Set.insert "Life Data Anim" model.layersVisible }
    Event (Ok (Leaflet.OverlayAdd name (Just serverId))) ->
      if Set.member serverId model.monumentsFetched then
        ({model | layersVisible = Set.insert name model.layersVisible }, Cmd.none)
      else
        ({model | layersVisible = Set.insert name model.layersVisible }, fetchMonuments model.cachedApiUrl serverId)
    Event (Ok (Leaflet.OverlayAdd name _)) ->
      ({model | layersVisible = Set.insert name model.layersVisible }, Cmd.none)
    Event (Ok (Leaflet.OverlayRemove name)) ->
      ({model | layersVisible = Set.remove name model.layersVisible }, Cmd.none)
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
      , Cmd.batch
        [ Leaflet.serverList servers
        , fetchMonuments model.cachedApiUrl id
        ]
      )
    ServerList (Err error) ->
      let _ = Debug.log "fetch servers failed" error in
      ({model | servers = Failed error}, Cmd.none)
    MonumentList serverId (Ok monuments) ->
      ( {model | monumentsFetched = Set.insert serverId model.monumentsFetched}
      , Leaflet.monumentList serverId monuments
      )
    MonumentList serverId (Err error) ->
      let _ = Debug.log "fetch monuments failed" error in
      (model, Cmd.none)
    DataLayer (Ok lives) ->
      ( {model | dataLayer = Data True}
      , Cmd.batch
        [ Leaflet.dataLayer lives
        , if Set.member "Life Data Anim" model.layersVisible then
            case model.endTimeMode of
              ServerRange -> update (PlayRelativeTo model.endTime) model |> Tuple.second
              FromNow -> Task.perform PlayRelativeTo Time.now
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
          time
          model.hoursBefore
      )
    PlayRelativeTo time ->
      ( model
      , Leaflet.beginPlayback model.gameSecondsPerFrame model.frameRate
        (time
          |> Time.posixToMillis
          |> (\x -> x - model.hoursBefore * 60 * 60 * 1000)
          |> Time.millisToPosix
        )
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

changeRouteTo : Url -> Model -> (Model, Cmd Msg)
changeRouteTo location model =
  let
    mx = extractHashArgument "x" location
    my = extractHashArgument "y" location
    mz = extractHashArgument "z" location
    m2 = {model | location = location}
  in
    case (mx, my, mz) of
      (Just x, Just y, Just z) ->
        setView (Point x y z) m2
      (Just x, Just y, Nothing) ->
        setView (Point x y 24) m2
      _ ->
        ( m2, Cmd.none)

setView : Point -> Model -> (Model, Cmd Msg)
setView point model =
  if model.center /= point then
    ({model|center = point}, Leaflet.setView point)
  else
    (model, Cmd.none)

fetchDataForTime : Model -> Cmd Msg
fetchDataForTime model =
  case model.endTimeMode of
    ServerRange ->
      fetchDataLayer model.apiUrl
        (model.selectedServer |> Maybe.map .id |> Maybe.withDefault 17)
        model.endTime
        model.hoursBefore
    FromNow ->
      Task.perform FetchUpTo Time.now

subscriptions : Model -> Sub Msg
subscriptions model =
  Leaflet.event Event

fetchServers : String -> Cmd Msg
fetchServers baseUrl =
  Http.get
    { url = Url.crossOrigin baseUrl ["servers"] []
    , expect = Http.expectJson ServerList Decode.servers
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

      ]
    , expect = Http.expectJson DataLayer Json.Decode.value
    }

fetchDataLayer : String -> Int -> Posix -> Int -> Cmd Msg
fetchDataLayer baseUrl serverId endTime hoursBefore =
  Http.get
    { url = Url.crossOrigin baseUrl ["lives"]
      [ Url.int "server_id" serverId
      , Url.string "period" ("PT" ++ (String.fromInt hoursBefore) ++ "H")
      , Url.int "end_time" (endTime |> Time.posixToMillis |> (\x -> x // 1000))
      , Url.string "limit" "70000"
      ]
    , expect = Http.expectJson DataLayer Json.Decode.value
    }

centerUrl : Url -> Point-> String
centerUrl location {x, y, z} =
  { location
  | fragment =
    Url.toQuery
      [ Url.int "x" x
      , Url.int "y" y
      , Url.int "z" z
      ]
      |> String.dropLeft 1
      |> Just
  } |> Url.toString

extractHashArgument : String -> Url -> Maybe Int
extractHashArgument key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.int key))
    |> Maybe.withDefault Nothing

inRange : Posix -> Posix -> Posix -> Posix
inRange mint t maxt =
  let
    mini = Time.posixToMillis mint
    i = Time.posixToMillis t
    maxi = Time.posixToMillis maxt
  in
    Time.millisToPosix (min maxi (max mini i))
