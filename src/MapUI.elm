module MapUI exposing (..)

import Leaflet exposing (Point)
import OHOLData as Data exposing (Server)
import OHOLData.Decode as Decode
import OHOLData.Encode as Encode
import View exposing (RemoteData(..), Life)

import Browser
--import Browser.Dom
import Browser.Navigation as Navigation
import Http
import Json.Decode
import Set exposing(Set)
import Task
import Time exposing (Posix)
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
  | CurrentZone Time.Zone
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , zone : Time.Zone
  , center : Point
  , cachedApiUrl : String
  , apiUrl : String
  , lineageUrl: String
  , sidebarOpen : Bool
  , sidebarMode : View.Mode
  , searchTerm : String
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
      , cachedApiUrl = config.cachedApiUrl
      , apiUrl = config.apiUrl
      , lineageUrl = config.lineageUrl
      , sidebarOpen = True
      , sidebarMode = View.LifeSearch
      , searchTerm = ""
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
      , Cmd.none
      )
    Event (Ok (Leaflet.MoveEnd point)) ->
      ( {model|center = point} 
      , Navigation.replaceUrl model.navigationKey <|
        centerUrl model.location point
      )
    Event (Ok (Leaflet.OverlayAdd "Search" _)) ->
      ({model | sidebarOpen = True}, Cmd.none)
    Event (Ok (Leaflet.OverlayAdd "48h Births" _)) ->
      requireRecentLives model
    Event (Ok (Leaflet.OverlayAdd "48h Births Anim" _)) ->
      requireRecentLives model
    Event (Ok (Leaflet.OverlayAdd name (Just serverId))) ->
      if Set.member serverId model.monumentsFetched then
        (model, Cmd.none)
      else
        (model, fetchMonuments model.cachedApiUrl serverId)
    Event (Ok (Leaflet.OverlayAdd _ _)) ->
      (model, Cmd.none)
    Event (Ok (Leaflet.OverlayRemove "Search")) ->
      ({model | sidebarOpen = False}, Cmd.none)
    Event (Ok (Leaflet.OverlayRemove name)) ->
      (model, Cmd.none)
    Event (Ok (Leaflet.SelectPoints lives)) ->
      ( { model
        | lives = lives |> List.map myLife |> Data
        , sidebarOpen = True
        }
      , Cmd.batch
        [ Leaflet.displayResults lives
        , Leaflet.searchOverlay True
        ]
      )
    Event (Ok (Leaflet.SidebarToggle)) ->
      ({model | sidebarOpen = not model.sidebarOpen}, Cmd.none)
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
      ( {model | servers = servers |> Data}
      , Cmd.batch
        [ Leaflet.serverList servers
        , fetchMonuments model.cachedApiUrl 17
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
      , Leaflet.dataLayer lives
      )
    DataLayer (Err error) ->
      let _ = Debug.log "fetch data failed" error in
      ({model | dataLayer = Failed error}, Cmd.none)
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
      , fetchRecentLives model.cachedApiUrl 17
      )
    Failed _ ->
      ( {model | dataLayer = Loading}
      , fetchRecentLives model.cachedApiUrl 17
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
      , Url.string "period" "P2D"
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
