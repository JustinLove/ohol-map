module MapUI exposing (..)

import Leaflet exposing (Point)
import OHOLData as Data
import OHOLData.Decode as Decode
import OHOLData.Encode as Encode
import View exposing (RemoteData(..), Life)

import Browser
--import Browser.Dom
import Browser.Navigation as Navigation
import Http
import Task
import Time exposing (Posix)
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser
import Url.Parser.Query

type Msg
  = UI View.Msg
  | Event Leaflet.Event
  | MatchingLives (Result Http.Error (List Data.Life))
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
  , sidebarOpen : Bool
  , searchTerm : String
  , lives : RemoteData (List Life)
  , focus : Maybe Life
  }

type alias Config =
  { cachedApiUrl: String
  , apiUrl: String
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
      , sidebarOpen = False
      , searchTerm = ""
      , lives = NotRequested
      , focus = Nothing
      }
    (model, cmd) = changeRouteTo location initialModel
  in
    ( model
    , Cmd.batch
      [ cmd
      , Time.here |> Task.perform CurrentZone
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
    Event (Leaflet.MoveEnd point) ->
      ( {model|center = point} 
      , Navigation.replaceUrl model.navigationKey <|
        centerUrl model.location point
      )
    Event (Leaflet.OverlayAdd "Search") ->
      ({model | sidebarOpen = True}, Cmd.none)
    Event (Leaflet.OverlayAdd name) ->
      (model, Cmd.none)
    Event (Leaflet.OverlayRemove "Search") ->
      ({model | sidebarOpen = False}, Cmd.none)
    Event (Leaflet.OverlayRemove name) ->
      (model, Cmd.none)
    Event (Leaflet.Error) ->
      let _ = Debug.log "error" "" in
      (model, Cmd.none)
    MatchingLives (Ok lives) ->
      ( {model | lives = lives |> List.map myLife |> Data}
      , Leaflet.displayResults lives
      )
    MatchingLives (Err error) ->
      let _ = Debug.log "fetch lives failed" error in
      ({model | lives = Failed error}, Cmd.none)
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
