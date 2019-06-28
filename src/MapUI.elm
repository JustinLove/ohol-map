module MapUI exposing (..)

import Leaflet exposing (Point)
import View

import Browser
--import Browser.Dom
import Browser.Navigation as Navigation
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser
import Url.Parser.Query

type Msg
  = UI View.Msg
  | Event Leaflet.Event
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , center : Point
  }

main = Browser.application
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = View.document UI
  , onUrlRequest = Navigate
  , onUrlChange = CurrentUrl
  }

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init _ location key =
  let
    initialModel =
      { location = location
      , navigationKey = key
      , center = Point 0 0 17
      }
  in
    changeRouteTo location initialModel

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI (View.None) -> (model, Cmd.none)
    Event (Leaflet.MoveEnd point) ->
      ( {model|center = point} 
      , Navigation.replaceUrl model.navigationKey <|
        centerUrl model.location point
      )
    Event (Leaflet.Error) ->
      let _ = Debug.log "error" "" in
      (model, Cmd.none)
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
