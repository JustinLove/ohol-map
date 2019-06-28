module MapUI exposing (..)

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
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , center : Point
  }

type alias Point =
  { x : Int
  , y : Int
  , z : Int
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
    Debug.log "init" <| changeRouteTo location initialModel

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI (View.None) -> (model, Cmd.none)
    CurrentUrl location ->
      Debug.log "url" <| changeRouteTo location model
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
  in
    case (mx, my, mz) of
      (Just x, Just y, Just z) ->
        ( { model
          | location = location
          , center = Point x y z
          }
        , Cmd.none
        )
      (Just x, Just y, Nothing) ->
        ( { model
          | location = location
          , center = Point x y 24
          }
        , Cmd.none
        )
      _ ->
        ( { model | location = location }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

extractHashArgument : String -> Url -> Maybe Int
extractHashArgument key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.int key))
    |> Maybe.withDefault Nothing
