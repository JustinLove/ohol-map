module View exposing (Msg(..), view, document)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (on)
import Html.Keyed

type Msg
  = None

--document : (Msg -> msg) -> model -> Browser.Document msg
document tagger model =
  { title = "OHOL Map"
  , body = [Html.map tagger (view model)]
  }

-- view : model -> Html Msg
view model =
  Html.Keyed.node "div" [ Html.Attributes.id "mapcontainer" ]
    [("map", Html.div [ Html.Attributes.id "map" ] []) ]
