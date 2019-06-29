module View exposing (Msg(..), view, document)

import Browser
import Element exposing (..)
import Element.Keyed as Keyed
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
  layout [ width fill, height fill ] <|
    Keyed.row [ width fill, height fill ]
      [ ( "map", el [ id "map", width fill, height fill ] none)
      , ( "sidebar"
        , if model.sidebarOpen then
            el [ width (fill |> maximum 300) ] <| text "sidebar"
          else
            none
        )
      ]

id : String -> Attribute Msg
id =
  htmlAttribute << Html.Attributes.id
