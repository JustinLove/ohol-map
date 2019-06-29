module View exposing (Msg(..), RemoteData(..), view, document)

import Browser
import Element exposing (..)
import Element.Keyed as Keyed
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (on)
import Html.Keyed
import Http
import Json.Decode

type Msg
  = None
  | Search String

type RemoteData a
  = NotRequested
  | Loading
  | Data a
  | Failed Http.Error

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
            sidebar model
          else
            none
        )
      ]

-- sidebar : Model -> Element Msg
sidebar model =
  column [ width (fill |> maximum 300) ]
    [ searchBox model.lives
    ]

searchBox : RemoteData a -> Element Msg
searchBox request =
  el [ padding 2 ] <|
    html <|
      Html.div [ Html.Attributes.class "search" ]
        [ Html.label [ Html.Attributes.for "search" ]
          [ Html.text "Character Name or Hash" ]
        , Html.text " "
        , Html.input
          [ Html.Attributes.type_ "search"
          , Html.Attributes.size 42
          , Html.Attributes.id "search"
          , Html.Attributes.name "search"
          , Html.Attributes.disabled (request == Loading)
          , on "change" <| targetValue Json.Decode.string Search
          ] []
        ]

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

id : String -> Attribute Msg
id =
  htmlAttribute << Html.Attributes.id
