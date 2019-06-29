module View exposing (Msg(..), RemoteData(..), view, document)

import Browser
import Element exposing (..)
import Element.Input as Input
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
  | Typing String

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
    [ searchBox model.searchTerm model.lives
    ]

searchBox : String -> RemoteData a -> Element Msg
searchBox term request =
  Input.text
    [ padding 2
    , htmlAttribute <| on "change" <| targetValue Json.Decode.string Search
    ] <|
    { onChange = Typing
    , text = term
    , placeholder = Nothing
    , label = Input.labelAbove [] <| text "Character Name or Hash"
    }

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

id : String -> Attribute Msg
id =
  htmlAttribute << Html.Attributes.id
