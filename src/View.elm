module View exposing (Msg(..), RemoteData(..), Life, view, document)

import OHOLData as Data

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (on, stopPropagationOn)
import Html.Keyed
import Http
import Json.Decode
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Time exposing (Posix)
import Url exposing (Url)
import Url.Builder as Url

type Msg
  = None
  | Search String
  | Typing String
  | SelectMatchingLife Life
  | SelectLineage Life

type RemoteData a
  = NotRequested
  | Loading
  | Data a
  | Failed Http.Error

type alias Life =
  { birthTime : Posix
  , generation : Int
  , playerid : Int
  , lineage : Int
  , name : Maybe String
  , serverId : Int
  , epoch : Int
  , age : Float
  , birthX : Int
  , birthY : Int
  }

--document : (Msg -> msg) -> model -> Browser.Document msg
document tagger model =
  { title = "OHOL Map"
  , body = [Html.map tagger (view model)]
  }

-- view : model -> Html Msg
view model =
  layout
    [ width fill, height fill
    , Font.color foreground
    , Background.color background
    ] <|
    Keyed.row [ width fill, height fill ]
      [ ( "map"
        , el [ width fill, height fill, Font.size 12 ]
          (html <| Html.div [ Html.Attributes.id "map" ] [])
        )
      , ( "sidebar"
        , if model.sidebarOpen then
            sidebar model
          else
            none
        )
      ]

-- sidebar : Model -> Element Msg
sidebar model =
  column
    [ width (fill |> maximum 330)
    , height fill
    , alignTop
    , htmlAttribute (Html.Attributes.id "sidebar")
    ]
    [ searchBox model.searchTerm model.lives
    , showResult model model.lives
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

showResult model remote =
  case remote of
    NotRequested ->
      none
    Loading ->
      el [ centerX, centerY ] <| text "Loading"
    Data lives ->
      showMatchingLives model lives
    Failed error ->
      showError error

showLoading : RemoteData a -> Element Msg
showLoading remote =
  case remote of
    NotRequested ->
      none
    Loading ->
      el [ centerX, centerY ] <| text "Loading"
    Data _ ->
      none
    Failed error ->
      showError error

showError : Http.Error -> Element Msg
showError error =
  el [ centerX, centerY ] <|
    case error of
      Http.BadUrl url ->
        twoPartMessage 500
          "Bad Url"
          "This *really* shouldn't happen."
      Http.Timeout ->
        twoPartMessage 500
          "Timeout"
          "Wondible is cheap and you are getting this for free."
      Http.NetworkError ->
        twoPartMessage 500
          "Network Error"
          "Either you or the server went offline"
      Http.BadStatus code ->
        twoPartMessage 500
          (code |> String.fromInt)
          "Server is being naughty again.'"
      Http.BadBody body ->
        twoPartMessage 500
          "Bad Body"
          body

twoPartMessage : Int -> String -> String -> Element Msg
twoPartMessage height header body =
  column []
    [ el [ centerX, Font.size (scaled height 2)] <|
      text header
    , el [ centerX, Font.size (scaled height 1)] <|
      text body
    ]


showMatchingLives model lives =
  column [ spacing 10, width fill, scrollbarY ]
    (lives
      |> List.map (showMatchingLife model)
      |> (::) lifeListHeader
    )

lifeListHeader =
  row
    [ width fill
    , Font.size 10
    , Font.underline
    ]
    [ lifeDetailHeader
    , el [ width (px 30) ] (el [ centerX ] (text "Lin"))
    , el [ width (px 30) ] (el [ centerX ] (text "Tree"))
    ]

showMatchingLife model life =
  row
    [ if Just life == model.focus then
        Background.color highlight
      else
        Background.color background
    ]
    [ Input.button []
      { onPress = Just (SelectMatchingLife life)
      , label = showMatchingLifeDetail model life
      }
    , Input.button [ width(px 30), padding 10 ]
      { onPress = Just (SelectLineage life)
      , label = el [ centerX ] <| icon "users"
      }
    , newTabLink [ width(px 30), padding 10 ]
      { url = lineageUrl model.lineageUrl life.serverId life.epoch life.playerid
      , label = el [ centerX ] <| icon "tree"
      }
    ]

lifeDetailHeader =
  column [ padding 4 ]
    [ row
      [ Font.size 16
      , spacing 10
      , width fill
      ]
      [ el [ width (px 30) ]
        (text "Age")
      , el [ width (px 140) ]
        (text "Born")
      , el [ width (px 30) ]
        (text "Gen")
      ]
    ]

showMatchingLifeDetail model life =
  column [ padding 4 ]
    [ life.name
        |> Maybe.withDefault "nameless"
        |> text
    , row
      [ Font.size 16
      , spacing 10
      ]
      [ el [ width (px 30) ]
        (life.age
          |> ceiling
          |> String.fromInt
          |> text
        )
      , el [ width (px 140) ]
        ( life.birthTime
          |> date model.zone
          |> text
        )
      , el [ width (px 30) ]
        ( life.generation
          |> String.fromInt
          |> text
        )
      ]
    ]

date : Time.Zone -> Posix -> String
date zone time =
  let
    year = Time.toYear zone time |> String.fromInt
    month = Time.toMonth zone time |> formatMonth
    day = Time.toDay zone time |> String.fromInt |> String.padLeft 2 '0'
    hour = Time.toHour zone time |> String.fromInt |> String.padLeft 2 '0'
    minute = Time.toMinute zone time |> String.fromInt |> String.padLeft 2 '0'
  in
    year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute

formatMonth : Time.Month -> String
formatMonth month =
  case month of
    Time.Jan -> "01"
    Time.Feb -> "02"
    Time.Mar -> "03"
    Time.Apr -> "04"
    Time.May -> "05"
    Time.Jun -> "06"
    Time.Jul -> "07"
    Time.Aug -> "08"
    Time.Sep -> "09"
    Time.Oct -> "10"
    Time.Nov -> "11"
    Time.Dec -> "12"

lineageUrl : String -> Int -> Int -> Int -> String
lineageUrl base serverId epoch playerid =
  Url.custom (Url.CrossOrigin base) [] []
    (Url.toQuery
      [ Url.int "server_id" serverId
      , Url.int "epoch" epoch
      , Url.int "playerid" playerid
      ]
      |> String.dropLeft 1
      |> Just
    )

icon : String -> Element msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]
  |> html

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

foreground = rgb 0.1 0.1 0.1
background = rgb 0.98 0.98 0.98
highlight = rgb 0.8 0.8 0.8

scaled height = modular (max ((toFloat height)/30) 15) 1.25 >> round
