module View exposing (Msg(..), Mode(..), RemoteData(..), Life, view, document)

import OHOLData as Data exposing (Server)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
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
  | CoarseEndTime Posix
  | EndTime Posix
  | SelectMatchingLife Life
  | SelectLineage Life
  | SelectMode Mode
  | SelectServer Server

type Mode
  = LifeSearch
  | DataFilter

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
    [ modeSelect model
    , case model.sidebarMode of
        LifeSearch -> lifeSearch model
        DataFilter -> dataFilter model
    ]

-- modeSelect : Model -> Element Msg
modeSelect model =
  row
    [ width fill
    , Border.color divider
    , Border.widthEach
      { bottom = 1
      , left = 0
      , right = 0
      , top = 0
      }
    ]
    [ tabHeader "search" "Search" LifeSearch model.sidebarMode
    , tabHeader "filter" "Data" DataFilter model.sidebarMode
    ]

tabHeader : String -> String -> Mode -> Mode -> Element Msg
tabHeader ico name mode current =
  Input.button [ width fill ]
    { onPress = Just (SelectMode mode)
    , label = 
      el
        [ width fill
        , Background.color (if mode == current then highlight else background)
        ] <|
        row [ centerX, spacing 6 ]
          [ el [ Font.size 16 ] <| icon ico
          , text name
          ]
    }

-- lifeSearch : Model -> Element Msg
lifeSearch model =
  column
    [ width fill
    , height fill
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

-- dataFilter : Model -> Element Msg
dataFilter model =
  column
    [ width fill
    , height fill
    ]
    [ dateRangeSelect model
    , serverSelect model.servers model.selectedServer
    ]

--dateRangeSelect : Model -> Element Msg
dateRangeSelect model =
  column [ width fill ]
    [ Input.slider
      [ Background.color control ]
      { onChange = round
        >> ((*) 1000)
        >> Time.millisToPosix
        >> CoarseEndTime
      , label = Input.labelAbove [] <|
        row []
          [ text "Coarse End Date "
          , ( model.coarseEndTime
              |> date model.zone
              |> text
            )
          ]
      , min = model.minTimeRange
        |> Time.posixToMillis
        |> (\x -> x // 1000)
        |> toFloat
      , max = model.maxTimeRange
        |> Time.posixToMillis
        |> (\x -> x // 1000)
        |> toFloat
      , value = model.coarseEndTime
        |> Time.posixToMillis
        |> (\x -> x // 1000)
        |> toFloat
      , thumb = Input.defaultThumb
      , step = Just 1
      }
    , Input.slider
      [ Background.color control ]
      { onChange = round
        >> ((*) 1000)
        >> Time.millisToPosix
        >> EndTime
      , label = Input.labelAbove [] <|
        row []
          [ text "Fine End Time "
          , ( model.endTime
              |> date model.zone
              |> text
            )
          ]
      , min = model.coarseEndTime
        |> Time.posixToMillis
        |> (\x -> x // 1000 - 7*24*60*60)
        |> toFloat
      , max = model.coarseEndTime
        |> Time.posixToMillis
        |> (\x -> x // 1000 + 7*24*60*60)
        |> toFloat
      , value = model.endTime
        |> Time.posixToMillis
        |> (\x -> x // 1000)
        |> toFloat
      , thumb = Input.defaultThumb
      , step = Just 1
      }
    ]

serverSelect : RemoteData (List Server) -> Maybe Server -> Element Msg
serverSelect servers selectedServer =
  case servers of
    NotRequested -> none
    Loading -> showLoading servers
    Failed error -> showError error
    Data list ->
      Input.radio [ padding 10, spacing 2 ]
        { onChange = SelectServer
        , selected = selectedServer
        , label = Input.labelAbove [] (text "Server")
        , options = list |> List.map serverItem
        }

serverItem : Server -> Input.Option Server Msg
serverItem server =
  Input.option server
    (text (serverDisplayName server))

serverDisplayName : Server -> String
serverDisplayName {serverName} =
  serverName
    |> String.split "."
    |> List.head
    |> Maybe.withDefault serverName

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
divider = rgb 0.7 0.7 0.7
control = rgb 0.90 0.90 0.90

scaled height = modular (max ((toFloat height)/30) 15) 1.25 >> round
