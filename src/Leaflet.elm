port module Leaflet exposing
  ( Point
  , PointColor(..)
  , PointLocation(..)
  , Event(..)
  , setView
  , serverList
  , monumentList
  , dataLayer
  , beginPlayback
  , playbackScale
  , displayResults
  , focus
  , searchOverlay
  , animOverlay
  , baseLayer
  , pointColor
  , pointLocation
  , event)

import OHOLData as Data
import OHOLData.Decode as Decode
import OHOLData.Encode as Encode

import Json.Decode as Decode
import Json.Encode as Encode
import Time exposing (Posix)

type alias Point =
  { x : Int
  , y : Int
  , z : Int
  }

type PointColor
  = LineageColor
  | BirthTimeColor
  | ChainColor
  | CauseOfDeathColor

type PointLocation
  = BirthLocation
  | DeathLocation

setView : Point -> Cmd msg
setView {x, y, z} =
  Encode.object
    [ ("kind", Encode.string "setView")
    , ("x", Encode.int x)
    , ("y", Encode.int y)
    , ("z", Encode.int z)
    ]
    |> leafletCommand

serverList : List Data.Server -> Cmd msg
serverList servers =
  Encode.object
    [ ("kind", Encode.string "serverList")
    , ("servers", Encode.servers servers)
    ]
    |> leafletCommand

monumentList : Int -> Encode.Value -> Cmd msg
monumentList serverId monuments =
  Encode.object
    [ ("kind", Encode.string "monumentList")
    , ("server_id", Encode.int serverId)
    , ("monuments", monuments)
    ]
    |> leafletCommand

dataLayer : Encode.Value -> Cmd msg
dataLayer lives =
  Encode.object
    [ ("kind", Encode.string "dataLayer")
    , ("lives", lives)
    ]
    |> leafletCommand

beginPlayback : Int -> Int -> Posix -> Cmd msg
beginPlayback gameSecondsPerFrame frameRate startTime =
  Encode.object
    [ ("kind", Encode.string "beginPlayback")
    , ("start_time", Encode.int (Time.posixToMillis startTime))
    , ("game_seconds_per_frame", Encode.int gameSecondsPerFrame)
    , ("frame_rate", Encode.int frameRate)
    ]
    |> leafletCommand

playbackScale : Int -> Cmd msg
playbackScale gameSecondsPerFrame =
  Encode.object
    [ ("kind", Encode.string "playbackScale")
    , ("game_seconds_per_frame", Encode.int gameSecondsPerFrame)
    ]
    |> leafletCommand

displayResults : List Data.Life -> Cmd msg
displayResults lives =
  Encode.object
    [ ("kind", Encode.string "displayResults")
    , ("lives", Encode.lives lives)
    ]
    |> leafletCommand

focus : Data.Life -> Cmd msg
focus life =
  Encode.object
    [ ("kind", Encode.string "focus")
    , ("life", Encode.life life)
    ]
    |> leafletCommand

searchOverlay : Bool -> Cmd msg
searchOverlay status =
  Encode.object
    [ ("kind", Encode.string "searchOverlay")
    , ("status", Encode.bool status)
    ]
    |> leafletCommand

animOverlay : Bool -> Cmd msg
animOverlay status =
  Encode.object
    [ ("kind", Encode.string "animOverlay")
    , ("status", Encode.bool status)
    ]
    |> leafletCommand

baseLayer : String -> Cmd msg
baseLayer layer =
  Encode.object
    [ ("kind", Encode.string "baseLayer")
    , ("layer", Encode.string layer)
    ]
    |> leafletCommand

pointColor : PointColor -> Cmd msg
pointColor color =
  Encode.object
    [ ("kind", Encode.string "pointColor")
    , ("color", Encode.string <|
      case color of
        LineageColor -> "lineageColor"
        BirthTimeColor -> "birthTimeColor"
        ChainColor -> "chainColor"
        CauseOfDeathColor -> "causeOfDeathColor"
      )
    ]
    |> leafletCommand

pointLocation : PointLocation -> Cmd msg
pointLocation location =
  Encode.object
    [ ("kind", Encode.string "pointLocation")
    , ("location", Encode.string <|
      case location of
        BirthLocation -> "birth"
        DeathLocation -> "death"
      )
    ]
    |> leafletCommand

type Event
  = MoveEnd Point
  | OverlayAdd String (Maybe Int)
  | OverlayRemove String
  | SelectPoints (List Data.Life)
  | SidebarToggle

event : (Result Decode.Error Event -> msg) -> Sub msg
event tagger =
  leafletEvent (decodeEvent >> tagger)

decodeEvent : Decode.Value -> Result Decode.Error Event
decodeEvent thing =
  Decode.decodeValue eventDecoder thing
    |> Result.mapError (Debug.log "map decode error")

eventDecoder : Decode.Decoder Event
eventDecoder =
  (Decode.field "kind" Decode.string)
    |> Decode.andThen (\kind ->
      case kind of
        "moveend" ->
          Decode.map MoveEnd pointDecoder
        "overlayadd" ->
          Decode.map2 OverlayAdd
            (Decode.field "name" Decode.string)
            (Decode.field "server_id" (Decode.maybe Decode.int))
        "overlayremove" ->
          Decode.map OverlayRemove (Decode.field "name" Decode.string)
        "selectPoints" ->
           Decode.map SelectPoints (Decode.field "lives" Decode.lives)
        "sidebarToggle" ->
          Decode.succeed SidebarToggle
        _ ->
          Decode.fail kind
      )

pointDecoder : Decode.Decoder Point
pointDecoder =
  Decode.map3 Point
    (Decode.field "x" Decode.int)
    (Decode.field "y" Decode.int)
    (Decode.field "z" Decode.int)

port leafletCommand : Encode.Value -> Cmd msg
port leafletEvent : (Decode.Value -> msg) -> Sub msg
