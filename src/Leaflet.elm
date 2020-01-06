port module Leaflet exposing
  ( Point
  , PointColor(..)
  , PointLocation(..)
  , Event(..)
  , setView
  , currentTime
  , currentServer
  , worldList
  , objectBounds
  , monumentList
  , dataLayer
  , displayResults
  , focus
  , searchOverlay
  , animOverlay
  , baseLayer
  , pointColor
  , pointLocation
  , fadeTallObjects
  , showNaturalObjectsAboveZoom
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
  | AgeColor

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

currentTime : Posix -> Cmd msg
currentTime time =
  Encode.object
    [ ("kind", Encode.string "currentTime")
    , ("time", Encode.timeStamp time)
    ]
    |> leafletCommand

currentServer : Int -> Cmd msg
currentServer serverId =
  Encode.object
    [ ("kind", Encode.string "currentServer")
    , ("serverId", Encode.int serverId)
    ]
    |> leafletCommand

worldList : List Data.World -> Cmd msg
worldList worlds =
  Encode.object
    [ ("kind", Encode.string "worldList")
    , ("worlds", Encode.worlds worlds)
    ]
    |> leafletCommand

objectBounds : Encode.Value -> Encode.Value -> Cmd msg
objectBounds ids bounds =
  Encode.object
    [ ("kind", Encode.string "objectBounds")
    , ("ids", ids)
    , ("bounds", bounds)
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
        AgeColor -> "ageColor"
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

fadeTallObjects : Bool -> Cmd msg
fadeTallObjects status =
  Encode.object
    [ ("kind", Encode.string "fadeTallObjects")
    , ("status", Encode.bool status)
    ]
    |> leafletCommand

showNaturalObjectsAboveZoom : Int -> Cmd msg
showNaturalObjectsAboveZoom zoom =
  Encode.object
    [ ("kind", Encode.string "showNaturalObjectsAboveZoom")
    , ("zoom", Encode.int zoom)
    ]
    |> leafletCommand


type Event
  = MoveEnd Point
  | OverlayAdd String (Maybe Int)
  | OverlayRemove String
  | SelectPoints (List Data.Life)
  | DataRange Posix Posix
  | SidebarToggle
  | AnimToggle

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
        "dataRange" ->
          Decode.map2 DataRange
            (Decode.field "min" Decode.timeStamp)
            (Decode.field "max" Decode.timeStamp)
        "sidebarToggle" ->
          Decode.succeed SidebarToggle
        "animToggle" ->
          Decode.succeed AnimToggle
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
