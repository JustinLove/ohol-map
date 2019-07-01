port module Leaflet exposing
  ( Point
  , Event(..)
  , setView
  , focus
  , displayResults
  , searchOverlay
  , event)

import OHOLData as Data
import OHOLData.Decode as Decode
import OHOLData.Encode as Encode

import Json.Decode as Decode
import Json.Encode as Encode

type alias Point =
  { x : Int
  , y : Int
  , z : Int
  }

setView : Point -> Cmd msg
setView {x, y, z} =
  Encode.object
    [ ("kind", Encode.string "setView")
    , ("x", Encode.int x)
    , ("y", Encode.int y)
    , ("z", Encode.int z)
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


type Event
  = Error
  | MoveEnd Point
  | OverlayAdd String
  | OverlayRemove String
  | SelectPoints (List Data.Life)

event : (Event -> msg) -> Sub msg
event tagger =
  leafletEvent (decodeEvent >> tagger)

decodeEvent : Decode.Value -> Event
decodeEvent thing =
  Decode.decodeValue eventDecoder thing
    |> Result.mapError (Debug.log "map decode error")
    |> Result.withDefault Error

eventDecoder : Decode.Decoder Event
eventDecoder =
  (Decode.field "kind" Decode.string)
    |> Decode.andThen (\kind ->
      case kind of
        "moveend" ->
          Decode.map MoveEnd pointDecoder
        "overlayadd" ->
          Decode.map OverlayAdd (Decode.field "name" Decode.string)
        "overlayremove" ->
          Decode.map OverlayRemove (Decode.field "name" Decode.string)
        "selectPoints" ->
           Decode.map SelectPoints (Decode.field "lives" Decode.lives)
        _ ->
          Decode.succeed Error
      )

pointDecoder : Decode.Decoder Point
pointDecoder =
  Decode.map3 Point
    (Decode.field "x" Decode.int)
    (Decode.field "y" Decode.int)
    (Decode.field "z" Decode.int)

port leafletCommand : Encode.Value -> Cmd msg
port leafletEvent : (Decode.Value -> msg) -> Sub msg
