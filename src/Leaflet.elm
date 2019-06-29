port module Leaflet exposing (Point, Event(..), setView, event)

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

type Event
  = Error
  | MoveEnd Point
  | OverlayAdd String
  | OverlayRemove String

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
        "moveend" -> Decode.map MoveEnd pointDecoder
        "overlayadd" -> Decode.map OverlayAdd (Decode.field "name" Decode.string)
        "overlayremove" -> Decode.map OverlayRemove (Decode.field "name" Decode.string)
        _ -> Decode.succeed Error
      )

pointDecoder : Decode.Decoder Point
pointDecoder =
  Decode.map3 Point
    (Decode.field "x" Decode.int)
    (Decode.field "y" Decode.int)
    (Decode.field "z" Decode.int)

port leafletCommand : Encode.Value -> Cmd msg
port leafletEvent : (Decode.Value -> msg) -> Sub msg
