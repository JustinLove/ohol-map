port module Leaflet exposing (Point, setView)

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

port leafletCommand : Encode.Value -> Cmd msg
port leafletEvent : (Encode.Value -> msg) -> Sub msg
