port module Leaflet exposing
  ( Life
  , Cluster
  , Event(..)
  , setView
  , currentTime
  , currentServer
  , worldList
  , objectBounds
  , monumentList
  , notableObjects
  , dataLayer
  , displayResults
  , displayClusters
  , focusLife
  , focusPoint
  , focusPlacement
  , focusNone
  , lifeSearchOverlay
  , objectSearchOverlay
  , highlightObjects
  , animOverlay
  , timelineVisible
  , sidebar
  , overlayVisible
  , dataLayerVisible
  , pointColor
  , pointLocation
  , changeTheme
  , showOnlyCurrentMonuments
  , fadeTallObjects
  , showNaturalObjectsAboveZoom
  , activityMapSampleSize
  , showActivityMapBelowZoom
  , event)

import OHOLData as Data
import OHOLData.Decode
import OHOLData.Encode
import OHOLData.ParseMap as Parse
import Leaflet.Types exposing (..)
import Leaflet.Decode as Decode
import Leaflet.Encode as Encode

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Time exposing (Posix)

type alias Life = Leaflet.Types.Life
type alias Cluster = Leaflet.Types.Cluster

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
    , ("worlds", OHOLData.Encode.worlds worlds)
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

notableObjects : List Parse.Key -> Cmd msg
notableObjects locations =
  Encode.object
    [ ("kind", Encode.string "notableObjects")
    , ("locations", Encode.list keyPlacement locations)
    ]
    |> leafletCommand

keyPlacement : Parse.Key -> Encode.Value
keyPlacement (Parse.Key obj x y) =
  Encode.object
    [ ("id", Encode.int <| Parse.objectPlacementId obj)
    , ("x", Encode.int x)
    , ("y", Encode.int y)
    ]

dataLayer : List Life -> Bool -> Cmd msg
dataLayer lives sendRange =
  Encode.object
    [ ("kind", Encode.string "dataLayer")
    , ("lives", Encode.lives lives)
    , ("sendRange", Encode.bool sendRange)
    ]
    |> leafletCommand

displayResults : List Life -> Cmd msg
displayResults lives =
  Encode.object
    [ ("kind", Encode.string "displayResults")
    , ("lives", Encode.lives lives)
    ]
    |> leafletCommand

displayClusters : Dict Int (List Cluster) -> Cmd msg
displayClusters clusters =
  Encode.object
    [ ("kind", Encode.string "displayClusters")
    , ("clusters", Encode.clusters clusters)
    ]
    |> leafletCommand

focusLife : Life -> Cmd msg
focusLife life =
  Encode.object
    [ ("kind", Encode.string "focusLife")
    , ("life", Encode.life life)
    ]
    |> leafletCommand

focusPoint : Int -> Int -> Cmd msg
focusPoint x y =
  Encode.object
    [ ("kind", Encode.string "focusPoint")
    , ("x", Encode.int x)
    , ("y", Encode.int y)
    ]
    |> leafletCommand

focusPlacement : Int -> Int -> Posix -> Cmd msg
focusPlacement x y t =
  Encode.object
    [ ("kind", Encode.string "focusPlacement")
    , ("x", Encode.int x)
    , ("y", Encode.int y)
    , ("t", Encode.msTime t)
    ]
    |> leafletCommand

focusNone : Cmd msg
focusNone =
  Encode.object
    [ ("kind", Encode.string "focusNone")
    ]
    |> leafletCommand

lifeSearchOverlay : Bool -> Cmd msg
lifeSearchOverlay status =
  Encode.object
    [ ("kind", Encode.string "lifeSearchOverlay")
    , ("status", Encode.bool status)
    ]
    |> leafletCommand

objectSearchOverlay : Bool -> Cmd msg
objectSearchOverlay status =
  Encode.object
    [ ("kind", Encode.string "objectSearchOverlay")
    , ("status", Encode.bool status)
    ]
    |> leafletCommand

highlightObjects : List Int -> List Int -> Cmd msg
highlightObjects swatches images =
  Encode.object
    [ ("kind", Encode.string "highlightObjects")
    , ("swatches", Encode.list Encode.int swatches)
    , ("images", Encode.list Encode.int images)
    ]
    |> leafletCommand

animOverlay : Bool -> Cmd msg
animOverlay status =
  Encode.object
    [ ("kind", Encode.string "animOverlay")
    , ("status", Encode.bool status)
    ]
    |> leafletCommand

timelineVisible : Bool -> Cmd msg
timelineVisible status =
  Encode.object
    [ ("kind", Encode.string "timelineVisible")
    , ("status", Encode.bool status)
    ]
    |> leafletCommand

sidebar : String -> Cmd msg
sidebar status =
  Encode.object
    [ ("kind", Encode.string "sidebar")
    , ("sidebar", Encode.string status)
    ]
    |> leafletCommand

overlayVisible : String -> Bool -> Cmd msg
overlayVisible layer status =
  Encode.object
    [ ("kind", Encode.string "overlayVisible")
    , ("layer", Encode.string layer)
    , ("status", Encode.bool status)
    ]
    |> leafletCommand

dataLayerVisible : Bool -> Cmd msg
dataLayerVisible status =
  overlayVisible "Life Data" status

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

changeTheme : String -> Cmd msg
changeTheme theme =
  Encode.object
    [ ("kind", Encode.string "changeTheme")
    , ("theme", Encode.string theme)
    ]
    |> leafletCommand

showOnlyCurrentMonuments : Bool -> Cmd msg
showOnlyCurrentMonuments status =
  Encode.object
    [ ("kind", Encode.string "showOnlyCurrentMonuments")
    , ("status", Encode.bool status)
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

activityMapSampleSize : Int -> Cmd msg
activityMapSampleSize sampleSize =
  Encode.object
    [ ("kind", Encode.string "activityMapSampleSize")
    , ("sampleSize", Encode.int sampleSize)
    ]
    |> leafletCommand

showActivityMapBelowZoom : Int -> Cmd msg
showActivityMapBelowZoom zoom =
  Encode.object
    [ ("kind", Encode.string "showActivityMapBelowZoom")
    , ("zoom", Encode.int zoom)
    ]
    |> leafletCommand

type Event
  = MoveEnd Point
  | ZoomEnd Int
  | OverlayAdd String (Maybe Int)
  | OverlayRemove String
  | SelectPoints (List Life)
  | SelectLineage Int
  | DataRange Posix Posix
  | SidebarToggle
  | TimelineToggle
  | AnimToggle
  | DataAnimated Animatable

event : (Result Decode.Error Event -> msg) -> Sub msg
event tagger =
  leafletEvent (decodeEvent >> tagger)

decodeEvent : Decode.Value -> Result Decode.Error Event
decodeEvent thing =
  Decode.decodeValue eventDecoder thing

eventDecoder : Decode.Decoder Event
eventDecoder =
  (Decode.field "kind" Decode.string)
    |> Decode.andThen (\kind ->
      case kind of
        "moveend" ->
          Decode.map MoveEnd pointDecoder
        "zoomend" ->
          Decode.map ZoomEnd
            (Decode.field "z" Decode.int)
        "overlayadd" ->
          Decode.map2 OverlayAdd
            (Decode.field "name" Decode.string)
            (Decode.field "server_id" (Decode.maybe Decode.int))
        "overlayremove" ->
          Decode.map OverlayRemove (Decode.field "name" Decode.string)
        "selectPoints" ->
          Decode.map SelectPoints (Decode.field "lives" Decode.lives)
        "selectLineage" ->
          Decode.map SelectLineage (Decode.field "lineage" Decode.int)
        "dataRange" ->
          Decode.map2 DataRange
            (Decode.field "min" Decode.timeStamp)
            (Decode.field "max" Decode.timeStamp)
        "sidebarToggle" ->
          Decode.succeed SidebarToggle
        "timelineToggle" ->
          Decode.succeed TimelineToggle
        "animToggle" ->
          Decode.succeed AnimToggle
        "dataAnimated" ->
          Decode.map DataAnimated animatableDecoder
        _ ->
          Decode.fail kind
      )

pointDecoder : Decode.Decoder Point
pointDecoder =
  Decode.map3 Point
    (Decode.field "x" Decode.int)
    (Decode.field "y" Decode.int)
    (Decode.field "z" Decode.int)

animatableDecoder : Decode.Decoder Animatable
animatableDecoder =
  (Decode.field "animated" Decode.string)
    |> Decode.andThen (\a -> Decode.succeed (case a of
        "static" -> Static
        "animated" -> Animated
        _ -> Inert
      )
      )

port leafletCommand : Encode.Value -> Cmd msg
port leafletEvent : (Decode.Value -> msg) -> Sub msg
