module Clusters exposing
  ( Clusters
  , fromLives
  , atTime
  , displayClusters
  )

import Leaflet.Types exposing (PointLocation(..))
import OHOLData as Data

import Dict exposing(Dict)
import Time exposing (Posix)

type alias LocationSample =
  { time : Posix
  , lineage : Int
  , x : Int
  , y : Int
  }

type alias Cluster =
  { x : Int
  , y : Int
  , members : Int
  }

type alias Lineage =
  { locations : List LocationSample
  , clusters : List Cluster
  }

type alias Clusters =
  { locations : List LocationSample
  , lineages : Dict Int Lineage
  }

locationsFromLives : PointLocation -> Posix -> List Data.Life -> List LocationSample
locationsFromLives pointLocation default data =
  let
    samples =
      case pointLocation of
        BirthLocation ->
          data
            |> List.map (\{lineage, birthTime, birthX, birthY} ->
                { time = birthTime
                , lineage = lineage
                , x = birthX
                , y = birthY
                }
              )
        DeathLocation ->
          data
            |> List.filterMap (\{lineage, deathTime, deathX, deathY} ->
              Maybe.map3 (\dt dx dy ->
                  { time = dt
                  , lineage = lineage
                  , x = dx
                  , y = dy
                  }
                )
                deathTime deathX deathY
              )
  in
    samples-- |> List.sortBy (.time>>Time.posixToMillis)

fromLives : PointLocation -> Posix -> List Data.Life -> Clusters
fromLives pointLocation default data =
  let
    locations = locationsFromLives pointLocation default data
    lineageLocations = groupLineages locations
  in
  { locations = locations
  , lineages = mapValues lineageClusters lineageLocations
  }

atTime : Posix -> Clusters -> Clusters
atTime time clusters =
  let
    start = time
      |> Time.posixToMillis
      |> (\x -> x - 60*60*1000)
      |> Time.millisToPosix
    range = (start, time)
    locationsAtTime = clusters.locations
      |> List.filter (.time>>(isInRange range))
    lineageLocations = groupLineages locationsAtTime
  in
  { clusters | lineages = mapValues lineageClusters lineageLocations }

groupLineages : List LocationSample -> Dict Int (List LocationSample)
groupLineages = groupListToDict .lineage

groupListToDict : (a -> comparable) -> List a -> Dict comparable (List a)
groupListToDict toKey list =
  List.foldl (\value ->
      Dict.update (toKey value) (\mprev ->
        case mprev of
          Just prev ->
            Just (value :: prev)
          Nothing ->
            Just ([ value ])
      )
    )
    Dict.empty
    list

mapValues : (a -> b) -> Dict comparable a -> Dict comparable b
mapValues f dict =
  Dict.map (\_ a -> f a) dict

clustering : List LocationSample -> List Cluster
clustering locations =
  List.foldl clusterStep [] locations
    |> List.sortBy (\{members} -> -members)

clusterStep : LocationSample -> List Cluster -> List Cluster
clusterStep location clusters =
  case nearestCluster 200 location clusters of
    Just best ->
      List.map (clusterUpdate location best) clusters
    Nothing ->
      { x = location.x
      , y = location.y
      , members = 1
      } :: clusters

nearestCluster : Int -> LocationSample -> List Cluster -> Maybe Cluster
nearestCluster range {x, y} clusters =
  let
    range2 = range*range
  in
  (List.foldl (\candidate best ->
      let
        dx = x - candidate.x
        dy = y - candidate.y
        d = dx*dx + dy*dy
      in
      if d >= range2 then
        best
      else
        case best of
          Just (bd, bc) ->
            if d < bd then
              Just (d, candidate)
            else
              Just (bd, bc)
          Nothing ->
            Just (d, candidate)
    )
    Nothing
    clusters
  )
    |> Maybe.map Tuple.second

clusterFactor = 0.3

clusterUpdate : LocationSample -> Cluster -> Cluster -> Cluster
clusterUpdate location previous cluster =
  if cluster == previous then
    { x = cluster.x + (round ((toFloat (location.x - cluster.x)) * clusterFactor))
    , y = cluster.y + (round ((toFloat (location.y - cluster.y)) * clusterFactor))
    , members = cluster.members + 1
    }
  else
    cluster

lineageClusters : List LocationSample -> Lineage
lineageClusters locations =
  { locations = locations
  , clusters = clustering locations
  }

displayClusters : Bool -> Int -> Clusters -> Dict Int (List Cluster)
displayClusters dataAnimated zoom clusters =
  let
    threashold =
      if dataAnimated then
        2
      else
        2^(26-zoom)
  in
  clusters
    |> .lineages
    |> Dict.map (\k v ->
        v.clusters
          |> List.filter (\{members} -> members > threashold)
          |> List.map (\{x,y,members} ->
              { x = collapseToWellGrid x
              , y = collapseToWellGrid y
              , members = members
              }
            )
      )

collapseToWellGrid : Int -> Int
collapseToWellGrid x =
  x + 20 - (modBy 40 (x + 20))

isInRange : (Posix, Posix) -> Posix -> Bool
isInRange (mint, maxt) t =
  let
    mini = Time.posixToMillis mint
    maxi = Time.posixToMillis maxt
    i = Time.posixToMillis t
  in
    mini < i && i <= maxi
