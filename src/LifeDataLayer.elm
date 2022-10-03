module LifeDataLayer exposing
  ( LifeDataLayer
  , empty
  , load
  , fromLives
  , fail
  , hasData
  , hasDataFor
  , canMakeRequest
  , shouldRequest
  , eventRange
  , population
  , clusters
  , clusterForLineage
  , PopulationSample
  , SettingValue
  )

import Clusters exposing (Clusters, Cluster)
import Leaflet.Types exposing (PointLocation(..))
import OHOLData as Data
import RemoteData exposing (RemoteData(..))

import Http
import Time exposing (Posix)

type alias LifeDataLayer =
  { serverId : Int
  , lives : RemoteData Int
  , population : Maybe Population
  , clusters : Maybe Clusters
  }

empty : LifeDataLayer
empty =
  { serverId = 0
  , lives = NotRequested
  , population = Nothing
  , clusters = Nothing
  }

load : Int -> LifeDataLayer
load server =
  { serverId = server
  , lives = Loading
  , population = Nothing
  , clusters = Nothing
  }

fromLives : Int -> PointLocation -> Posix -> List Data.Life -> LifeDataLayer
fromLives server pointLocation defaultTime lives =
  { serverId = server
  , lives = Data server
  , population = Just (populationFromLives defaultTime lives)
  , clusters = Just (Clusters.fromLives pointLocation defaultTime lives)
  }

fail : Int -> Http.Error -> LifeDataLayer
fail server error =
  { serverId = server
  , lives = Failed error
  , population = Nothing
  , clusters = Nothing
  }

hasData : LifeDataLayer -> Bool
hasData data =
  data.lives /= NotRequested

hasDataFor : Int -> LifeDataLayer -> Bool
hasDataFor serverId data =
  data.lives == Data serverId

canMakeRequest : LifeDataLayer -> Bool
canMakeRequest data =
  case data.lives of
    NotRequested -> True
    NotAvailable -> False
    Loading -> False
    Data _ -> True
    Failed _ -> True

shouldRequest : LifeDataLayer -> Bool
shouldRequest data =
  case data.lives of
    NotRequested -> True
    NotAvailable -> False
    Loading -> False
    Data _ -> False
    Failed _ -> True

type alias PopulationSample = (Posix, Int)
type alias SettingValue = (Posix, Int) -- e.g. race biome specialization population limit

type alias Population =
  { data : List PopulationSample
  , eventRange : (Posix, Posix)
  , populationRange : (Posix, Posix)
  }

population : LifeDataLayer -> Maybe Population
population data =
  data.population

eventRange : LifeDataLayer -> Maybe (Posix, Posix)
eventRange data =
  population data |> Maybe.map .eventRange

clusters : LifeDataLayer -> Maybe Clusters
clusters data =
  data.clusters

clusterForLineage : Int -> LifeDataLayer -> Maybe Cluster
clusterForLineage lineageId data =
  clusters data |> Maybe.andThen (Clusters.forLineage lineageId)

populationFromLives : Posix -> List Data.Life -> Population
populationFromLives default unsortedData =
  let
    data = List.sortBy (.birthTime>>Time.posixToMillis>>negate) unsortedData
    lastBirth = data
      |> List.head
      |> Maybe.map .birthTime
      |> Maybe.withDefault default
    firstEvent = data
      |> List.foldl (\{birthTime} _ -> birthTime) default
    usablePoints = data
      |> List.concatMap (\{birthTime, birthPopulation, deathTime, deathPopulation} ->
          case (deathTime, deathPopulation) of
            (Just dt, Just dp) -> [(birthTime, birthPopulation), (dt, dp)]
            _ -> [(birthTime, birthPopulation)]
        )
      |> List.sortBy (Tuple.first>>Time.posixToMillis)
      |> resample 4096
    lastEvent = usablePoints
      |> List.foldl (\(t, _) _ -> t) default
  in
  { data = usablePoints
  , eventRange = (firstEvent, lastBirth)
  , populationRange = (firstEvent, lastEvent)
  }

resample : Int -> List a -> List a
resample targetSamples input =
  let
    length = List.length input
  in
    if length <= targetSamples then
      input
    else
      input
        |> List.foldl
          (\x (accum, out) ->
            let next = accum + targetSamples in
            if next >= length then
              (next - length, x::out)
            else
              (next, out)
          )
          (0, [])
        |> Tuple.second
        |> List.reverse
