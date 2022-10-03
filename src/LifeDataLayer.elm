module LifeDataLayer exposing
  ( LifeDataLayer
  , empty
  , load
  , withLives
  , failed
  , hasData
  , canMakeRequest
  , eventRange
  , population
  , PopulationSample
  , SettingValue
  )

import OHOLData as Data
import RemoteData exposing (RemoteData(..))

import Http
import Time exposing (Posix)

type alias LifeDataLayer =
  { serverId : Int
  , lives : RemoteData Int
  , population : Maybe Population
  }

empty : LifeDataLayer
empty =
  { serverId = 0
  , lives = NotRequested
  , population = Nothing
  }

load : Int -> LifeDataLayer
load server =
  { serverId = server
  , lives = Loading
  , population = Nothing
  }

withLives : Int -> Posix -> List Data.Life -> LifeDataLayer
withLives server defaultTime lives =
  { serverId = server
  , lives = Data server
  , population = Just (populationFromLives defaultTime lives)
  }

failed : Int -> Http.Error -> LifeDataLayer
failed server error =
  { serverId = server
  , lives = Failed error
  , population = Nothing
  }

hasData : LifeDataLayer -> Bool
hasData data =
  data.lives /= NotRequested

canMakeRequest : LifeDataLayer -> Bool
canMakeRequest data =
  case data.lives of
    NotRequested -> True
    NotAvailable -> False
    Loading -> False
    Data _ -> True
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
