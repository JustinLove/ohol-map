module LifeDataLayer exposing
  ( LifeDataLayer
  , LifeLogDay
  , empty
  , load
  , fromLives
  , livesReceived
  , resolveLivesIfLoaded
  , fail
  , displayRange
  , displayLineageOf
  , hasData
  , hasDataFor
  , canMakeRequest
  , shouldRequest
  , eventRange
  , currentLives
  , queryAroundTime
  , queryExactTime
  , queryLineageOfLife
  , neededDates
  , setLoading
  , allPossibleLifelogsRequired
  , population
  , clusters
  , clusterForLineage
  , PopulationSample
  , SettingValue
  )

import Clusters exposing (Clusters, Cluster)
import Leaflet.Types exposing (PointLocation(..))
import OHOLData as Data
import OHOLData.ParseLives as Parse
import RemoteData exposing (RemoteData(..))

import Calendar exposing (Date)
import Http
import Time exposing (Posix)

type alias LifeDataLayer =
  { serverId : Int
  , displayFilter : LifeDisplayFilter
  , lives : RemoteData (List Data.Life)
  , population : Maybe Population
  , clusters : Maybe Clusters
  , logs : List (Date, RemoteData LifeLogDay)
  }

type alias LifeLogDay =
  { serverId : Int
  , date : Date
  , lifelogs : List Parse.LifeLog
  , names : List Parse.NameLog
  }

type LifeDisplayFilter
  = DisplayRange Posix Posix
  | DisplayLineageOf Int
  | DisplayAll

empty : LifeDataLayer
empty =
  { serverId = 0
  , displayFilter = DisplayAll
  , lives = NotRequested
  , population = Nothing
  , clusters = Nothing
  , logs = []
  }

load : Int -> List Date -> LifeDataLayer
load server dates =
  { serverId = server
  , displayFilter = DisplayAll
  , lives = Loading
  , population = Nothing
  , clusters = Nothing
  , logs = List.map (\d -> (d, Loading)) dates
  }

fromLives : Int -> PointLocation -> Posix -> List Data.Life -> LifeDataLayer
fromLives server pointLocation defaultTime lives =
  { serverId = server
  , displayFilter = DisplayAll
  , lives = Data lives
  , population = Just (populationFromLives defaultTime lives)
  , clusters = Just (Clusters.fromLives pointLocation defaultTime lives)
  , logs = []
  }

livesReceived : LifeLogDay -> LifeDataLayer -> LifeDataLayer
livesReceived lifeLogDay data =
  if lifeLogDay.serverId /= data.serverId then
    data
  else
    { data | logs = updateLog lifeLogDay.date (Data lifeLogDay) data.logs }

updateLog : Date -> RemoteData LifeLogDay -> List (Date, RemoteData LifeLogDay) -> List (Date, RemoteData LifeLogDay)
updateLog date value logs =
  if List.any ((\(d,_) -> d == date)) logs then
    List.map (\tuple ->
      let d = Tuple.first tuple in
      if d == date then
        (d, value)
      else
        tuple
      )
      logs
  else
    (date, value) :: logs

resolveLivesIfLoaded : PointLocation -> Posix -> LifeDataLayer -> LifeDataLayer
resolveLivesIfLoaded pointLocation defaultTime data =
  if List.all (\(_,rd) -> rd /= Loading) data.logs then
    resolveLives pointLocation defaultTime data
  else
    data

resolveLives : PointLocation -> Posix -> LifeDataLayer -> LifeDataLayer
resolveLives pointLocation defaultTime data =
  let
    lives = resolveLifeLogs data.logs
      |> List.map (\life -> {life | serverId = data.serverId})
      |> applyDisplayFilter data.displayFilter
  in
  { serverId = data.serverId
  , displayFilter = data.displayFilter
  , lives = Data lives
  , population = Just (populationFromLives defaultTime lives)
  , clusters = Just (Clusters.fromLives pointLocation defaultTime lives)
  , logs = data.logs
  }

resolveLifeLogs : List (Date, RemoteData LifeLogDay) -> List Data.Life
resolveLifeLogs logs =
  let
    sortedLogs = logs
      |> List.sortBy (Tuple.first >> Calendar.toMillis)
    days = sortedLogs
      |> List.map Tuple.second
    namelessLives = days
      |> List.concatMap (RemoteData.map .lifelogs >> RemoteData.withDefault [])
      |> Parse.mergeLifeEvents
    names = days
      |> List.concatMap (RemoteData.map .names >> RemoteData.withDefault [])
  in
    Parse.mergeNames namelessLives names

applyDisplayFilter : LifeDisplayFilter -> List Data.Life -> List Data.Life
applyDisplayFilter filter lives =
  case filter of
    DisplayRange startTime endTime ->
      let
        startMs = Time.posixToMillis startTime
        endMs = Time.posixToMillis endTime
      in
        List.filter (\life ->
            let ms = Time.posixToMillis life.birthTime in
            startMs <= ms && ms <= endMs
          )
          lives
    DisplayLineageOf playerid ->
      let
        mplayer = lives
          |> List.filter (\life -> life.playerid == playerid)
          |> List.head
      in
        case mplayer of
          Just player -> List.filter (\life -> life.lineage == player.lineage) lives
          Nothing -> []
    DisplayAll ->
      lives

fail : Int -> Date -> Http.Error -> LifeDataLayer -> LifeDataLayer
fail server date error data =
  { serverId = server
  , displayFilter = data.displayFilter
  , lives = Failed error
  , population = Nothing
  , clusters = Nothing
  , logs = [(date, Failed error)]
  }

displayAll : LifeDataLayer -> LifeDataLayer
displayAll data =
  { data
  | displayFilter = DisplayAll
  }

displayRange : Posix -> Posix -> LifeDataLayer -> LifeDataLayer
displayRange startTime endTime data =
  { data
  | displayFilter = DisplayRange startTime endTime
  }

displayLineageOf : Int -> LifeDataLayer -> LifeDataLayer
displayLineageOf playerId data =
  { data
  | displayFilter = DisplayLineageOf playerId
  }


hasData : LifeDataLayer -> Bool
hasData data =
  data.lives /= NotRequested

hasDataFor : Int -> LifeDataLayer -> Bool
hasDataFor serverId data =
  case data.lives of
    NotRequested -> False
    NotAvailable -> False
    Loading -> False
    Data _ -> data.serverId == serverId
    Failed _ -> False

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

currentLives : LifeDataLayer -> List Data.Life
currentLives data =
  data.lives |> RemoteData.withDefault []

queryAroundTime : Int -> Posix -> Posix -> LifeDataLayer -> LifeDataLayer
queryAroundTime serverId startTime endTime data =
  update serverId startTime endTime data
    |> displayAll

queryExactTime : Int -> Posix -> Posix -> LifeDataLayer -> LifeDataLayer
queryExactTime serverId startTime endTime data =
  update serverId startTime endTime data
    |> displayRange startTime endTime

oneHour = 60 * 60 * 1000

queryLineageOfLife : Int -> Int -> Posix -> LifeDataLayer -> LifeDataLayer
queryLineageOfLife serverId playerid startTime data =
  case eventRange data of
    Just (dataStart, dataEnd) ->
      let
        startMs = startTime |> Time.posixToMillis
        lineageBirthTimes = data.logs
          |> resolveLifeLogs
          |> applyDisplayFilter (DisplayLineageOf playerid)
          |> List.map (.birthTime>>Time.posixToMillis)
        firstBirth = List.minimum lineageBirthTimes
          |> Maybe.withDefault startMs
        lastBirth = List.maximum lineageBirthTimes
          |> Maybe.withDefault startMs
        dataStartMs = Time.posixToMillis dataStart
        dataEndMs = Time.posixToMillis dataEnd
        potentialStart = firstBirth - oneHour
        potentialEnd = lastBirth + oneHour
        potentialStartPosix = potentialStart |> Time.millisToPosix
        potentialEndPosix = potentialEnd |> Time.millisToPosix
      in
        update serverId potentialStartPosix potentialEndPosix data
          |> displayLineageOf playerid
    Nothing ->
      update serverId startTime startTime data
        |> displayLineageOf playerid

update : Int -> Posix -> Posix -> LifeDataLayer -> LifeDataLayer
update serverId startTime endTime data =
  let
    newDates = allPossibleLifelogsRequired startTime endTime
    relevantLogs = data.logs
      |> List.filter (logIsOnServer serverId)
      |> List.filter (logIsInDates newDates)
    relevantDates = relevantLogs
      |> List.map Tuple.first
    missingLogs = newDates
      |> List.filter (\date -> not <| List.member date relevantDates)
      |> List.map (\date -> (date, NotRequested))
  in
    { data
    | serverId = serverId
    , logs = List.append relevantLogs missingLogs
    }

neededDates : LifeDataLayer -> List Date
neededDates data =
  data.logs
    |> List.filter (\(_,rd) -> rd == NotRequested)
    |> List.map Tuple.first

setLoading : LifeDataLayer -> LifeDataLayer
setLoading data =
  { serverId = data.serverId
  , displayFilter = data.displayFilter
  , lives = Loading
  , population = Nothing
  , clusters = Nothing
  , logs = data.logs
    |> List.map (\(date,rd) -> if rd == NotRequested then (date,Loading) else (date,rd))
  }

logIsOnServer : Int -> (Date, RemoteData LifeLogDay) -> Bool
logIsOnServer serverId (date, rdLog) =
  rdLog
    |> RemoteData.map (\log -> log.serverId == serverId)
    |> RemoteData.withDefault False

logIsInDates : List Date -> (Date, a) -> Bool
logIsInDates dates (date, _) =
  List.member date dates

allPossibleLifelogsRequired : Posix -> Posix -> List Date
allPossibleLifelogsRequired startTime endTime =
  Calendar.getDateRange (Calendar.fromPosix startTime) (Calendar.fromPosix endTime)
    |> limit 7

limit : Int -> List a -> List a
limit largest list =
  let
    length = List.length list
  in
    List.drop (max 0 (length - largest)) list

lifelogFiles : String -> Int -> String -> Posix -> List String
lifelogFiles lifeLogUrl serverId serverName time =
  let
    filename = dateYearMonthMonthDayWeekday Time.utc time
  in
  [ lifeLogUrl
      |> String.replace "{server}" serverName
      |> String.replace "{filename}" filename
  , lifeLogUrl
      |> String.replace "{server}" serverName
      |> String.replace "{filename}" (filename ++ "_names")
  ]

dateYearMonthMonthDayWeekday : Time.Zone -> Posix -> String
dateYearMonthMonthDayWeekday zone time =
  let
    year = Time.toYear zone time |> String.fromInt
    month = Time.toMonth zone time |> formatMonth
    day = Time.toDay zone time |> String.fromInt |> String.padLeft 2 '0'
    weekday = Time.toWeekday zone time |> formatWeekday
  in
    year ++ "_" ++ month ++ "_" ++ day ++ "_" ++ weekday

formatMonth : Time.Month -> String
formatMonth month =
  case month of
    Time.Jan -> "01January"
    Time.Feb -> "02February"
    Time.Mar -> "03March"
    Time.Apr -> "04April"
    Time.May -> "05May"
    Time.Jun -> "06June"
    Time.Jul -> "07July"
    Time.Aug -> "08August"
    Time.Sep -> "09September"
    Time.Oct -> "10October"
    Time.Nov -> "11November"
    Time.Dec -> "12December"

formatWeekday : Time.Weekday -> String
formatWeekday weekday =
  case weekday of
    Time.Mon -> "Monday"
    Time.Tue -> "Tuesday"
    Time.Wed -> "Wednesday"
    Time.Thu -> "Thursday"
    Time.Fri -> "Friday"
    Time.Sat -> "Saturday"
    Time.Sun -> "Sunday"

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
