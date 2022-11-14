module LifeDataLayer exposing
  ( LifeDataLayer
  , LifeLogDay
  , empty
  , fail
  , livesReceived
  , loadingCount
  , resolveLivesIfLoaded
  , hasData
  , hasDataFor
  , canMakeRequest
  , loadingProgress
  , shouldRequest
  , eventRange
  , currentLives
  , isDisplayingExactRange
  , isDisplayingSingleLineage
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

fail : Int -> Date -> Http.Error -> LifeDataLayer -> LifeDataLayer
fail server date error data =
  if server /= data.serverId then
    data
  else
    { data | logs = updateLog date (Failed error) data.logs }

livesReceived : LifeLogDay -> LifeDataLayer -> LifeDataLayer
livesReceived lifeLogDay data =
  if lifeLogDay.serverId /= data.serverId then
    data
  else
    { data | logs = updateLog lifeLogDay.date (Data lifeLogDay) data.logs }

loadingCount : LifeDataLayer -> Int
loadingCount data =
  List.foldr (\(_,rd) accum -> if rd == Loading then accum + 1 else accum) 0 data.logs

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
      |> expandingQuery defaultTime
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
  , clusters = Just (Clusters.fromLives pointLocation lives)
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

expandingQuery : Posix -> LifeDataLayer -> LifeDataLayer
expandingQuery defaultTime data =
  case data.displayFilter of
    DisplayLineageOf playerid ->
      expandingQueryLineageOfLife data.serverId playerid defaultTime data
    _ ->
      data

displayAll : LifeDataLayer -> LifeDataLayer
displayAll data =
  changeDisplay (DisplayAll) data

displayRange : Posix -> Posix -> LifeDataLayer -> LifeDataLayer
displayRange startTime endTime data =
  changeDisplay (DisplayRange startTime endTime) data

displayLineageOf : Int -> LifeDataLayer -> LifeDataLayer
displayLineageOf playerId data =
  changeDisplay (DisplayLineageOf playerId) data

changeDisplay : LifeDisplayFilter -> LifeDataLayer -> LifeDataLayer
changeDisplay filter data =
  if data.displayFilter /= filter then
    { data | displayFilter = filter }
      |> resolveLives BirthLocation (Time.millisToPosix 0)
  else
    data

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

loadingProgress : LifeDataLayer -> (Int, Int)
loadingProgress data =
  (List.foldl (\(_,rd) accum -> if rd /= Loading then accum + 1 else accum) 0 data.logs, List.length data.logs)

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

isDisplayingExactRange : LifeDataLayer -> Bool
isDisplayingExactRange data =
  case data.displayFilter of
    DisplayRange _ _ -> True
    _ -> False

isDisplayingSingleLineage : LifeDataLayer -> Bool
isDisplayingSingleLineage data =
  case data.displayFilter of
    DisplayLineageOf _ -> True
    _ -> False

queryAroundTime : Int -> Posix -> Posix -> Int -> LifeDataLayer -> LifeDataLayer
queryAroundTime serverId startTime endTime maxLogs data =
  updateAndDrop serverId startTime endTime maxLogs data
    |> displayAll

queryExactTime : Int -> Posix -> Posix -> Int -> LifeDataLayer -> LifeDataLayer
queryExactTime serverId startTime endTime maxLogs data =
  updateAndDrop serverId startTime endTime maxLogs data
    |> displayRange startTime endTime

oneHour = 60 * 60 * 1000

oneHourAround : (Posix, Posix) -> (Posix, Posix)
oneHourAround (start, end) =
  ( Time.millisToPosix ((Time.posixToMillis start) - oneHour)
  , Time.millisToPosix ((Time.posixToMillis end) + oneHour)
  )

queryLineageOfLife : Int -> Int -> Posix -> LifeDataLayer -> LifeDataLayer
queryLineageOfLife serverId playerid startTime dataWithUnknownDisplay =
  let

    data = displayLineageOf playerid dataWithUnknownDisplay
    (start, end) = eventRange data
      |> Maybe.map oneHourAround
      |> Maybe.withDefault (startTime, startTime)
  in
    updateUnlimited serverId start end data

expandingQueryLineageOfLife : Int -> Int -> Posix -> LifeDataLayer -> LifeDataLayer
expandingQueryLineageOfLife serverId playerid startTime dataWithUnknownDisplay =
  let
    data = displayLineageOf playerid dataWithUnknownDisplay
    mrange = eventRange data
      |> Maybe.map oneHourAround
  in
    case mrange of
      Just (start, end) -> updateUnlimited serverId start end data
      Nothing -> data

updateAndDrop : Int -> Posix -> Posix -> Int -> LifeDataLayer -> LifeDataLayer
updateAndDrop serverId startTime endTime maxLogs data =
  let
    newDates = allPossibleLifelogsRequired startTime endTime
      |> limit maxLogs
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

updateUnlimited : Int -> Posix -> Posix -> LifeDataLayer -> LifeDataLayer
updateUnlimited serverId startTime endTime data =
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
    |> RemoteData.withDefault True -- only data has server, have to assume others are for this server or we get an infinite loop. Maybe needs to be part tuple(triple)?

logIsInDates : List Date -> (Date, a) -> Bool
logIsInDates dates (date, _) =
  List.member date dates

allPossibleLifelogsRequired : Posix -> Posix -> List Date
allPossibleLifelogsRequired startTime endTime =
  Calendar.getDateRange (Calendar.fromPosix startTime) (Calendar.fromPosix endTime)

limit : Int -> List a -> List a
limit largest list =
  let
    length = List.length list
  in
    List.drop (max 0 (length - largest)) list

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
