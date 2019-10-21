module OHOLData exposing
  ( Life
  , Server
  , Arc
  , Objects
  , VersionChange
  , Spawn
  , SpawnChange(..)
  , Version
  , completeVersions
  , ages
  )

import Dict exposing (Dict)
import Iso8601
import Json.Decode exposing (Value)
import Set exposing (Set)
import Time exposing (Posix)

type alias Life =
  { birthX : Int
  , birthY : Int
  , birthTime : Posix
  , chain : Int
  , lineage : Int
  , name : Maybe String
  , serverId : Int
  , epoch : Int
  , playerid : Int
  , age : Float
  }

type alias Server =
  { id : Int
  , serverName : String
  , minTime: Posix
  , maxTime: Posix
  }

type alias Arc =
  { serverId : Int
  , start: Posix
  , end: Posix
  , seed: Int
  }

type alias Objects =
  { ids: Value
  , bounds: Value
  , spawnChanges: List VersionChange
  }

type alias VersionChange =
  { id: Int
  , start: Posix
  , spawnChanges: List SpawnChange
  }

type alias Spawn =
  { id: Int
  , mapChance: Float
  , biomes: List Int
  , moving: Bool
  , wide: Bool
  , leftBlockingRadius: Int
  , rightBlockingRadius: Int
  , gridPlacement: Maybe Int
  , randPlacement: Maybe Int
  }

type SpawnChange
  = Removed Int
  | Changed Spawn

type alias Biome =
  { id: Int
  , objects: OrderedList Spawn
  , totalChanceWeight: Float
  }

type alias BiomeSet = Dict Int Biome

type alias Version =
  { id: Int
  , start: Posix
  , objects: Dict Int Spawn
  , biomes: BiomeSet
  , gridPlacements: List Spawn
  , randPlacements: List Spawn
  }

completeVersions : List VersionChange -> List Version
completeVersions changes =
  changes
    |> List.foldl addVersionChange []
    |> List.reverse

emptyVersion : Version
emptyVersion =
  { id = 0
  , start = (Time.millisToPosix 0)
  , objects = Dict.empty
  , biomes = Dict.empty
  , gridPlacements = []
  , randPlacements = []
  }

addVersionChange : VersionChange -> List Version -> List Version
addVersionChange change versions =
  let
    previous = versions
      |> List.head
      |> Maybe.withDefault emptyVersion
  in
    (applyVersionChange change previous) :: versions

applyVersionChange : VersionChange -> Version -> Version
applyVersionChange change previous =
  { id = change.id
  , start = change.start
  , objects = List.foldl applySpawnChangeToObjects previous.objects change.spawnChanges
  , biomes = change.spawnChanges
    |> List.foldl applySpawnChangeToBiomes previous.biomes
    |> Dict.map (\bid biome -> calculateChance biome)
  , gridPlacements = List.foldl (applySpawnChangeToPlacements .gridPlacement) previous.gridPlacements change.spawnChanges
  , randPlacements = List.foldl (applySpawnChangeToPlacements .randPlacement) previous.randPlacements change.spawnChanges
  }

applySpawnChangeToObjects : SpawnChange -> Dict Int Spawn -> Dict Int Spawn
applySpawnChangeToObjects change objects =
  case change of
    Removed id -> Dict.remove id objects
    Changed spawn -> Dict.insert spawn.id spawn objects

applySpawnChangeToBiomes : SpawnChange -> BiomeSet -> BiomeSet
applySpawnChangeToBiomes change biomes =
  case change of
    Removed id ->
      biomes
        |> Dict.map (\bid biome -> {biome | objects = orderedRemove .id id biome.objects})
    Changed spawn ->
      if spawn.gridPlacement == Nothing && spawn.randPlacement == Nothing then
        spawn.biomes
          |> List.foldl (changeSpawn spawn) biomes
      else
        biomes

calculateChance : Biome -> Biome
calculateChance biome =
  { biome
  | totalChanceWeight =
    biome.objects
      |> List.map .mapChance
      |> List.foldl (+) 0
  }

changeSpawn : Spawn -> Int -> BiomeSet -> BiomeSet
changeSpawn spawn bid biomes =
  Dict.update bid (\mbiome ->
    case mbiome of
      Just biome -> Just <| {biome | objects = orderedUpdate .id spawn biome.objects}
      Nothing -> Just
        { id = bid
        , objects = [spawn]
        , totalChanceWeight = 0
        }
    ) biomes

applySpawnChangeToPlacements : (Spawn -> Maybe a) -> SpawnChange -> OrderedList Spawn -> OrderedList Spawn
applySpawnChangeToPlacements filter change placements =
  case change of
    Removed id -> orderedRemove .id id placements
    Changed spawn ->
      if (filter spawn) /= Nothing then
        orderedUpdate .id spawn placements
      else
        orderedRemove .id spawn.id placements

type alias OrderedList a = List a

orderedRemove : (a -> id) -> id -> OrderedList a -> OrderedList a
orderedRemove fid id list =
  List.filter (\x -> (fid x) /= id) list

orderedUpdate : (a -> comparable) -> a -> OrderedList a -> OrderedList a
orderedUpdate fid x list =
  case list of
    h :: t ->
      if (fid h) == (fid x) then
        x :: t
      else if (fid x) < (fid h) then
        x :: h :: t
      else
        h :: orderedUpdate fid x t
    [] ->
      [x]

type alias Age =
  { name: String
  , start: Posix
  , end: Maybe Posix
  --, generation: ???/
  }

humanTime : String -> Posix
humanTime s =
  Iso8601.toTime s
    |> Result.mapError (Debug.log ("time error " ++ s))
    |> Result.withDefault (Time.millisToPosix 0)

ages : List Age
ages =
  [ { name = "Badlands Age"
    , start = Time.millisToPosix 0
    , end = Nothing
    --, biomeLayer = badlandsAge
    --, generation =
    --  { allowOffBiomeMovingObjects = true
    --  , biomeMap = badlandsBiomeMap
    --  }
    }
  , { name = "Arctic Age"
    , start = humanTime "2018-03-08"
    , end = Nothing
    --, generation =
    --  { allowOffBiomeMovingObjects = true
    --  , biomeMap = arcticBiomeMap
    --  }
    }
  , { name = "Desert Age"
    , start = humanTime "2018-03-31"
    , end = Nothing
    --  generation = {
    --  , allowOffBiomeMovingObjects = true
    --  , biomeMap = desertBiomeMap
    --  }
    }
  , { name = "Jungle Age (off biome animals)"
    , start = humanTime "2018-11-19"
    , end = Nothing
    --  generation = {
    --  , allowOffBiomeMovingObjects = true
    --  , biomeMap = jungleBiomeMap
    --  }
    }
  , { name = "Jungle Age"
    , start = humanTime "2019-03-29T21:48:07.000Z"
    , end = Nothing
    --  generation = {
    --  , biomeMap = jungleBiomeMap
    --  }
    }
  , { name = "Jungle Age (screenshot 1)"
    , start = humanTime "2019-04-27T21:15:24.000Z"
    , end = Nothing
    --, biomeLayer = L.layerGroup([biomeImageLayer screenshotImageLayer]),
    --  generation = {
    --  , biomeMap = jungleBiomeMap
    --  }
    }
  , { name = "Jungle Age (small objects)"
    , start = humanTime "2019-05-04T17:11:31.000Z"
    , end = Nothing
    --  generation = {
    --  , tallHeight = 2 * CELL_D // heights that block
    --  , veryTallHeight = 3 * CELL_D
    --  , biomeMap = jungleBiomeMap
    --  }
    }
  , { name = "Jungle Age (screenshot 2)"
    , start = humanTime "2019-05-17T02:07:50.000Z"
    , end = Nothing
    --, biomeLayer = L.layerGroup([biomeImageLayer screenshotImageLayer]),
    --  generation = {
    --  , biomeMap = jungleBiomeMap
    --  }
    }
  , { name = "Random Age"
    , start = humanTime "2019-07-27T21:00:00Z"
    , end = Nothing
    --  generation = {
    --  , biomeMap = jungleBiomeMap
    --  }
    }
  , { name = "Topographic Age"
    , start = humanTime "2019-07-31T01:25:24Z"
    , end = Nothing
    --  generation = {
    --  , computeMapBiomeIndex = topographicMapBiomeIndex
    --  , biomeTotalWeight = topographicBiomeTotalWeight
    --  , biomeCumuWeights = topographicBiomeCumuWeights
    --  , biomeMap = topographicBiomeMap
    --  }
    }
  , { name = "Special Age"
    , start = humanTime "2019-08-01T02:08:47Z"
    , end = Nothing
    --  generation = {
    --  , computeMapBiomeIndex = topographicMapBiomeIndex
    --  , biomeTotalWeight = specialBiomeTotalWeight
    --  , biomeCumuWeights = specialBiomeCumuWeights
    --  , biomeMap = specialBiomeMap
    --  , numSpecialBiomes = 3
    --  }
    }
  ]
  |> List.foldr (\age (mtime, newages) ->
      ( Just age.start
      , { age
        | end = mtime
        , start = age.start
          |> Time.posixToMillis
          |> (+) 1
          |> Time.millisToPosix
        } :: newages
      )
    ) (Nothing, [])
  |> Tuple.second
