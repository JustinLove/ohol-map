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
  )

import Dict exposing (Dict)
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
  , mapchance: Float
  , biomes: List Int
  , moving: Bool
  , leftBlockingRadius: Int
  , rightBlockingRadius: Int
  , gridPlacement: Maybe Int
  , randPlacement: Maybe Int
  }

type SpawnChange
  = Removed Int
  | Changed Spawn

type alias Biome = OrderedList Spawn
type alias BiomeSet = Dict Int Biome

type alias Version =
  { id: Int
  , start: Posix
  , objects: Dict Int Spawn
  , biomes: BiomeSet
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
  , biomes = List.foldl applySpawnChangeToBiomes previous.biomes change.spawnChanges
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
        |> Dict.map (\bid biome -> orderedRemove .id id biome)
    Changed spawn ->
      spawn.biomes
        |> List.foldl (changeSpawn spawn) biomes

changeSpawn : Spawn -> Int -> BiomeSet -> BiomeSet
changeSpawn spawn bid biomes =
  Dict.update bid (\mbiome ->
    case mbiome of
      Just biome -> Just <| orderedUpdate .id spawn biome
      Nothing -> Just [spawn]
    ) biomes

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
