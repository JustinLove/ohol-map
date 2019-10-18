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

type alias Biome = List Spawn

type alias Version =
  { id: Int
  , start: Posix
  , objects: Dict Int Spawn
  , biomes: Dict Int Biome
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
  let
    objects = List.foldl applySpawnChange previous.objects change.spawnChanges
    biomeSet = List.foldl
      (\{biomes} set -> Set.union set (Set.fromList biomes))
      Set.empty
      (Dict.values objects)
    biomeDict = Set.foldl
      (\biomeId accum ->
        Dict.insert
          biomeId
          (List.filter (\spawn -> List.member biomeId spawn.biomes) (Dict.values objects))
          accum
      )
      Dict.empty
      biomeSet
  in
    { id = change.id
    , start = change.start
    , objects = objects
    , biomes = biomeDict
    }

applySpawnChange : SpawnChange -> Dict Int Spawn -> Dict Int Spawn
applySpawnChange change objects =
  case change of
    Removed id -> Dict.remove id objects
    Changed spawn -> Dict.insert spawn.id spawn objects
