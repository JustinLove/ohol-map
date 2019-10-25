module OHOLData exposing
  ( Life
  , Server
  , Arc
  , Objects
  , VersionChange
  , Spawn
  , SpawnChange(..)
  , Version
  , World
  , completeVersions
  , codeChanges
  , rebuildWorlds
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

worldMerge : Posix -> Maybe Age -> Maybe Version -> Maybe Arc -> World
worldMerge start mage mver marc =
  let
    name =
      [ mage |> Maybe.map .name
      , mver |> Maybe.map (.id >> String.fromInt)
      , marc |> Maybe.map (.seed >> String.fromInt)
      ]
      |> List.filterMap identity
      |> String.join " "
    ageGen = mage
      |> Maybe.map .generation
      |> Maybe.withDefault defaultGeneration
    generation = 
      { ageGen
      | biomeSeedOffset = marc
        |> Maybe.map .seed
        |> Maybe.withDefault defaultGeneration.biomeSeedOffset
      , objects = mver
        |> Maybe.map .objects
        |> Maybe.withDefault emptyVersion.objects
      , biomes = mver
        |> Maybe.map .biomes
        |> Maybe.withDefault emptyVersion.biomes
      , gridPlacements = mver
        |> Maybe.map .gridPlacements
        |> Maybe.withDefault emptyVersion.gridPlacements
      , randPlacements = mver
        |> Maybe.map .randPlacements
        |> Maybe.withDefault emptyVersion.randPlacements
      }
  in
    { name = name
    , start = start
    , end = marc |> Maybe.map .end
    , dataTime = marc |> Maybe.map .end
    , biomeLayer = mage |> Maybe.andThen .biomeLayer
    , generation = generation
    }

rebuildWorlds : List Age -> List Version -> List Arc -> List World
rebuildWorlds ages versions arcs =
  rebuildWorldsTimeList
    (timeList ages versions arcs)
    ages
    versions
    arcs
    []
    |> List.reverse
    |> fixupEndTime

rebuildWorldsTimeList : List Int -> List Age -> List Version -> List Arc -> List World -> List World
rebuildWorldsTimeList times ages versions arcs worlds =
  case times of
    nextTime :: rest ->
      let _ = Debug.log "time" nextTime in
      rebuildWorldsTimeList
        rest
        (advance nextTime ages)
        (advance nextTime versions)
        (advance nextTime arcs)
        ((worldMerge (Time.millisToPosix nextTime)
          (current nextTime ages)
          (current nextTime versions)
          (current nextTime arcs)
        ) :: worlds)
    [] ->
      worlds

current : Int -> List {a|start:Posix} -> Maybe {a|start:Posix}
current time list =
  let
    inRange = List.head list
      |> Maybe.map (.start >> Time.posixToMillis >> (\x -> time >= x))
      |> Maybe.withDefault False
  in
    if inRange then
      List.head list
    else
      Nothing

advance : Int -> List {a|start:Posix} -> List {a|start:Posix}
advance time list =
  let
    next = List.drop 1 list
  in
    if (current time next) /= Nothing then
      advance time next
    else
      list

timeList : List Age -> List Version -> List Arc -> List Int
timeList ages versions arcs =
  [ List.map .start ages
  , List.map .start versions
  , List.map .start arcs
  ]
    |> List.concat
    |> List.map Time.posixToMillis
    |> List.sort
    |> Debug.log "sorted"
    |> List.foldr compressValues []
    |> Debug.log "compressed"

compressValues : Int -> List Int -> List Int
compressValues value accum =
  case accum of
    last :: rest ->
      if last - value < 10*60*1000 then
        accum
      else
        value :: accum
    [] ->
      [value]

type alias Age = World
type alias World =
  { name: String
  , start: Posix
  , end: Maybe Posix
  , dataTime: Maybe Posix
  , biomeLayer: Maybe String
  , generation: Generation
  }

type alias Generation =
  { allowOffBiomeMovingObjects: Bool
  , biomeMap: List Int
  , tallHeight: Int
  , veryTallHeight: Int
  , computeMapBiomeIndex: String
  , biomeTotalWeight: Float
  , biomeCumuWeights: List Float
  , numSpecialBiomes: Int
  , objects: Dict Int Spawn
  , biomes: BiomeSet
  , gridPlacements: List Spawn
  , randPlacements: List Spawn
  , biomeSeedOffset: Int
  }

codeChanges : List Age
codeChanges =
  [ { name = "Badlands Age"
    , start = Time.millisToPosix 0
    , end = Nothing
    , dataTime = Nothing
    , biomeLayer = Just "badlandsAge"
    , generation =
      { defaultGeneration
      | biomeMap = badlandsBiomeMap
      , allowOffBiomeMovingObjects = True
      }
    }
  , { name = "Arctic Age"
    , start = humanTime "2018-03-08"
    , end = Nothing
    , dataTime = Nothing
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = arcticBiomeMap
      , allowOffBiomeMovingObjects = True
      }
    }
  , { name = "Desert Age"
    , start = humanTime "2018-03-31"
    , end = Nothing
    , dataTime = Nothing
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = desertBiomeMap
      , allowOffBiomeMovingObjects = True
      }
    }
  , { name = "Jungle Age (off biome animals)"
    , start = humanTime "2018-11-19"
    , end = Nothing
    , dataTime = Nothing
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      , allowOffBiomeMovingObjects = True
      }
    }
  , { name = "Jungle Age"
    , start = humanTime "2019-03-29T21:48:07.000Z"
    , end = Nothing
    , dataTime = Nothing
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      }
    }
  , { name = "Jungle Age (screenshot 1)"
    , start = humanTime "2019-04-27T21:15:24.000Z"
    , end = Nothing
    , dataTime = Nothing
    , biomeLayer = Just "screenshot" --L.layerGroup([biomeImageLayer screenshotImageLayer]),
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      }
    }
  , { name = "Jungle Age (small objects)"
    , start = humanTime "2019-05-04T17:11:31.000Z"
    , end = Nothing
    , dataTime = Nothing
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      , tallHeight = 2 * cellD -- heights that block
      , veryTallHeight = 3 * cellD
      }
    }
  , { name = "Jungle Age (screenshot 2)"
    , start = humanTime "2019-05-17T02:07:50.000Z"
    , end = Nothing
    , dataTime = Nothing
    , biomeLayer = Just "screenshot" --L.layerGroup([biomeImageLayer screenshotImageLayer]),
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      }
    }
  , { name = "Random Age"
    , start = humanTime "2019-07-27T21:00:00Z"
    , end = Nothing
    , dataTime = Nothing
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      }
    }
  , { name = "Topographic Age"
    , start = humanTime "2019-07-31T01:25:24Z"
    , end = Nothing
    , dataTime = Nothing
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = topographicBiomeMap
      } |> topographic topographicBiomeWeights
    }
  , { name = "Special Age"
    , start = humanTime "2019-08-01T02:08:47Z"
    , end = Nothing
    , dataTime = Nothing
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = specialBiomeMap
      , numSpecialBiomes = 3
      } |> topographic specialBiomeWeights
    }
  ]
  |> fixupEndTime
  |> fixupStartTime

fixupEndTime : List World -> List World
fixupEndTime =
  List.foldr (\world (mtime, newages) ->
      ( Just world.start
      , { world
        | end = mtime
        } :: newages
      )
    ) (Nothing, [])
  >> Tuple.second

fixupStartTime : List Age -> List Age
fixupStartTime =
  List.map (\age ->
      { age
      | start = age.start
        |> Time.posixToMillis
        |> (+) 1
        |> Time.millisToPosix
      }
    )

humanTime : String -> Posix
humanTime s =
  Iso8601.toTime s
    |> Result.mapError (Debug.log ("time error " ++ s))
    |> Result.withDefault (Time.millisToPosix 0)

cellD = 128

defaultGeneration : Generation
defaultGeneration =
  { allowOffBiomeMovingObjects = False
  , biomeMap = jungleBiomeMap
  , tallHeight = 2
  , veryTallHeight = 3
  , computeMapBiomeIndex = "competeMapBiomeIndex"
  , biomeTotalWeight = 0
  , biomeCumuWeights = []
  , numSpecialBiomes = 0
  , objects = Dict.empty
  , biomes = Dict.empty
  , gridPlacements = []
  , randPlacements = []
  , biomeSeedOffset = 723
  }

jungleBiomeMap =
  [ 0
  , 3
  , 4
  , 5
  , 2
  , 1
  , 6
  ]
desertBiomeMap =
  [ 0
  , 3
  , 4
  , 5
  , 2
  , 1
  ]
arcticBiomeMap =
  [ 0
  , 3
  , 4
  , 2
  , 1
  ]
badlandsBiomeMap =
  [ 0
  , 3
  , 2
  , 1
  ]

topographicBiomeMap =
  [ 1
  , 0
  , 2
  , 6
  , 5
  , 3
  , 4
  ]

topographicBiomeWeights =
  [ 0.32
  , 0.11
  , 0.08
  , 0.05
  , 0.05
  , 0.13
  , 0.25
  ]

topographic : List Float -> Generation -> Generation
topographic weights gen =
  { gen
  | computeMapBiomeIndex = "topographicMapBiomeIndex"
  , biomeTotalWeight = List.sum topographicBiomeWeights
  , biomeCumuWeights = topographicBiomeWeights
    |> List.foldl (\w (accum, result) ->
      ( accum + w
      , (accum + w) :: result
      )
    ) (0, [])
    |> Tuple.second
    |> List.reverse
  }

specialBiomeMap =
  [ 1
  , 0
  , 2
  , 3
  , 6
  , 5
  , 4
  ]

specialBiomeWeights =
  [ 0.32
  , 0.12
  , 0.09
  , 0.11
  , 0.11
  , 0.11
  , 0.13
  ]
