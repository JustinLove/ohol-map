module OHOLData exposing
  ( Life
  , Server
  , Monument
  , Arc
  , Span
  , Objects
  , VersionChange
  , Spawn
  , SpawnChange(..)
  , Biome
  , Version
  , Age
  , World
  , Generation
  , SecondPlaceBiomeObjects(..)
  , completeVersions
  , oholCodeChanges
  , crucibleCodeChanges
  , tholCodeChanges
  , assignDataTime
  , rebuildWorlds
  , terminateMonuments
  , current
  , advance
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
  , age : Maybe Float
  , deathX : Maybe Int
  , deathY : Maybe Int
  , deathTime : Maybe Posix
  }

type alias Server =
  { id : Int
  , serverName : String
  , minTime: Posix
  , maxTime: Posix
  }

type alias Monument =
  { x : Int
  , y : Int
  , date : Posix
  , end : Maybe Posix
  }

type alias Arc =
  { serverId : Int
  , start: Posix
  , end: Maybe Posix
  , dataTime: Maybe Posix
  , seedA: Maybe Int
  , seedB: Maybe Int
  }

type alias Span =
  { start: Posix
  , end: Posix
  , base: Posix
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
      spawn.biomes
        |> List.foldl ensureBiome biomes
        |> Dict.map (changedSpawn spawn)

spawnsIn : Int -> Spawn -> Bool
spawnsIn bid spawn =
  spawn.gridPlacement == Nothing && spawn.randPlacement == Nothing && spawn.mapChance > 0 && List.member bid spawn.biomes

changedSpawn : Spawn -> Int -> Biome -> Biome
changedSpawn spawn bid biome =
  if spawnsIn bid spawn then
    {biome|objects = orderedUpdate .id spawn biome.objects}
  else
    {biome|objects = orderedRemove .id spawn.id biome.objects}

calculateChance : Biome -> Biome
calculateChance biome =
  { biome
  | totalChanceWeight =
    biome.objects
      |> List.map .mapChance
      |> List.foldl (+) 0
  }

ensureBiome : Int -> BiomeSet -> BiomeSet
ensureBiome bid biomes =
  Dict.update bid (\mbiome ->
    case mbiome of
      Just biome -> mbiome
      Nothing -> Just
        { id = bid
        , objects = []
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
    --_ = Debug.log "start" start
    name =
      [ mage |> Maybe.map .name
      , mver |> Maybe.map (.id >> String.fromInt)
      , marc |> Maybe.andThen .seedA |> Maybe.map String.fromInt
      ]
      |> List.filterMap identity
      |> String.join " "
    ageGen = mage
      |> Maybe.map .generation
      |> Maybe.withDefault defaultGeneration
    generation = 
      { ageGen
      | biomeRandSeedA = marc
        |> Maybe.map .seedA
        |> Maybe.withDefault ageGen.biomeRandSeedA
      , biomeRandSeedB = marc
        |> Maybe.map .seedB
        |> Maybe.withDefault ageGen.biomeRandSeedB
      , randSeed = case ageGen.randSeed of
        Just seed -> Just seed
        Nothing -> marc |> Maybe.andThen .seedA
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
    , end = marc |> Maybe.andThen .end
    , spans = []
    , biomeLayer = mage |> Maybe.andThen .biomeLayer
    , generation = generation
    }

rebuildWorlds : List Age -> List Version -> List Arc -> List Span -> List World
rebuildWorlds ages versions arcs spans =
  rebuildWorldsTimeList
    (timeList ages versions arcs)
    ages
    versions
    arcs
    []
    |> List.reverse
    |> fixupEndTime
    |> assignSpans spans
    --|> checkSpans spans

rebuildWorldsTimeList : List Int -> List Age -> List Version -> List Arc -> List World -> List World
rebuildWorldsTimeList times ages versions arcs worlds =
  case times of
    nextTime :: rest ->
      let
        --_ = Debug.log "time" nextTime
        nextAges = advance nextTime ages
        nextVersions = advance nextTime versions
        nextArcs = advance nextTime arcs
      in
      rebuildWorldsTimeList
        rest
        nextAges
        nextVersions
        nextArcs
        ((worldMerge (Time.millisToPosix nextTime)
          (current nextTime nextAges)
          (current nextTime nextVersions)
          (current nextTime nextArcs)
        ) :: worlds)
    [] ->
      worlds

assignSpans : List Span -> List World -> List World
assignSpans spans =
  List.map (assignWorldSpans spans)

assignWorldSpans : List Span -> World -> World
assignWorldSpans spans world =
  { world
  | spans = List.filter (\span -> (overlap world span) >= 0.5) spans
  }

assignDataTime : List Span -> List Arc -> List Arc
assignDataTime spans =
  List.map (assignArcDataTime spans)

assignArcDataTime : List Span -> Arc -> Arc
assignArcDataTime spans arc =
  { arc
  | dataTime = spans
    |> List.filter (\span -> (overlap arc span) >= 0.5)
    |> List.reverse
    |> List.head
    |> Maybe.map .end
  }

overlap : {r|start: Posix, end: Maybe Posix} -> Span -> Float
overlap world span =
  let
    spanStart = Time.posixToMillis span.start
    spanEnd = Time.posixToMillis span.end
    worldStart = Time.posixToMillis world.start
    worldEnd = Time.posixToMillis (world.end |> Maybe.withDefault span.end)
    end = min worldEnd spanEnd
    start = max worldStart spanStart
    length = max 0 (end - start)
    total = spanEnd - spanStart
    over = (toFloat length) / (toFloat total)
  in
   over

checkSpans : List Span -> List World -> List World
checkSpans spans worlds =
  let
    _ = spans |> List.length |> Debug.log "spans"
    _ = worlds |> List.concatMap .spans |> List.length |> Debug.log "used"
  in
    worlds

inRange : Int -> List {a|start:Posix} -> Bool
inRange time list =
  List.head list
    |> Maybe.map (.start >> Time.posixToMillis >> (\x -> time >= x))
    |> Maybe.withDefault False

current : Int -> List {a|start:Posix} -> Maybe {a|start:Posix}
current time list =
  if inRange time list then
    List.head list
  else
    Nothing

advance : Int -> List {a|start:Posix} -> List {a|start:Posix}
advance time list =
  let
    next = List.drop 1 list
  in
    if inRange time next then
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
    |> List.foldr compressValues []

compressValues : Int -> List Int -> List Int
compressValues value accum =
  case accum of
    last :: rest ->
      --if last - value < 10*60*1000 then
      if last - value < 1000 then
        accum
      else
        value :: accum
    [] ->
      [value]

terminateMonuments : List Arc -> List Monument -> List Monument
terminateMonuments arcs monuments =
  if List.isEmpty arcs then
    monuments
  else
    arcs
      |> List.foldl addEndTimeToMonuments (List.reverse monuments, [])
      |> Tuple.second

addEndTimeToMonuments : Arc -> (List Monument, List Monument) -> (List Monument, List Monument)
addEndTimeToMonuments arc (pending, processed) =
  case pending of
    monument :: rest ->
      let
        time = Time.posixToMillis monument.date
        start = Time.posixToMillis arc.start
        end = arc.end |> Maybe.map Time.posixToMillis |> Maybe.withDefault time
      in
        if time < start then
          addEndTimeToMonuments arc (rest, {monument|end = Just arc.start} :: processed)
        else if start < time && time <= end then
          addEndTimeToMonuments arc (rest, {monument|end = arc.end} :: processed)
        else
          (pending, processed)
    [] -> (pending, processed)

type alias Age =
  { name: String
  , start: Posix
  , biomeLayer: Maybe String
  , generation: Generation
  }

type alias World =
  { name: String
  , start: Posix
  , end: Maybe Posix
  , spans: List Span
  , biomeLayer: Maybe String
  , generation: Generation
  }

type SecondPlaceBiomeObjects
  = SecondPlaceObjects
  | SecondPlaceStaticObjects
  | NoMovingObjects
  | NoSecondPlace

type alias Generation =
  { secondPlaceBiomeObjects : SecondPlaceBiomeObjects
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
  , biomeRandSeedA: Maybe Int
  , biomeRandSeedB: Maybe Int
  , randSeed: Maybe Int
  , biomeOffset: Float
  , biomeScale: Float
  , biomeFractalRoughness: Float
  }

oholCodeChanges : List Age
oholCodeChanges =
  [ { name = "Badlands Age"
    , start = Time.millisToPosix 0
    , biomeLayer = Just "badlandsAge"
    , generation =
      { defaultGeneration
      | biomeMap = badlandsBiomeMap
      , secondPlaceBiomeObjects = SecondPlaceObjects
      }
    }
  , { name = "Arctic Age"
    , start = humanTime "2018-03-08"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = arcticBiomeMap
      , secondPlaceBiomeObjects = SecondPlaceObjects
      }
    }
  , { name = "Desert Age"
    , start = humanTime "2018-03-31"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = desertBiomeMap
      , secondPlaceBiomeObjects = SecondPlaceObjects
      }
    }
  , { name = "Jungle Age (off biome animals)"
    , start = humanTime "2018-11-19"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      , secondPlaceBiomeObjects = SecondPlaceObjects
      }
    }
  , { name = "Jungle Age"
    , start = humanTime "2019-03-29T21:48:07.000Z"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      }
    }
  , { name = "Jungle Age (screenshot 1)"
    , start = humanTime "2019-04-27T21:15:24.000Z"
    , biomeLayer = Just "screenshot" --L.layerGroup([biomeImageLayer screenshotImageLayer]),
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      }
    }
  , { name = "Jungle Age (small objects)"
    , start = humanTime "2019-05-04T17:11:31.000Z"
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
    , biomeLayer = Just "screenshot" --L.layerGroup([biomeImageLayer screenshotImageLayer]),
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      }
    }
  , { name = "Random Age"
    , start = humanTime "2019-07-27T21:00:00Z"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      , biomeRandSeedA = Nothing
      }
    }
  , { name = "Topographic Age"
    , start = humanTime "2019-07-31T01:25:24Z"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = topographicBiomeMap
      , biomeRandSeedA = Nothing
      } |> topographic topographicBiomeWeights
    }
  , { name = "Special Age"
    , start = humanTime "2019-08-01T02:08:47Z"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = specialBiomeMap
      , biomeRandSeedA = Nothing
      , numSpecialBiomes = 3
      } |> topographic specialBiomeWeights
    }
  , { name = "Special Age (seeded placements)"
    -- the short arc just before here had no tapped tarry spots or nosaj, so ambiguous
    , start = humanTime "2019-10-23T17:57:00Z"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = specialBiomeMap
      , biomeRandSeedA = Nothing
      , randSeed = Nothing
      , numSpecialBiomes = 3
      } |> topographic specialBiomeWeights
    }
  , { name = "Special Age (no animals)"
    , start = humanTime "2019-11-16T09:14:00Z"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = specialBiomeMap
      , biomeRandSeedA = Nothing
      , randSeed = Nothing
      , numSpecialBiomes = 3
      , secondPlaceBiomeObjects = NoMovingObjects
      } |> topographic specialBiomeWeights
    }
  , { name = "Special Age (specialists)"
    , start = humanTime "2019-11-18T18:41:00Z"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = specialBiomeMap
      , biomeRandSeedA = Nothing
      , randSeed = Nothing
      , numSpecialBiomes = 3
      , secondPlaceBiomeObjects = NoSecondPlace
      } |> topographic specialBiomeWeights
    }
  ]
  |> fixupStartTime

crucibleCodeChanges : List Age
crucibleCodeChanges =
  [ { name = "Cruicible"
    , start = humanTime "2019-06-02"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = jungleBiomeMap
      , tallHeight = 2 * cellD -- heights that block
      , veryTallHeight = 3 * cellD
      , biomeOffset = 8.3332
      , biomeScale = 0.8333
      , biomeFractalRoughness = 0.35
      }
    }
  ]
  |> fixupStartTime

tholCodeChanges : List Age
tholCodeChanges =
  [ { name = "Two Hours One Life (20264)"
    , start = humanTime "2019-08-31T13:00:00Z"
    , biomeLayer = Nothing
    , generation =
      { defaultGeneration
      | biomeMap = thol1BiomeMap
      , biomeRandSeedA = Just -1691939191
      , numSpecialBiomes = 3
      } |> topographic thol1BiomeWeights
    }
  , { name = "Two Hours One Life (20265)"
    , start = humanTime "2019-11-26T19:06:00Z"
    , biomeLayer = Just "tholMap"
    , generation =
      { defaultGeneration
      | biomeMap = thol2BiomeMap
      , biomeRandSeedA = Just -1691939191
      , numSpecialBiomes = 3
      } |> topographic thol2BiomeWeights
    }
  ]
  |> fixupStartTime

fixupEndTime : List World -> List World
fixupEndTime =
  List.foldr (\world (mtime, newages) ->
      ( Just world.start
      , { world
        | end = case mtime of
          Just _ -> mtime
          Nothing -> world.end
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
  { secondPlaceBiomeObjects = SecondPlaceStaticObjects
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
  , biomeRandSeedA = Just 723
  , biomeRandSeedB = Just 0
  , randSeed = Just 124567
  , biomeOffset = 0.83332
  , biomeScale = 0.08333
  , biomeFractalRoughness = 0.55
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
  , biomeTotalWeight = List.sum weights
  , biomeCumuWeights = weights
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

thol1BiomeMap =
  [ 1
  , 0
  , 2
  , 7
  , 3
  , 6
  , 5
  , 4
  ]

thol1BiomeWeights =
  [ 0.32
  , 0.12
  , 0.09
  , 0.21
  , 0.07
  , 0.07
  , 0.07
  , 0.08
  ]

thol2BiomeMap =
  [ 7
  , 0
  , 1
  , 2
  , 3
  , 6
  , 5
  , 4
  ]

thol2BiomeWeights =
  [ 0.18
  , 0.18
  , 0.13
  , 0.08
  , 0.09
  , 0.09
  , 0.11
  , 0.12
  ]
