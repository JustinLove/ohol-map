module RawLivesParserTest exposing (..)

import OHOLData as Data exposing (Life, Parent(..))
import OHOLData.ParseLives as Parse

import Parser.Advanced as Parser
import Time exposing (Posix)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "raw life parser"
    [ describe "complete records"
      [ test "birth line" <| \_ ->
        (Parser.run Parse.rawBirthLine birthLine)
          |> Expect.equal (Ok birthBirth)
      , test "death line" <| \_ ->
        (Parser.run Parse.rawDeathLine deathLine)
          |> Expect.equal (Ok deathDeath)
      , test "lifelogs" <| \_ ->
        (Parser.run Parse.rawLifeLogs multipleLines)
          |> Expect.equal (Ok lifeLogs)
      , test "multiple lines" <| \_ ->
        (Parser.run Parse.rawLives multipleLines)
          |> Expect.equal (Ok multipleLives)
      ]
    , describe "birth death merge"
      [ test "life merge" <| \_ ->
        Parse.fullLife matchingBirth matchingDeath
          |> Expect.equal matchingLife
      , test "match death" <| \_ ->
        Parse.matchDeath [matchingBirth] matchingDeath
          |> Expect.equal ([], Just matchingLife)
      , test "merge step death only" <| \_ ->
        Parse.mergeStep (Parse.DeathLog deathDeath) ([], [])
          |> Expect.equal ([], [deathLife])
      , test "merge step birth only" <| \_ ->
        Parse.mergeStep (Parse.BirthLog matchingBirth) ([], [])
          |> Expect.equal ([matchingBirth], [])
      , test "merge step match only" <| \_ ->
        Parse.mergeStep (Parse.DeathLog matchingDeath) ([matchingBirth], [])
          |> Expect.equal ([], [matchingLife])
      , test "merge" <| \_ ->
        (Parse.mergeLifeEvents mergeLogs)
          |> Expect.equal mergeLives
      ]
    , describe "name logs"
      [ test "name line" <| \_ ->
        (Parser.run Parse.rawNameLine "1234 JOHN DOE")
          |> Expect.equal (Ok (Just (1234, "JOHN DOE")))
      , test "name log" <| \_ ->
        (Parser.run Parse.rawNameLogs "1234 JOHN DOE\r\n123 EVE DOE")
          |> Expect.equal (Ok [(1234, "JOHN DOE"), (123, "EVE DOE")])
      , test "name log with partial write" <| \_ ->
        (Parser.run Parse.rawNameLogs "1234 JOHN DOE\r\n12")
          |> Expect.equal (Ok [(1234, "JOHN DOE")])
      ]
    , describe "life name merge"
      [ test "name merge" <| \_ ->
        Parse.mergeNames [matchingLife] [(matchingLife.playerid, "JOHN DOE")]
          |> Expect.equal [{matchingLife | name = Just "JOHN DOE"}]
      ]
    ]

isOk : Result x a -> Bool
isOk result =
  case result of
    Ok _ -> True
    Err _ -> False

birthLine = "B 1663977625 5512552 0b628e9b63c4e3689d67d5951796d5b54afa01c7 F (-165256,233) parent=5512473 pop=42 chain=98"
birthBirth : Parse.Birth
birthBirth =
  { birthTime = Time.millisToPosix 1663977625000
  , playerid = 5512552
  , accountHash = "0b628e9b63c4e3689d67d5951796d5b54afa01c7"
  , gender = "F"
  , birthLocation = (-165256,233)
  , parent = ChildOf 5512473
  , birthPopulation = 42
  , chain = 98
  }

birthLife : Life
birthLife =
  { birthX = -165256
  , birthY = 233
  , birthTime = Time.millisToPosix 1663977625000
  , birthPopulation = 42
  , chain = 98
  , lineage = 5512473
  , name = Nothing
  , serverId = 0
  , epoch = 0
  , playerid = 5512552
  , accountHash = Just "0b628e9b63c4e3689d67d5951796d5b54afa01c7"
  , age = Nothing
  , gender = "F"
  , deathX = Nothing
  , deathY = Nothing
  , deathTime = Nothing
  , deathPopulation = Nothing
  , deathCause = Nothing
  }

deathLine = "D 1663977645 5512452 ee67bcfe81c26a7190ee1023c2c759219fc75b71 age=48.47 F (-165277,237) hunger pop=41"
deathDeath : Parse.Death
deathDeath =
  { deathTime = Time.millisToPosix 1663977645000
  , playerid = 5512452
  , accountHash = "ee67bcfe81c26a7190ee1023c2c759219fc75b71"
  , age = 48.47
  , gender = "F"
  , deathLocation = (-165277, 237)
  , deathCause = "hunger"
  , deathPopulation = 41
  }

deathLife : Life
deathLife =
  { birthX = -165277
  , birthY = 237
  , birthTime = Time.millisToPosix 1663977645000
  , birthPopulation = 41
  , chain = 0
  , lineage = 5512452
  , name = Nothing
  , serverId = 0
  , epoch = 0
  , playerid = 5512452
  , accountHash = Just "ee67bcfe81c26a7190ee1023c2c759219fc75b71"
  , age = Just 48.47
  , gender = "F"
  , deathX = Just -165277
  , deathY = Just 237
  , deathTime = Just (Time.millisToPosix 1663977645000)
  , deathPopulation = Just 41
  , deathCause = Just "hunger"
  }


matchingBirthLine = "B 1663891224 5510708 f2669e3148ba99cc5c5d46948b9a2817120426ed F (-166014,-320) parent=5510702 pop=34 chain=2"
matchingBirth : Parse.Birth
matchingBirth =
  { birthTime = Time.millisToPosix 1663891224000
  , playerid = 5510708
  , accountHash = "f2669e3148ba99cc5c5d46948b9a2817120426ed"
  , gender = "F"
  , birthLocation = (-166014,-320)
  , parent = ChildOf 5510702
  , birthPopulation = 34
  , chain = 2
  }

matchingDeathLine = "D 1663894824 5510708 f2669e3148ba99cc5c5d46948b9a2817120426ed age=60.00 F (-165959,-319) oldAge pop=43"
matchingDeath : Parse.Death
matchingDeath =
  { deathTime = Time.millisToPosix 1663894824000
  , playerid = 5510708
  , accountHash = "f2669e3148ba99cc5c5d46948b9a2817120426ed"
  , age = 60.00
  , gender = "F"
  , deathLocation = (-165959,-319)
  , deathCause = "oldAge"
  , deathPopulation = 43
  }

matchingLife : Life
matchingLife =
  { birthX = -166014
  , birthY = -320
  , birthTime = Time.millisToPosix 1663891224000
  , birthPopulation = 34
  , chain = 2
  , lineage = 5510702
  , name = Nothing
  , serverId = 0
  , epoch = 0
  , playerid = 5510708
  , accountHash = Just "f2669e3148ba99cc5c5d46948b9a2817120426ed"
  , age = Just 60.00
  , gender = "F"
  , deathX = Just -165959
  , deathY = Just -319
  , deathTime = Just (Time.millisToPosix 1663894824000)
  , deathPopulation = Just 43
  , deathCause = Just "oldAge"
  }

multipleLines = birthLine ++ "\r\n" ++ deathLine
lifeLogs = [ Parse.BirthLog birthBirth, Parse.DeathLog deathDeath ]
multipleLives = [ deathLife, birthLife ]
mergeLogs =
  [ Parse.DeathLog deathDeath
  , Parse.BirthLog matchingBirth
  , Parse.DeathLog matchingDeath
  , Parse.BirthLog birthBirth
  ]
mergeLives =
  [ deathLife
  , matchingLife
  , birthLife
  ]
