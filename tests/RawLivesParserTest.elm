module RawLivesParserTest exposing (..)

import OHOLData as Data exposing (Life)
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
        (Parser.run Parse.rawLifeLine birthLine)
          |> Expect.equal (Ok birthLife)
      , test "death line" <| \_ ->
        (Parser.run Parse.rawLifeLine deathLine)
          |> Expect.equal (Ok deathLife)
      , test "multiple lines" <| \_ ->
        (Parser.run Parse.rawLives multipleLines)
          |> Expect.equal (Ok multipleLives)
      ]
    ]

isOk : Result x a -> Bool
isOk result =
  case result of
    Ok _ -> True
    Err _ -> False

birthLine = "B 1663977625 5512552 0b628e9b63c4e3689d67d5951796d5b54afa01c7 F (-165256,233) parent=5512473 pop=42 chain=98"
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
  , age = Nothing
  , gender = "F"
  , deathX = Nothing
  , deathY = Nothing
  , deathTime = Nothing
  , deathPopulation = Nothing
  , deathCause = Nothing
  }

deathLine = "D 1663977645 5512452 ee67bcfe81c26a7190ee1023c2c759219fc75b71 age=48.47 F (-165277,237) hunger pop=41"
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
  , age = Just 48.47
  , gender = "F"
  , deathX = Just -165277
  , deathY = Just 237
  , deathTime = Just (Time.millisToPosix 1663977645000)
  , deathPopulation = Just 41
  , deathCause = Just "hunger"
  }

multipleLines = birthLine ++ "\n" ++ deathLine
multipleLives = [ birthLife, deathLife ]
