module KeyValueYXFirstParserTest exposing (..)

import OHOLData as Data
import OHOLData.Parse as Parse exposing (Placement(..))

import Dict exposing (Dict)
import Parser.Advanced as Parser
import Time exposing (Posix)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "key-value-y-x-first parser"
    [ test "value line" <| \_ ->
      (Parser.run Parse.valueLine "v1")
        |> Expect.equal (Ok 1)
    , test "positive dy coord" <| \_ ->
      (Parser.run Parse.deltaY "9F1376")
        |> Expect.equal (Ok 9)
    , test "negative dy coord" <| \_ ->
      (Parser.run Parse.deltaY "-9F1376")
        |> Expect.equal (Ok -9)
    , test "positive short dx coord" <| \_ ->
      (Parser.run Parse.encodedDeltaX "a")
        |> Expect.equal (Ok 0)
    , test "9 short dx coord" <| \_ ->
      (Parser.run Parse.encodedDeltaX "j")
        |> Expect.equal (Ok 9)
    , test "negative short dx coord" <| \_ ->
      (Parser.run Parse.encodedDeltaX "B")
        |> Expect.equal (Ok -1)
    , test "-9 short dx coord" <| \_ ->
      (Parser.run Parse.encodedDeltaX "J")
        |> Expect.equal (Ok -9)
    , test "positive long dx coord" <| \_ ->
      (Parser.run Parse.encodedDeltaX "b2")
        |> Expect.equal (Ok 12)
    , test "coordinate line - one" <| \_ ->
      (Parser.run (Parse.coordinateLine
        { reversedPlacements = []
        , id = 753
        , x = 0
        , y = 0
        })
        "9F1367")
        |> Result.map .reversedPlacements
        |> Expect.equal (Ok [Placement 753 -51367 9])
    , test "coordinate line update x" <| \_ ->
      (Parser.run (Parse.coordinateLine
        { reversedPlacements = []
        , id = 753
        , x = 0
        , y = 0
        })
        "9F1367")
        |> Result.map .x
        |> Expect.equal (Ok -51367)
    , test "coordinate line uses base coords" <| \_ ->
      (Parser.run (Parse.coordinateLine
        { reversedPlacements = []
        , id = 753
        , x = 1
        , y = 1
        })
        "9F1367")
        |> Result.map .reversedPlacements
        |> Expect.equal (Ok [Placement 753 -51366 10])
    , test "coordinate line - two" <| \_ ->
      (Parser.run (Parse.coordinateLine
        { reversedPlacements = []
        , id = 753
        , x = 0
        , y = 0
        })
        "9F1367b")
        |> Result.map .reversedPlacements
        |> Expect.equal (Ok [Placement 753 -51366 9, Placement 753 -51367 9])
    , test "value and coordiate line" <| \_ ->
      (Parser.run Parse.keyValueYXFirst minimalKeyValueYXFirst)
        |> Expect.equal (Ok minimalKeyValueYXFirstResult)
    , test "mutiple coordinate lines" <| \_ ->
      (Parser.run Parse.keyValueYXFirst multiCoord)
        |> Expect.equal (Ok multiCoordResult)
    , test "mutiple value lines" <| \_ ->
      (Parser.run Parse.keyValueYXFirst multiValue)
        |> Expect.equal (Ok multiValueResult)
    ]

minimalKeyValueYXFirst = "v753\n9F1367"
minimalKeyValueYXFirstResult = [Parse.Placement 753 -51367 9]

multiCoord = """v1065
60F1323
207B158"""
multiCoordResult =
  [ Parse.Placement 1065 -51323 60
  , Parse.Placement 1065 -52481 267
  ]

multiValue = """v0
1B8bb1bc
v1385
-1b"""
multiValueResult =
  [ Parse.Placement 0 -18 1
  , Parse.Placement 0 -17 1
  , Parse.Placement 0 -6 1
  , Parse.Placement 0 -5 1
  , Parse.Placement 0 -3 1
  , Parse.Placement 1385 -2 0
  ]
