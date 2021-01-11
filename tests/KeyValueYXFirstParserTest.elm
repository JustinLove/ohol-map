module KeyValueYXFirstParserTest exposing (..)

import OHOLData as Data
import OHOLData.Parse as Parse exposing (Key(..), Object(..))

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
        |> Expect.equal (Ok (Object 1))
    , test "floors" <| \_ ->
      (Parser.run Parse.valueLine "vf1")
        |> Expect.equal (Ok (Floor 1))
    , test "uses" <| \_ ->
      (Parser.run Parse.valueLine "v1u0")
        |> Expect.equal (Ok (Use 1 0))
    , test "variant" <| \_ ->
      (Parser.run Parse.valueLine "v1v0")
        |> Expect.equal (Ok (Variant 1 0))
    , test "positive dy coord" <| \_ ->
      (Parser.run Parse.deltaY "9F1376")
        |> Expect.equal (Ok 9)
    , test "negative dy coord" <| \_ ->
      (Parser.run Parse.deltaY "-9F1376")
        |> Expect.equal (Ok -9)
    , test "negative dy coord - delivery truck" <| \_ ->
      (Parser.run Parse.deltaY "-401E")
        |> Expect.equal (Ok -401)
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
    , test "number series - one" <| \_ ->
      (Parser.run (Parse.numberSeries identity
        (\results n -> results) [] 0)
        "F1367")
        |> Expect.equal (Ok [-51367])
    , test "number series n" <| \_ ->
      (Parser.run (Parse.numberSeries identity
        (\results n -> n) [] 0)
        "F1367")
        |> Expect.equal (Ok -51367)
    , test "number series - two" <| \_ ->
      (Parser.run (Parse.numberSeries identity
        (\results n -> results) [] 0)
        "F1367b")
        |> Expect.equal (Ok [-51366, -51367])
    , test "number series uses base value" <| \_ ->
      (Parser.run (Parse.numberSeries identity
        (\results n -> results) [] 1)
        "F1367")
        |> Expect.equal (Ok [-51366])
    , test "coordinate line - one" <| \_ ->
      (Parser.run Parse.keyValueYXFirst "9F1367")
        |> Expect.equal (Ok [Key (Object 0) -51367 9])
    , test "coordinate line - two" <| \_ ->
      (Parser.run Parse.keyValueYXFirst "9F1367b")
        |> Expect.equal (Ok [Key (Object 0) -51367 9, Key (Object 0) -51366 9])
    , test "value and coordiate line" <| \_ ->
      (Parser.run Parse.keyValueYXFirst minimalKeyValueYXFirst)
        |> Expect.equal (Ok minimalKeyValueYXFirstResult)
    , test "mutiple coordinate lines" <| \_ ->
      (Parser.run Parse.keyValueYXFirst multiCoord)
        |> Expect.equal (Ok multiCoordResult)
    , test "mutiple value lines" <| \_ ->
      (Parser.run Parse.keyValueYXFirst multiValue)
        |> Expect.equal (Ok multiValueResult)
    , test "deliveryTruck" <| \_ ->
      (Parser.run Parse.keyValueYXFirst deliveryTruck)
        |> isOk
        |> Expect.true "delivery truck did not parse"
    ]

isOk : Result x a -> Bool
isOk result =
  case result of
    Ok _ -> True
    Err _ -> False

minimalKeyValueYXFirst = "v753\n9F1367"
minimalKeyValueYXFirstResult = [Parse.Key (Object 753) -51367 9]

multiCoord = """v1065
60F1323
207B158"""
multiCoordResult =
  [ Parse.Key (Object 1065) -51323 60
  , Parse.Key (Object 1065) -52481 267
  ]

multiValue = """v0
1B8bb1bc
v1385
-1b"""
multiValueResult =
  [ Parse.Key (Object 0) -18 1
  , Parse.Key (Object 0) -17 1
  , Parse.Key (Object 0) -6 1
  , Parse.Key (Object 0) -5 1
  , Parse.Key (Object 0) -3 1
  , Parse.Key (Object 1385) -2 0
  ]

deliveryTruck = """v4654
-401E6618
63H990
3a
24b858
203h96
55b145
1a
29f1
49F78
61f096
284J61
23563C8220"""
