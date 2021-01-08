module LogValueYXTFirstParserTest exposing (..)

import OHOLData as Data
import OHOLData.Parse as Parse exposing (Log(..), Object(..))

import Dict exposing (Dict)
import Parser.Advanced as Parser
import Time exposing (Posix)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "key-value-y-x-t-first parser"
    [ test "y line" <| \_ ->
      (Parser.run Parse.yLine "y1")
        |> Expect.equal (Ok 1)
    , test "minimal log" <| \_ ->
      (Parser.run Parse.logValueYXTFirst minimalLogValueYXTFirst)
        |> Expect.equal (Ok minimalLogValueYXTFirstResult)
    , test "multiple log" <| \_ ->
      (Parser.run Parse.logValueYXTFirst multipleLogValueYXTFirst)
        |> Expect.equal (Ok multipleLogValueYXTFirstResult)
    ]

t = Time.millisToPosix

minimalLogValueYXTFirst = """v153
y84
-52429b60825540792"""
minimalLogValueYXTFirstResult =
  [ Parse.Log (Object 153) -52429 84 (t 1608255407920)
  ]

multipleLogValueYXTFirst = """v520
y45
-51331b60826375750
y60
-1579C826215e46"""
multipleLogValueYXTFirstResult =
  [ Parse.Log (Object 520) -51331 45 (t 1608263757500)
  , Parse.Log (Object 520) -52910 105 (t 1608235495350)
  , Parse.Log (Object 520) -52910 105 (t 1608235499810)
  ]
