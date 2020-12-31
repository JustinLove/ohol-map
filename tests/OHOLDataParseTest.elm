module OHOLDataParseTest exposing (..)

import OHOLData as Data
import OHOLData.Parse as Parse

import Dict exposing (Dict)
import Parser.Advanced as Parser
import Time exposing (Posix)

import Expect exposing (Expectation)
import Test exposing (..)

indexString = """1323 1
66 2473
-
764 3039
51 3150
"""

indexResult : Dict Int (Int, Bool)
indexResult = Dict.fromList
  [ (1323, (1, True))
  , (66, (2473, True))
  , (764, (3039, False))
  , (51, (3150, False))
  ]

suite : Test
suite =
  describe "index parser"
    [ test "id" <| \_ ->
      (Parser.run Parse.objectId "1")
        |> Expect.equal (Ok 1)
    , test "count" <| \_ ->
      (Parser.run Parse.objectCount "1")
        |> Expect.equal (Ok 1)
    , test "line" <| \_ ->
      (Parser.run (Parse.line False) "1 2")
        |> Expect.equal (Ok (Dict.singleton 1 (2, False)))
    , test "multiple lines" <| \_ ->
      (Parser.run Parse.objectSearchIndex "1 2\n3 4")
        |> Expect.equal (Ok (Dict.fromList
          [ (1, (2, True))
          , (3, (4, True))
          ]))
    , test "parse" <| \_ ->
      (Parser.run Parse.objectSearchIndex indexString)
        |> Expect.equal (Ok indexResult)
    ]
