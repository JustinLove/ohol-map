module OHOLDataTest exposing (..)

import OHOLData exposing (..)

import Time exposing (Posix)

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
  describe "world merge"
    [ describe "current"
      [ describe "empty list"
        [ test "x" <| \_ ->
          (current 0 [])
            |> Expect.equal Nothing
        ]
      , describe "singleton"
        [ test "0 before time" <| \_ ->
          (current 0 [time1])
            |> Expect.equal Nothing
        , test "1 at time" <| \_ ->
          (current 1 [time1])
            |> Expect.equal (Just time1)
        , test "2 past end of list" <| \_ ->
          (current 2 [time1])
            |> Expect.equal (Just time1)
        ]
      , describe "multiple items"
        [ test "0 before time" <| \_ ->
          (current 0 [time1, time10])
            |> Expect.equal Nothing
        , test "1 at time before another" <| \_ ->
          (current 1 [time1, time10])
            |> Expect.equal (Just time1)
        , test "2 between entries" <| \_ ->
          (current 2 [time1, time10])
            |> Expect.equal (Just time1)
        , test "10 at time last entry - should have been pre advanced" <| \_ ->
          (current 10 [time1, time10])
            |> Expect.equal (Just time1)
        , test "20 past end of list - should have been pre advanced" <| \_ ->
          (current 20 [time1, time10])
            |> Expect.equal (Just time1)
        ]
      ]
    , describe "advance"
      [ describe "empty list"
        [ test "x" <| \_ ->
          (advance 0 [])
            |> Expect.equal []
        ]
      , describe "singleton"
        [ test "0 before time" <| \_ ->
          (advance 0 [time1])
            |> Expect.equal [time1]
        , test "1 at time" <| \_ ->
          (advance 1 [time1])
            |> Expect.equal [time1]
        , test "2 past end of list" <| \_ ->
          (advance 2 [time1])
            |> Expect.equal [time1]
        ]
      , describe "multiple items"
        [ test "0 before time" <| \_ ->
          (advance 0 [time1, time10])
            |> Expect.equal [time1, time10]
        , test "1 at time before another" <| \_ ->
          (advance 1 [time1, time10])
            |> Expect.equal [time1, time10]
        , test "2 between entries" <| \_ ->
          (advance 2 [time1, time10])
            |> Expect.equal [time1, time10]
        , test "2 at time last entry" <| \_ ->
          (advance 10 [time1, time10])
            |> Expect.equal [time10]
        , test "20 past end of list" <| \_ ->
          (advance 20 [time1, time10])
            |> Expect.equal [time10]
        ]
      ]
    ]

time1 = {start = Time.millisToPosix 1}
time10 = {start = Time.millisToPosix 10}
