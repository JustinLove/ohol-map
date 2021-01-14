module ZipperTest exposing (..)

import Zipper exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "Zipper"
    [ test "fromList" <| \_ ->
      (construct 0 [])
        |> toList
        |> Expect.equal [0]
    , test "current" <| \_ ->
      (construct 0 [])
        |> current
        |> Expect.equal 0
    , test "next" <| \_ ->
      (construct 0 [1])
        |> next
        |> current
        |> Expect.equal 1
    , test "next off end" <| \_ ->
      (construct 0 [])
        |> next
        |> current
        |> Expect.equal 0
    , test "next order" <| \_ ->
      (construct 0 [1, 2])
        |> next
        |> next
        |> next
        |> toList
        |> Expect.equal [0, 1, 2]
    , test "previous" <| \_ ->
      (construct 0 [1])
        |> next
        |> previous
        |> current
        |> Expect.equal 0
    , test "previous off end" <| \_ ->
      (construct 0 [])
        |> previous
        |> current
        |> Expect.equal 0
    , test "previous order" <| \_ ->
      (construct 0 [1, 2])
        |> previous
        |> toList
        |> Expect.equal [0, 1, 2]
    , test "mapToList trivial" <| \_ ->
      (construct 0 [])
        |> mapToList identity
        |> Expect.equal [0]
    , test "mapToList ordering" <| \_ ->
      (construct 0 [1, 2, 3, 4])
        |> next
        |> next
        |> mapToList identity
        |> Expect.equal [0, 1, 2, 3, 4]
    , test "goto" <| \_ ->
      (construct 0 [])
        |> goto 0
        |> current
        |> Expect.equal 0
    , test "goto after" <| \_ ->
      (construct 0 [1])
        |> goto 1
        |> current
        |> Expect.equal 1
    , test "goto before" <| \_ ->
      (construct 0 [1])
        |> next
        |> goto 0
        |> current
        |> Expect.equal 0
    ]
