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
    , test "goto current" <| \_ ->
      (construct 0 [1, 2])
        |> next
        |> goto 1
        |> current
        |> Expect.equal 1
    , test "next matching match" <| \_ ->
      (construct 0 [1])
        |> nextMatching (always True)
        |> current
        |> Expect.equal 1
    , test "next matching no match found" <| \_ ->
      (construct 0 [1])
        |> nextMatching (always False)
        |> current
        |> Expect.equal 0
    , test "next matching several steps" <| \_ ->
      (construct 0 [1, 2])
        |> nextMatching (\x -> x == 2)
        |> current
        |> Expect.equal 2
    , test "next matching wraparound" <| \_ ->
      (construct 0 [1, 2])
        |> next
        |> nextMatching (\x -> x == 0)
        |> current
        |> Expect.equal 0
    , test "previous matching match" <| \_ ->
      (construct 0 [1])
        |> next
        |> previousMatching (always True)
        |> current
        |> Expect.equal 0
    , test "previous matching no match found" <| \_ ->
      (construct 0 [1])
        |> next
        |> previousMatching (always False)
        |> current
        |> Expect.equal 1
    , test "previous matching several steps" <| \_ ->
      (construct 0 [1, 2])
        |> next
        |> next
        |> previousMatching (\x -> x == 0)
        |> current
        |> Expect.equal 0
    , test "previous matching wraparound" <| \_ ->
      (construct 0 [1, 2])
        |> previousMatching (\x -> x == 2)
        |> current
        |> Expect.equal 2
    ]
