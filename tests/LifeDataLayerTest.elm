module LifeDataLayerTest exposing (..)

import LifeDataLayer exposing (..)

import Calendar exposing (Date)
import Time exposing (Posix)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "LifeDataLayer"
    [ describe "files to download"
      [ test "single file" <| \_ ->
        (lifelogsRequired wednesdayStart wednesdayEnd )
          |> Expect.equal
            [ date 2022 Time.Oct 5
            ]
      , test "two files" <| \_ ->
        (lifelogsRequired tuesdayEnd wednesdayEnd )
          |> Expect.equal
            [ date 2022 Time.Oct 4
            , date 2022 Time.Oct 5
            ]
      ]
    ]

date : Int -> Time.Month -> Int -> Date
date year month day =
  Calendar.fromRawParts { year = year, month = month, day = day }
    |> Maybe.withDefault (0 |> Time.millisToPosix |> Calendar.fromPosix)

publicLifeLogData = "publicLifeLogData/lifeLog_{server}/{filename}.txt"
wednesdayStart = Time.millisToPosix 1664928045000
wednesdayEnd = Time.millisToPosix 1664956789000
tuesdayEnd = Time.millisToPosix 1664927992000
