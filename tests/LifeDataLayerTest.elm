module LifeDataLayerTest exposing (..)

import LifeDataLayer exposing (..)

import Time exposing (Posix)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "LifeDataLayer"
    [ describe "files to download"
      [ test "single file" <| \_ ->
        (lifelogsRequired publicLifeLogData 17 "servername" wednesdayStart wednesdayEnd )
          |> Expect.equal
            [ "publicLifeLogData/lifeLog_servername/2022_10October_05_Wednesday.txt"
            , "publicLifeLogData/lifeLog_servername/2022_10October_05_Wednesday_names.txt"
            ]
      , test "two files" <| \_ ->
        (lifelogsRequired publicLifeLogData 17 "servername" tuesdayEnd wednesdayEnd )
          |> Expect.equal
            [ "publicLifeLogData/lifeLog_servername/2022_10October_04_Tuesday.txt"
            , "publicLifeLogData/lifeLog_servername/2022_10October_04_Tuesday_names.txt"
            , "publicLifeLogData/lifeLog_servername/2022_10October_05_Wednesday.txt"
            , "publicLifeLogData/lifeLog_servername/2022_10October_05_Wednesday_names.txt"
            ]
      ]
    ]

publicLifeLogData = "publicLifeLogData/lifeLog_{server}/{filename}.txt"
wednesdayStart = Time.millisToPosix 1664928045000
wednesdayEnd = Time.millisToPosix 1664956789000
tuesdayEnd = Time.millisToPosix 1664927992000
