module LivesParserTest exposing (..)

import OHOLData as Data exposing (Life)
import OHOLData.Parse as Parse

--import Dict exposing (Dict)
import Parser.Advanced as Parser
import Time exposing (Posix)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "compact life parser"
    [ describe "helpers"
      [ test "name parser" <| \_ ->
        (Parser.run Parse.name "\"FIRST LAST\"")
          |> Expect.equal (Ok "FIRST LAST")
      ]
    , describe "complete records"
      [ test "birth only line" <| \_ ->
        (Parser.run Parse.lifeLine birthOnlyLine)
          |> Expect.equal (Ok birthOnlyLife)
      , test "anon death line" <| \_ ->
        (Parser.run Parse.lifeLine anonDeathLine)
          |> Expect.equal (Ok anonDeathLife)
      , test "named death line" <| \_ ->
        (Parser.run Parse.lifeLine namedDeathLine)
          |> Expect.equal (Ok namedDeathLife)
      , test "named birth line" <| \_ ->
        (Parser.run Parse.lifeLine namedBirthLine)
          |> Expect.equal (Ok namedBirthLife)
      , test "unrelated line" <| \_ ->
        (Parser.run Parse.lifeLine unrelatedLine)
          |> Expect.equal (Ok unrelatedLife)
      , test "multiple lines" <| \_ ->
        (Parser.run Parse.lives multipleLines)
          |> Expect.equal (Ok multipleLives)
      ]
    ]

isOk : Result x a -> Bool
isOk result =
  case result of
    Ok _ -> True
    Err _ -> False


birthOnlyLine = "17 2 4469793 1632556779 -108966 -20282 31 F 4469793 1 X X X X X X"
birthOnlyLife : Life
birthOnlyLife =
  { birthX = -108966
  , birthY = -20282
  , birthTime = Time.millisToPosix 1632556779000
  , chain = 1
  , lineage = 4469793
  , name = Nothing
  , serverId = 17
  , epoch = 2
  , playerid = 4469793
  , age = Nothing
  , deathX = Nothing
  , deathY = Nothing
  , deathTime = Nothing
  }

namedBirthLine = "17 2 4469793 1632556779 -108966 -20282 31 F 4469793 1 X X X X X X \"FIRST LAST\""
namedBirthLife : Life
namedBirthLife =
  { birthX = -108966
  , birthY = -20282
  , birthTime = Time.millisToPosix 1632556779000
  , chain = 1
  , lineage = 4469793
  , name = Just "FIRST LAST"
  , serverId = 17
  , epoch = 2
  , playerid = 4469793
  , age = Nothing
  , deathX = Nothing
  , deathY = Nothing
  , deathTime = Nothing
  }

unrelatedLine = "17 2 4469793 1632556779 -108966 -20282 31 F X 1 X X X X X X"
unrelatedLife : Life
unrelatedLife =
  { birthX = -108966
  , birthY = -20282
  , birthTime = Time.millisToPosix 1632556779000
  , chain = 1
  , lineage = 0
  , name = Nothing
  , serverId = 17
  , epoch = 2
  , playerid = 4469793
  , age = Nothing
  , deathX = Nothing
  , deathY = Nothing
  , deathTime = Nothing
  }

anonDeathLine = "17 2 4469784 1632556178 -88968 -282 32 F 4469784 1 1632556771 -88687 -41 30 23.89 hunger"
anonDeathLife : Life
anonDeathLife =
  { birthX = -88968
  , birthY = -282
  , birthTime = Time.millisToPosix 1632556178000
  , chain = 1
  , lineage = 4469784
  , name = Nothing
  , serverId = 17
  , epoch = 2
  , playerid = 4469784
  , age = Just 23.89
  , deathX = Just -88687
  , deathY = Just -41
  , deathTime = Just (Time.millisToPosix 1632556771000)
  }

namedDeathLine = "17 2 4469784 1632556178 -88968 -282 32 F 4469784 1 1632556771 -88687 -41 30 23.89 hunger \"FIRST LAST\""
namedDeathLife : Life
namedDeathLife =
  { birthX = -88968
  , birthY = -282
  , birthTime = Time.millisToPosix 1632556178000
  , chain = 1
  , lineage = 4469784
  , name = Just "FIRST LAST"
  , serverId = 17
  , epoch = 2
  , playerid = 4469784
  , age = Just 23.89
  , deathX = Just -88687
  , deathY = Just -41
  , deathTime = Just (Time.millisToPosix 1632556771000)
  }

multipleLines = birthOnlyLine ++ "\n" ++ namedDeathLine
multipleLives = [ birthOnlyLife, namedDeathLife ]
