module OHOLData.Parse exposing
  ( Placement(..)
  , objectSearchIndex
  , line
  , objectId
  , objectCount
  , deadEndsToString
  , keyValueYXFirst
  , valueLine
  , coordinateLine
  , deltaY
  , encodedDeltaX
  )

import OHOLData exposing (ObjectId, ObjectSearchIndex)

import Dict exposing (Dict)
import Parser.Advanced as Parser exposing (..)
import Time exposing (Posix)

type Placement = Placement ObjectId Int Int

type alias ObjectSearchParser a = Parser Context Problem a
type alias KeyValueYXFirstParser a = Parser Context Problem a
type alias Context = String
type alias Problem = String

objectSearchIndex : ObjectSearchParser ObjectSearchIndex
objectSearchIndex = 
  lines

lines : ObjectSearchParser ObjectSearchIndex
lines = 
  loop (Dict.empty, True) linesStep

linesStep : (ObjectSearchIndex, Bool) -> ObjectSearchParser (Step (ObjectSearchIndex, Bool) ObjectSearchIndex)
linesStep (index, included) =
  oneOf
    [ succeed (\l -> Loop (Dict.union l index, included))
      |= line included
      |. spaces
    , succeed (Loop (index, False))
      |. symbol (Token "-" "Looking for a dash separator")
      |. spaces
    , succeed ()
      |. end "unparsed trailing characters in index"
      |> map (\_ -> Done index)
    ]

line : Bool -> ObjectSearchParser ObjectSearchIndex
line included = 
  succeed (entry included)
    |= objectId
    |. spaces
    |= objectCount

entry : Bool -> ObjectId -> Int -> ObjectSearchIndex
entry included id count = Dict.singleton id (count, included)

objectId : ObjectSearchParser ObjectId
objectId = 
  int "Expecting Object Id" "Invalid Object Id"

objectCount : ObjectSearchParser ObjectId
objectCount = 
  int "Expecting Instance Count" "Invalid Instance Count"

type alias KeyValueYXFirstState =
  { reversedPlacements : List Placement
  , id : ObjectId
  , x : Int
  , y : Int
  }

keyValueYXFirst : KeyValueYXFirstParser (List Placement)
keyValueYXFirst = 
  loop (KeyValueYXFirstState [] 0 0 0) keyValueYXFirstStep

keyValueYXFirstStep
  : KeyValueYXFirstState
  -> ObjectSearchParser (Step KeyValueYXFirstState (List Placement))
keyValueYXFirstStep ({reversedPlacements, id, x, y} as state) = 
  oneOf
    [ succeed (\newid -> Loop (KeyValueYXFirstState reversedPlacements newid x y))
      |= valueLine
      |. spaces
    , succeed Loop
      |= coordinateLine state
      |. spaces
    , succeed ()
      |. end "unparsed trailing characters in index"
      |> map (\_ -> Done (List.reverse reversedPlacements))
    ]

valueLine : KeyValueYXFirstParser ObjectId
valueLine =
  succeed identity
    |. symbol (Token "v" "Looking for value line")
    |= objectId

coordinateLine : KeyValueYXFirstState -> KeyValueYXFirstParser KeyValueYXFirstState
coordinateLine {reversedPlacements, id, x, y} = 
  deltaY
    |> andThen (\dy -> 
      loop (KeyValueYXFirstState reversedPlacements id x (y + dy)) coordinateLineStep
    )

coordinateLineStep
  : KeyValueYXFirstState
  -> KeyValueYXFirstParser (Step KeyValueYXFirstState KeyValueYXFirstState)
coordinateLineStep ({reversedPlacements, id, x, y} as state) =
  oneOf
    [ succeed (\dx -> Loop (KeyValueYXFirstState ((Placement id (x + dx) y) :: reversedPlacements) id (x + dx) y))
      |= encodedDeltaX
    , succeed ()
      |> map (\_ -> Done state)
    ]

deltaY : KeyValueYXFirstParser Int
deltaY = 
  inContext "Delta X" <|
    oneOf
      [ succeed negate
        |. symbol (Token "-" "Checking for negation")
        |= int "Expecting int (negative)" "Invalid int (negative)"
      , int "Expecting int" "Invalid int"
      ]

encodedDeltaX : KeyValueYXFirstParser Int
encodedDeltaX = 
  succeed Tuple.pair
    |= leadingLetter
    |= trailingDigits
    |> andThen assembleDecodedInt
    |> inContext "Encoded Delta X"

assembleDecodedInt : ((Bool, Char), String) -> KeyValueYXFirstParser Int
assembleDecodedInt ((neg, head), tail) =
  String.toInt (String.cons head tail)
    |> Maybe.map (\n -> succeed <| if neg then -n else n)
    |> Maybe.withDefault (problem "did not decode as integer")

leadingLetter : KeyValueYXFirstParser (Bool, Char)
leadingLetter = 
  chompIf Char.isAlpha "Looking for leading letter"
    |> getChompedString
    |> andThen decodeLeader

decodeLeader : String -> KeyValueYXFirstParser (Bool, Char)
decodeLeader string =
  string
    |> String.uncons
    |> Maybe.map Tuple.first
    |> Maybe.map extractLeader
    |> Maybe.withDefault (problem "an encoded value chomped a character and was nto at least one character long")

extractLeader : Char -> KeyValueYXFirstParser (Bool, Char)
extractLeader char =
  let code = Char.toCode char in
  if (65 <= code && code <= 74) then
    succeed (True, Char.fromCode (code - (65-48)))
  else if (97 <= code && code <= 106) then
    succeed (False, Char.fromCode (code - (97-48)))
  else
    problem "invalid leader character"

validLeadingLetter : Char -> Bool
validLeadingLetter char =
  let code = Char.toCode char in
    (65 <= code && code <= 73) || (97 <= code && code <= 105)

trailingDigits : KeyValueYXFirstParser String
trailingDigits = 
  getChompedString <|
    chompWhile Char.isDigit

---------------------

deadEndsToString : List (DeadEnd Context Problem) -> String
deadEndsToString deadEnds =
  deadEnds
    |> List.map deadEndToString
    |> String.join "\n"

deadEndToString : DeadEnd Context Problem -> String
deadEndToString {problem, contextStack} =
  problem :: (contextStack |> List.map contextToString)
    |> String.join " while: "

contextToString : {r|context : Context} -> String
contextToString {context} =
  context
