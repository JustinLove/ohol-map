module OHOLData.Parse exposing
  ( Object(..)
  , Key(..)
  , Log(..)
  , objectSearchIndex
  , line
  , keyValueYXFirst
  , logValueYXTFirst
  , valueLine
  , yLine
  , numberSeries
  , deltaY
  , encodedDeltaX
  , objectId
  , objectCount
  , deadEndsToString
  )

import OHOLData exposing (ObjectId, ObjectSearchIndex)

import Dict exposing (Dict)
import Parser.Advanced as Parser exposing (..)
import Time exposing (Posix)

type Object
  = Object ObjectId
  | Use ObjectId Int
  | Variant ObjectId Int
  | Floor ObjectId

type Key = Key Object Int Int
type Log = Log Object Int Int Posix

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
  justAnInt "Expecting Object Id"

objectCount : ObjectSearchParser ObjectId
objectCount =
  justAnInt "Expecting Instance Count"

type alias KeyValueYXFirstState =
  { reversedPlacements : List Key
  , id : Object
  , x : Int
  , y : Int
  }

type alias LogValueYXTFirstState =
  { reversedPlacements : List Log
  , id : Object
  , x : Int
  , y : Int
  , t : Int
  }

type alias NumberSeriesState a =
  { reversedResults : List a
  , n : Int
  }

keyValueYXFirst : KeyValueYXFirstParser (List Key)
keyValueYXFirst =
  loop (KeyValueYXFirstState [] (Object 0) 0 0) keyValueYXFirstStep

keyValueYXFirstStep
  : KeyValueYXFirstState
  -> ObjectSearchParser (Step KeyValueYXFirstState (List Key))
keyValueYXFirstStep ({reversedPlacements, id, x, y} as state) =
  let constructor cid cy cx = Key cid cx cy in
  oneOf
    [ succeed (\newid -> Loop (KeyValueYXFirstState reversedPlacements newid x y))
      |= valueLine
      |. spaces
    , succeed Loop
      |= (deltaY
        |> andThen (\dy ->
          numberSeries
            (constructor id (y + dy))
            (\updatedPlacements updatedX -> KeyValueYXFirstState updatedPlacements id updatedX (y + dy))
            reversedPlacements x
        ))
      |. spaces
    , succeed ()
      |. end "unparsed trailing characters in keyValueYXFirst"
      |> map (\_ -> Done (List.reverse reversedPlacements))
    ]

logValueYXTFirst : KeyValueYXFirstParser (List Log)
logValueYXTFirst =
  loop (LogValueYXTFirstState [] (Object 0) 0 0 0) logValueYXTFirstStep

logValueYXTFirstStep
  : LogValueYXTFirstState
  -> ObjectSearchParser (Step LogValueYXTFirstState (List Log))
logValueYXTFirstStep ({reversedPlacements, id, x, y, t} as state) =
  let constructor cid cy cx ct = Log cid cx cy (Time.millisToPosix (ct * 10)) in
  oneOf
    [ succeed (\newid -> Loop (LogValueYXTFirstState reversedPlacements newid x y t))
      |= valueLine
      |. spaces
    , succeed (\dy -> Loop (LogValueYXTFirstState reversedPlacements id x (y + dy) t))
      |= yLine
      |. spaces
    , succeed Loop
      |= (deltaX
        |> andThen (\dx ->
          numberSeries
            (constructor id y (x + dx))
            (\updatedPlacements updatedT -> LogValueYXTFirstState updatedPlacements id (x + dx) y updatedT)
            reversedPlacements t
        ))
      |. spaces
    , succeed ()
      |. end "unparsed trailing characters in logValueYXTFirst"
      |> map (\_ -> Done (List.reverse reversedPlacements))
    ]

valueLine : KeyValueYXFirstParser Object
valueLine =
  succeed identity
    |. symbol (Token "v" "Looking for value line")
    |= object

object : KeyValueYXFirstParser Object
object =
  oneOf
    [ succeed Floor
      |. symbol (Token "f" "Looking for floor")
      |= objectId
    , objectId
      |> andThen (\id ->
        oneOf
          [ succeed (Use id)
            |. symbol (Token "u" "Looking for uses")
            |= justAnInt "Expecting use"
          , succeed (Variant id)
            |. symbol (Token "v" "Looking for variant")
            |= justAnInt "Expecting variant"
          , succeed (Object id)
          ]
        )
    ]

yLine : KeyValueYXFirstParser Int
yLine =
  succeed identity
    |. symbol (Token "y" "Looking for value line")
    |= deltaY

numberSeries
  : (Int -> a)
  -> (List a -> Int -> b)
  -> List a
  -> Int
  -> KeyValueYXFirstParser b
numberSeries constructor finalize reversedResults n =
    loop (NumberSeriesState reversedResults n) (numberSeriesStep2 constructor finalize)

numberSeriesStep2
  : (Int -> a)
  -> (List a -> Int -> b)
  -> NumberSeriesState a
  -> KeyValueYXFirstParser (Step (NumberSeriesState a) b)
numberSeriesStep2 constructor finalize ({reversedResults, n} as state) =
  oneOf
    [ succeed (\dn ->
      Loop (NumberSeriesState (constructor (n + dn) :: reversedResults) (n + dn))
      )
      |= encodedInt
    , succeed ()
      |> map (\_ -> Done (finalize reversedResults n))
    ]

deltaY : KeyValueYXFirstParser Int
deltaY =
  inContext "Delta Y" <|
    signedInt

deltaX : KeyValueYXFirstParser Int
deltaX =
  inContext "Delta X" <|
    signedInt

signedInt : KeyValueYXFirstParser Int
signedInt =
  inContext "Signed Int" <|
    oneOf
      [ succeed negate
        |. symbol (Token "-" "Checking for negation")
        |= justAnInt "Expecting int (negative)"
      , justAnInt "Expecting int"
      ]

justAnInt : Problem -> KeyValueYXFirstParser Int
justAnInt prob =
  succeed ()
    |. chompIf Char.isDigit prob
    |. chompWhile Char.isDigit
    |> getChompedString
    |> andThen (String.toInt >> Maybe.map succeed >> Maybe.withDefault (problem "parsed as a integer but did not convert"))

encodedDeltaX : KeyValueYXFirstParser Int
encodedDeltaX =
  encodedInt
    |> inContext "Encoded Delta X"

encodedInt : KeyValueYXFirstParser Int
encodedInt =
  succeed Tuple.pair
    |= leadingLetter
    |= trailingDigits
    |> andThen assembleDecodedInt
    |> inContext "Encoded Int"

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
