module OHOLData.Parse exposing
  ( Object(..)
  , Key(..)
  , Log(..)
  , objectPlacementCode
  , objectPlacementId
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
  , lives
  , lifeLine
  , name
  , deadEndsToString
  )

import OHOLData exposing (ObjectId, ObjectSearchIndex, Life)

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
type alias LifeParser a = Parser Context Problem a
type alias Context = String
type alias Problem = String

objectPlacementCode : Object -> String
objectPlacementCode obj =
  case obj of
    Object id -> String.fromInt id
    Use id count -> (String.fromInt id) ++ "u" ++ (String.fromInt count)
    Variant id n -> (String.fromInt id) ++ "v" ++ (String.fromInt n)
    Floor id -> "F" ++ (String.fromInt id)

objectPlacementId : Object -> ObjectId
objectPlacementId obj =
  case obj of
    Object id -> id
    Use id count -> id
    Variant id n -> id
    Floor id -> id

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

lives : LifeParser (List Life)
lives =
  loop [] livesStep

livesStep : List Life -> LifeParser (Step (List Life) (List Life))
livesStep reversedList =
  oneOf
    [ succeed (\l -> Loop (l :: reversedList))
      |= lifeLine
      |. oneOf
        [ newline
        , end "something other than newline after recorfd"
        ]
    , succeed ()
      |. end "unparsed trailing characters in lives"
      |> map (\_ -> Done (List.reverse reversedList))
    ]

lifeLine : LifeParser Life
lifeLine =
  succeed life
    |= serverId
    |. spacesOnly
    |= epoch
    |. spacesOnly
    |= playerId
    |. spacesOnly
    |= timeStamp
    |. spacesOnly
    |= coordinates
    |. spacesOnly
    |= playerCount
    |. spacesOnly
    |. gender
    |. spacesOnly
    |= lineageId
    |. spacesOnly
    |= chain
    |. spacesOnly
    |= optional timeStamp
    |. spacesOnly
    |= optionalCoordinates
    |. spacesOnly
    |= optional playerCount
    |. spacesOnly
    |= optional age
    |. spacesOnly
    |= optional deathReason
    |= oneOf
      [ succeed Just
        |. spacesOnly
        |= name
      , succeed Nothing
      ]

life
  : Int -- serverid
  -> Int -- epoch
  -> Int -- playerid
  -> Posix -- birthtime
  -> (Int, Int) -- birth location
  -> Int -- birth pop
  -> Int -- lineage
  -> Int -- chain
  -> Maybe Posix -- deathtime
  -> (Maybe Int, Maybe Int) -- death location
  -> Maybe Int -- death pop
  -> Maybe Float -- age
  -> Maybe String -- cause of death
  -> Maybe String -- name
  -> Life
life sid e pid bt (bx, by) bp lin ch dt (dx, dy) dp a dc n =
  { birthX = bx
  , birthY = by
  , birthTime = bt
  , birthPopulation = bp
  , chain = ch
  , lineage = lin
  , name = n
  , serverId = sid
  , epoch = e
  , playerid = pid
  , age = a
  , deathX = dx
  , deathY = dy
  , deathTime = dt
  , deathPopulation = dp
  , deathCause = dc
  }

serverId : LifeParser Int
serverId = positiveInt |> inContext "looking for a serverid"

epoch : LifeParser Int
epoch = positiveInt |> inContext "looking for epoch"

playerId : LifeParser Int
playerId = positiveInt |> inContext "looking for player id"

coordinates : LifeParser (Int, Int)
coordinates =
  succeed Tuple.pair
    |= coordinate
    |. spacesOnly
    |= coordinate
    |> inContext "looking for coordinates"

optionalCoordinates : LifeParser (Maybe Int, Maybe Int)
optionalCoordinates =
  succeed Tuple.pair
    |= optional coordinate
    |. spacesOnly
    |= optional coordinate
    |> inContext "looking for coordinates"

playerCount : LifeParser Int
playerCount = positiveInt |> inContext "looking for player count"

lineageId : LifeParser Int
lineageId =
  oneOf
    [ playerId
    , succeed 0 |. symbol (Token "X" "looking for no data marker")
    ]
    |> inContext "looking for lineage"

chain : LifeParser Int
chain = positiveInt |> inContext "looking for chain"

gender : LifeParser String
gender =
  oneOf
    [ succeed "M" |. symbol (Token "M" "looking for male")
    , succeed "F" |. symbol (Token "F" "looking for female")
    ]
    |> inContext "looking for gender"

optional : LifeParser a -> LifeParser (Maybe a)
optional parser =
  oneOf
    [ succeed Nothing |. noData
    , succeed Just
        |= parser
    ]

timeStamp : LifeParser Posix
timeStamp =
  succeed (\x -> Time.millisToPosix (x * 1000))
    |= positiveInt
    |> inContext "timestamp"

positiveInt : LifeParser Int
positiveInt =
  int "looking for positive integer" "invalid number"

coordinate : LifeParser Int
coordinate =
  negativeInt
    |> inContext "looking for a coordinate"

negativeInt : LifeParser Int
negativeInt =
  succeed Tuple.pair
    |= leadingNegative
    |= positiveInt
    |> andThen assembleNegativeInt
    |> inContext "looking for possibly negative integer"

leadingNegative : LifeParser Bool
leadingNegative =
  oneOf
    [ succeed True |. symbol (Token "-" "looking for negative sign")
    , succeed False
    ]

assembleNegativeInt : (Bool, Int) -> LifeParser Int
assembleNegativeInt (neg, n) =
  succeed (if neg then -n else n)

deathReason : LifeParser String
deathReason =
  oneOf
    [ succeed "hunger"
      |. token (Token "h" "looking for hunger")
    , succeed "disconnect"
      |. token (Token "d" "looking for disconnect")
    , succeed "oldAge"
      |. token (Token "o" "looking for oldAge")
    , succeed (\id -> "killer_"++id)
      |. token (Token "k" "looking for killer")
      |= (getChompedString <|
          chompWhile Char.isDigit)
    ]

age : LifeParser Float
age =
  float "looking for age" "invalid float"

name : LifeParser String
name =
  succeed identity
    |. symbol (Token "\"" "looking for start quote")
    |= quoteTerminatedString
    |. symbol (Token "\"" "looking for end quote")
    |> inContext "looking for name"

quoteTerminatedString : LifeParser String
quoteTerminatedString =
  getChompedString <|
    chompWhile (\c -> c /= quoteChar && c /= newlineChar)

spacesOnly : LifeParser ()
spacesOnly =
  succeed ()
    |. chompIf (\c -> c == ' ') "looking for one or more spaces"
    |. chompWhile (\c -> c == ' ')

newline : LifeParser ()
newline =
  symbol (Token "\n" "looking for newline")

---------------------

deadEndsToString : List (DeadEnd Context Problem) -> String
deadEndsToString deadEnds =
  deadEnds
    |> List.map deadEndToString
    |> String.join "\n"

deadEndToString : DeadEnd Context Problem -> String
deadEndToString r =
  ((positionToString r) ++ r.problem) :: (r.contextStack |> List.map contextToString)
    |> String.join " while: "

contextToString : {r|row : Int, col : Int, context : Context} -> String
contextToString r =
  (positionToString r) ++ r.context

positionToString : {r|row : Int, col : Int} -> String
positionToString {row, col} =
  (String.fromInt row) ++ "," ++ (String.fromInt col) ++ ": "

noData = symbol (Token "X" "looking for nodata marker")
newlineChar = '\n'
quoteChar = '"'
