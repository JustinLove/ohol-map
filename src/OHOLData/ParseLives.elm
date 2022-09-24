module OHOLData.ParseLives exposing
  ( dbLives
  , dbLifeLine
  , rawLives
  , rawLifeLine
  , quotedName
  )

import OHOLData exposing (Life)

import Parser.Advanced as Parser exposing (..)
import Time exposing (Posix)
import Char

type alias LifeParser a = Parser Context Problem a
type alias Context = String
type alias Problem = String

dbLives : LifeParser (List Life)
dbLives =
  loop [] dbLivesStep

dbLivesStep : List Life -> LifeParser (Step (List Life) (List Life))
dbLivesStep reversedList =
  oneOf
    [ succeed (\l -> Loop (l :: reversedList))
      |= dbLifeLine
      |. oneOf
        [ newline
        , end "something other than newline after record"
        ]
    , succeed ()
      |. end "unparsed trailing characters in lives"
      |> map (\_ -> Done (List.reverse reversedList))
    ]

dbLifeLine : LifeParser Life
dbLifeLine =
  succeed dbLife
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
    |= gender
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
    |= optional shortDeathReason
    |= oneOf
      [ succeed Just
        |. spacesOnly
        |= quotedName
      , succeed Nothing
      ]

dbLife
  : Int -- serverid
  -> Int -- epoch
  -> Int -- playerid
  -> Posix -- birthtime
  -> (Int, Int) -- birth location
  -> Int -- birth pop
  -> String -- gender
  -> Int -- lineage
  -> Int -- chain
  -> Maybe Posix -- deathtime
  -> (Maybe Int, Maybe Int) -- death location
  -> Maybe Int -- death pop
  -> Maybe Float -- age
  -> Maybe String -- cause of death
  -> Maybe String -- name
  -> Life
dbLife sid e pid bt (bx, by) bp g lin ch dt (dx, dy) dp a dc n =
  { birthX = bx
  , birthY = by
  , birthTime = bt
  , birthPopulation = bp
  , gender = g
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

rawLives : LifeParser (List Life)
rawLives =
  loop [] rawLivesStep

rawLivesStep : List Life -> LifeParser (Step (List Life) (List Life))
rawLivesStep reversedList =
  oneOf
    [ succeed (\l -> Loop (l :: reversedList))
      |= rawLifeLine
      |. oneOf
        [ newline
        , end "something other than newline after record"
        ]
    , succeed ()
      |. end "unparsed trailing characters in lives"
      |> map (\_ -> Done (List.reverse reversedList))
    ]

rawLifeLine : LifeParser Life
rawLifeLine =
  oneOf
    [ rawBirthLine
    , rawDeathLine
    ]

rawBirthLine : LifeParser Life
rawBirthLine =
  succeed rawBirth
    |. symbol (Token "B" "looking for birth")
    |. spacesOnly
    |= timeStamp
    |. spacesOnly
    |= playerId
    |. spacesOnly
    |= accountHash
    |. spacesOnly
    |= gender
    |. spacesOnly
    |= parenthesizedCoordinates
    |. spacesOnly
    |= parent
    |. spacesOnly
    |. tagName "pop"
    |= playerCount
    |. spacesOnly
    |. tagName "chain"
    |= chain

rawDeathLine : LifeParser Life
rawDeathLine =
  succeed rawDeath
    |. symbol (Token "D" "looking for death")
    |. spacesOnly
    |= timeStamp
    |. spacesOnly
    |= playerId
    |. spacesOnly
    |= accountHash
    |. spacesOnly
    |. tagName "age"
    |= age
    |. spacesOnly
    |= gender
    |. spacesOnly
    |= parenthesizedCoordinates
    |. spacesOnly
    |= deathReason
    |. spacesOnly
    |. tagName "pop"
    |= playerCount

rawBirth
  : Posix -- birthtime
  -> Int -- playerid
  -> String -- hash
  -> String -- gender
  -> (Int, Int) -- birth location
  -> Int -- lineage
  -> Int -- birth pop
  -> Int -- chain
  -> Life
rawBirth bt pid hash g (bx, by) lin bp ch =
  { birthX = bx
  , birthY = by
  , birthTime = bt
  , birthPopulation = bp
  , gender = g
  , chain = ch
  , lineage = lin
  , name = Nothing
  , serverId = 0
  , epoch = 0
  , playerid = pid
  , age = Nothing
  , deathX = Nothing
  , deathY = Nothing
  , deathTime = Nothing
  , deathPopulation = Nothing
  , deathCause = Nothing
  }

rawDeath
  : Posix -- deattime
  -> Int -- playerid
  -> String -- hash
  -> Float -- age
  -> String -- gender
  -> (Int, Int) -- death location
  -> String -- death cause
  -> Int -- death pop
  -> Life
rawDeath dt pid hash a g (dx, dy) dc dp =
  { birthX = dx
  , birthY = dy
  , birthTime = dt
  , birthPopulation = dp
  , gender = g
  , chain = 0
  , lineage = pid
  , name = Nothing
  , serverId = 0
  , epoch = 0
  , playerid = pid
  , age = Just a
  , deathX = Just dx
  , deathY = Just dy
  , deathTime = Just dt
  , deathPopulation = Just dp
  , deathCause = Just dc
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

parenthesizedCoordinates : LifeParser (Int, Int)
parenthesizedCoordinates =
  succeed Tuple.pair
    |. openParen
    |= coordinate
    |. comma
    |= coordinate
    |. closeParen
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

parent : LifeParser Int
parent =
  oneOf
    [ succeed identity
      |. tagName "parent"
      |= playerId
    , succeed 0 |. keyword (Token "noParent" "looking for no eve marker")
    ]
    |> inContext "looking for parent"

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


newline : LifeParser ()
newline =
  symbol (Token "\n" "looking for newline")

shortDeathReason : LifeParser String
shortDeathReason =
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

deathReason : LifeParser String
deathReason =
  getChompedString <|
    chompWhile (\c -> Char.isAlphaNum c || c == '_')

age : LifeParser Float
age =
  float "looking for age" "invalid float"

quotedName : LifeParser String
quotedName =
  succeed identity
    |. symbol (Token "\"" "looking for start quote")
    |= quoteTerminatedString
    |. symbol (Token "\"" "looking for end quote")
    |> inContext "looking for name"

quoteTerminatedString : LifeParser String
quoteTerminatedString =
  getChompedString <|
    chompWhile (\c -> c /= quoteChar && c /= newlineChar)

accountHash : LifeParser String
accountHash =
  getChompedString <|
    chompWhile Char.isHexDigit

spacesOnly : LifeParser ()
spacesOnly =
  succeed ()
    |. chompIf (\c -> c == ' ') "looking for one or more spaces"
    |. chompWhile (\c -> c == ' ')

tagName : String -> LifeParser ()
tagName name =
  keyword (Token name ("Looking for a tag " ++ name)) |. equals

openParen = symbol (Token "(" "Expecting open paren")
closeParen = symbol (Token ")" "Expecting close paren")
comma = symbol (Token "," "Expecting comma separator")
equals = symbol (Token "=" "Expecting = seperator")
noData = symbol (Token "X" "looking for nodata marker")
newlineChar = '\n'
quoteChar = '"'
