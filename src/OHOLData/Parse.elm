module OHOLData.Parse exposing
  ( objectSearchIndex
  , line
  , objectId
  , objectCount
  , deadEndsToString
  )

import OHOLData exposing (ObjectId, ObjectSearchIndex)

import Dict exposing (Dict)
import Parser.Advanced as Parser exposing (..)
import Time exposing (Posix)

type alias ObjectSearchParser a = Parser Context Problem a
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
