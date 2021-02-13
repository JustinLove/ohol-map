module Persist.Serialize exposing (lockedObjects)

import OHOLData exposing (ObjectId)

lockedObjects : List (ObjectId, Maybe String) -> String
lockedObjects locked =
  locked
    |> List.map lockedObject
    |> String.join "\n"

lockedObject : (ObjectId, Maybe String) -> String
lockedObject (id, mname) =
  case mname of
    Just name ->
      (String.fromInt id) ++ " " ++ name
    Nothing ->
      (String.fromInt id)
