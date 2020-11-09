module Theme exposing (Theme(..), toString, fromString)

type Theme
  = Light
  | Dark

toString : Theme -> String
toString theme =
  case theme of
    Light -> "light"
    Dark -> "dark"

fromString : String -> Maybe Theme
fromString theme =
  case theme of
    "light" -> Just Light
    "dark" -> Just Dark
    _ -> Nothing

