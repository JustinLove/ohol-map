module Zipper exposing
  ( Zipper
  , construct
  , length
  , toList
  , mapToList
  , current
  , next
  , previous
  , goto
  , nextMatching
  , previousMatching
  )

type Zipper a = Zipper (List a) a (List a)

construct : a -> (List a) -> Zipper a
construct head tail =
  Zipper [] head tail

length : Zipper a -> Int
length (Zipper before x after) =
  (List.length before) + 1 + (List.length after)

toList : Zipper a -> List a
toList (Zipper before x after) =
  List.append (List.reverse before) (x :: after)

mapToList : (a -> b) -> Zipper a -> List b
mapToList f zipper =
  foldr (\y l -> (f y) :: l) [] zipper

foldr : (a -> b -> b) -> b -> Zipper a -> b
foldr f init (Zipper before x after) =
  let
    a = List.foldr f init after
    b = f x a
  in
    List.foldl f b before

current : Zipper a -> a
current (Zipper _ x _) = x

next : Zipper a -> Zipper a
next (Zipper before x after) =
  case after of
    head :: tail -> Zipper (x :: before) head tail
    _ ->
      case (List.reverse (x :: before)) of
        hd :: tl -> Zipper [] hd tl
        _ -> Zipper before x after

previous : Zipper a -> Zipper a
previous (Zipper before x after) =
  case before of
    head :: tail -> Zipper tail head (x :: after)
    _ ->
      case (List.reverse (x :: after)) of
        hd :: tl -> Zipper tl hd []
        _ -> Zipper before x after

goto : a -> Zipper a -> Zipper a
goto target zipper =
  if (current zipper) == target then
    zipper
  else
    case nextTo target zipper of
      Just zip -> zip
      Nothing -> previousTo target zipper |> Maybe.withDefault zipper

nextTo : a -> Zipper a -> Maybe (Zipper a)
nextTo target (Zipper before x after) =
  case after of
    head :: tail ->
      if head == target then
        Just (Zipper (x :: before) head tail)
      else
        nextTo target (Zipper (x :: before) head tail)
    _ ->
      Nothing

previousTo : a -> Zipper a -> Maybe (Zipper a)
previousTo target (Zipper before x after) =
  case before of
    head :: tail ->
      if head == target then
        Just (Zipper tail head (x :: after))
      else
        previousTo target (Zipper tail head (x :: after))
    _ ->
      Nothing

nextMatching : (a -> Bool) -> Zipper a -> Zipper a
nextMatching test zipper =
  nextMatchingInner (current zipper) test (next zipper)

nextMatchingInner : a -> (a -> Bool) -> Zipper a -> Zipper a
nextMatchingInner start test ((Zipper _ x _) as zipper) =
  if x == start then
    zipper
  else if test x then
    zipper
  else
    nextMatchingInner start test (next zipper)

previousMatching : (a -> Bool) -> Zipper a -> Zipper a
previousMatching test zipper =
  previousMatchingInner (current zipper) test (previous zipper)

previousMatchingInner : a -> (a -> Bool) -> Zipper a -> Zipper a
previousMatchingInner start test ((Zipper _ x _) as zipper) =
  if x == start then
    zipper
  else if test x then
    zipper
  else
    previousMatchingInner start test (previous zipper)
