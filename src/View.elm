module View exposing
  ( Msg(..)
  , timeNoticeDuration
  , view
  , document
  )

import Leaflet exposing (Point, PointColor(..), PointLocation(..))
import Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Theme exposing (Theme)
import Zipper exposing (Zipper)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (on, stopPropagationOn)
import Html.Keyed
import Http
import Json.Decode
import SolidColor
import Set exposing (Set)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Time exposing (Posix)
import Url exposing (Url)
import Url.Builder as Url

type Msg
  = None
  | PerformLifeSearch String
  | LifeTyping String
  | ObjectTyping String
  | SelectTimeMode TimeMode
  | CoarseStartTime Posix
  | StartTime Posix
  | HoursBefore Int
  | HoursAfter Int
  | ToggleEvesOnly Bool
  | ToggleUTC Bool
  | ChangeTheme Theme
  | ToggleAnimated Bool
  | GameSecondsPerSecond Int
  | FramesPerSecond Int
  | MapTime Posix
  | Play
  | Pause
  | ToggleFadeTallObjects Bool
  | ToggleShowMonuments Bool
  | ToggleShowOnlyCurrentMonuments Bool
  | SelectNaturalObjectZoom Int
  | ToggleShowActivityMap Bool
  | SelectActivityMapSampleSize Int
  | SelectActivityMapZoom Int
  | ToggleShowLifeData Bool
  | SelectPointColor PointColor
  | SelectPointLocation PointLocation
  | SelectMatchingLife Life
  | ToggleMatchingObject ObjectId Bool
  | SelectMatchingObject ObjectId
  | ExitBrowseLocations
  | SelectBrowseLocation BrowseLocation
  | SelectBrowsePlacement BrowsePlacement
  | Previous
  | Next
  | ToggleBrowseProbablyTutorial Bool
  | ToggleAllObjects Bool
  | SelectMaximumObjects (Maybe Int)
  | LockObjects
  | DownloadLocked
  | ClearLocked
  | ToggleLockObject ObjectId Bool
  | ToggleIconDisplay ObjectId Bool
  | SelectLineage Life
  | SelectSidebarMode SidebarMode
  | SelectSearchMode SearchMode
  | SelectObjectListMode ObjectListMode
  | SelectServer Int
  | SelectArc (Maybe Arc)
  | SelectArcCoarse (Maybe Arc)
  | SelectShow

timeNoticeDuration = 6000

document : (Msg -> msg) -> Model -> Browser.Document msg
document tagger model =
  { title = "OHOL Map"
  , body = [Html.map tagger (view model)]
  }

view : Model -> Html Msg
view model =
  layout
    [ width fill, height fill
    , Font.color (themePalette model.theme).foreground
    , Font.size (scaled 2)
    , Background.color (themePalette model.theme).background
    , themeClass model.theme
    , htmlAttribute (Html.Attributes.id "layout")
    ] <|
    Keyed.row [ width fill, height fill ]
      [ ( "main"
        , Keyed.column [ width fill, height fill ]
          [ ( "map"
            , el
              [ width fill
              , height fill
              , Font.size 12
              , inFront (timeOverlay model)
              , htmlAttribute (Html.Attributes.id "map-container")
              , if model.graticuleVisible then
                  htmlAttribute (Html.Attributes.class "graticule")
                else
                  htmlAttribute (Html.Attributes.class "")
              ]
              (html <| Html.div [ Html.Attributes.id "map" ] [])
            )
          , ( "timeline"
            , if model.dataAnimated && model.timeRange /= Nothing then
                timeline model
              else
                none
            )
          ]
        )
      , ( "sidebar"
        , case model.sidebar of
            OpenSidebar -> sidebar model
            ClosedSidebar -> none
        )
      ]

timeOverlay : Model -> Element Msg
timeOverlay model =
  case model.notice of
    TimeNotice time until ->
      time
        |> dateYearMonthDayHourMinute model.zone
        |> text
        |> el [ centerX, centerY ]
        |> el
          [ width fill
          , height fill
          , Font.size 48
          , alpha <|
            max 0
              (
                ( (until |> Time.posixToMillis |> toFloat)
                - (model.time |>  Time.posixToMillis |> toFloat)
                )
              / (timeNoticeDuration/2)
              )
          , htmlAttribute (Html.Attributes.id "time-overlay")
          , htmlAttribute (Html.Attributes.style "z-index" "1000")
          , htmlAttribute (Html.Attributes.style "pointer-events" "none")
          ]
    NoNotice ->
      none


timeline : Model -> Element Msg
timeline model =
  let
    palette = themePalette model.theme
  in
  case (model.mapTime, model.timeRange) of
    (Just time, Just (start, end)) ->
      column [ width fill ]
        [ row []
          [ case model.player of
            Stopped ->
              Input.button [ width (px 40), padding 4 ]
                { onPress = Just Play
                , label = el [ centerX ] <| icon "play3"
                }
            _ ->
              Input.button [ width (px 40), padding 4 ]
                { onPress = Just Pause
                , label = el [ centerX ] <| icon "pause2"
                }
          , if model.zone == Time.utc then
              Input.button []
                { onPress = Just (ToggleUTC False)
                , label = dateWithZone model.zone time palette
                }
            else
              Input.button []
                { onPress = Just (ToggleUTC True)
                , label = dateWithZone model.zone time palette
                }
          , ( model.currentArc
              |> Maybe.andThen (yearOfArc time)
              |> Maybe.map (\s -> text (", " ++ s))
              |> Maybe.withDefault none
            )
            {-
          , ( time
              |> Time.posixToMillis
              |> String.fromInt
              |> (\s -> text (", " ++ s))
            )
            -}
          ]
        , Input.slider
          [ Background.color palette.control ]
          { onChange = round
            >> ((*) 1000)
            >> Time.millisToPosix
            >> MapTime
          , label = Input.labelHidden "timeline"
          , min = (start |> posixToFloat 0) + 1
          , max = end |> posixToFloat 0
          , value = time |> posixToFloat 0
          , thumb = Input.defaultThumb
          , step = Just 1
          }
        ]
    _ ->
      none

sidebar : Model -> Element Msg
sidebar model =
  column
    [ width (fill |> maximum 330)
    , height fill
    , alignTop
    , htmlAttribute (Html.Attributes.id "sidebar")
    ]
    [ sidebarModeSelect model
    , case model.sidebarMode of
        Search -> searchPanel model
        DataFilter -> dataFilter model
        Cosmetics -> cosmetics model
    ]

sidebarModeSelect : Model -> Element Msg
sidebarModeSelect model =
  let
    palette = (themePalette model.theme)
  in
  row
    [ width fill
    , Border.color palette.divider
    , Border.widthEach
      { bottom = 1
      , left = 0
      , right = 0
      , top = 0
      }
    ]
    [ tabHeader "search" "Search" SelectSidebarMode Search model.sidebarMode palette
    , tabHeader "filter" "Data" SelectSidebarMode DataFilter model.sidebarMode palette
    , tabHeader "paint-format" "Format" SelectSidebarMode Cosmetics model.sidebarMode palette
    ]

searchModeSelect : Model -> Element Msg
searchModeSelect model =
  let
    palette = (themePalette model.theme)
  in
  row
    [ width fill
    , Border.color palette.divider
    , Border.widthEach
      { bottom = 1
      , left = 0
      , right = 0
      , top = 0
      }
    ]
    [ tabHeader "users" "Lives" SelectSearchMode SearchLives model.searchMode palette
    , tabHeader "locate" "Objects" SelectSearchMode SearchObjects model.searchMode palette
    ]

objectListSelect : Model -> Element Msg
objectListSelect model =
  let
    palette = (themePalette model.theme)
    locked = model.lockedObjects
      |> Set.size
      |> String.fromInt
      |> (\n -> n ++ " Locked")
  in
  row
    [ width fill
    , Border.color palette.divider
    , Border.widthEach
      { bottom = 0
      , left = 0
      , right = 0
      , top = 1
      }
    ]
    [ tabHeader "search" "Search" SelectObjectListMode MatchingObjects model.objectListMode palette
    , tabHeader "lock" locked SelectObjectListMode LockedObjects model.objectListMode palette
    ]

tabHeader : String -> String -> (mode -> Msg) -> mode -> mode -> Palette -> Element Msg
tabHeader ico name tagger mode current palette =
  Input.button [ width fill ]
    { onPress = Just (tagger mode)
    , label = 
      el
        [ width fill
        , Background.color (if mode == current then palette.highlight else palette.background)
        ] <|
        row [ centerX, spacing 6 ]
          [ el [ Font.size 16 ] <| icon ico
          , text name
          ]
    }

searchPanel : Model -> Element Msg
searchPanel model =
  column
    [ width fill
    , height fill
    ]
    [ searchModeSelect model
    , case model.searchMode of
      SearchLives -> lifeSearch model
      SearchObjects -> objectSearch model
    ]

lifeSearch : Model -> Element Msg
lifeSearch model =
  column
    [ width fill
    , height fill
    ]
    [ lifeSearchBox (themePalette model.theme) model.lifeSearchTerm
    , showLifeResult model model.lives
    ]

objectSearch : Model -> Element Msg
objectSearch model =
  let
    palette = (themePalette model.theme)
    valid = objectSearchValid model
    color = if valid then
        palette.foreground
      else
        palette.deemphasis
  in
  case model.focusObject of
    Just id ->
      showBrowseObjects model id
    Nothing ->
      column
        [ width fill
        , height fill
        , Font.color color
        ]
        [ case model.objectListMode of
            MatchingObjects -> objectFinder model
            LockedObjects -> showLockedObjects model (Set.toList model.lockedObjects)
        , el [ alignBottom, width fill ] <| objectListSelect model
        ]

objectFinder : Model -> Element Msg
objectFinder model =
  let
    palette = (themePalette model.theme)
    valid = objectSearchValid model
    objects = if valid then
        model.matchingObjects
      else
        []
  in
  column
    [ width fill
    , height fill
    ]
    [ objectSearchBox palette model.objectSearchTerm
    , showObjectResult model objects
    ]

objectSearchValid : Model -> Bool
objectSearchValid model =
  case model.center of
    DefaultCenter -> False
    --SetCenter {z} -> z >= 24
    SetCenter _ -> True

lifeSearchBox : Palette -> String -> Element Msg
lifeSearchBox palette term =
  Input.text
    [ padding 2
    , Background.color palette.input
    , onChange PerformLifeSearch
    ] <|
    { onChange = LifeTyping
    , text = term
    , placeholder = Nothing
    , label = Input.labelAbove [] <| text "Character Name, Hash, or Id"
    }

objectSearchBox : Palette -> String -> Element Msg
objectSearchBox palette term =
  row [ spacing 2, width fill ]
    [ Input.text
      [ padding 2
      , width fill
      , Background.color palette.input
      , onEnter LockObjects
      ] <|
      { onChange = ObjectTyping
      , text = term
      , placeholder = Nothing
      , label = Input.labelAbove [] <| text "Object Name or Id"
      }
    , column []
      [ text ""
      , Input.button
          [ padding 2
          , Border.color palette.divider
          , Border.width 1
          , Border.rounded 6
          , Background.color palette.control
          ]
          { onPress = Just LockObjects
          , label = el [ centerX ] <| text "Lock"
          }
      ]
    ]

showLifeResult model remote =
  case remote of
    NotRequested ->
      none
    NotAvailable ->
      none
    Loading ->
      el [ centerX, centerY ] <| text "Loading"
    Data lives ->
      showMatchingLives model lives
    Failed error ->
      showError error

showObjectResult : Model -> List ObjectId -> Element Msg
showObjectResult model objects =
  if List.isEmpty objects then
    none
  else
    showMatchingObjects model objects

showMatchingLives model lives =
  column [ spacing 10, width fill, height fill, scrollbarY ]
    (lives
      |> List.map (showMatchingLife model)
      |> (::) lifeListHeader
    )

showMatchingObjects : Model -> List ObjectId -> Element Msg
showMatchingObjects model objects =
  column [ spacing 0, width fill, height fill, scrollbarY ]
    (objects
      |> List.map (showMatchingObject model)
      |> (::) (objectListHeader model)
    )

showLockedObjects : Model -> List ObjectId -> Element Msg
showLockedObjects model objects =
  column [ spacing 0, width fill, height fill, scrollbarY ]
    (objects
      |> List.map (showLockedObject model)
      |> (::) (lockedObjectListHeader model)
    )

showBrowseObjects : Model -> ObjectId -> Element Msg
showBrowseObjects model id =
  let
    palette = (themePalette model.theme)
  in
  column [ width fill, height fill, spacing 10 ]
    [ browseObjectsHeader model id
    , if model.dataAnimated then
        (model.spanData
        |> Maybe.map .browsePlacements
        |> Maybe.andThen (Dict.get id)
        |> Maybe.withDefault NotRequested
        |> showBrowseRemoteStatus (showBrowsePlacements model.zone palette id model.browseProbablyTutorial)
        )
      else
        (model.spanData
        |> Maybe.map .browseLocations
        |> Maybe.andThen (Dict.get id)
        |> Maybe.withDefault NotRequested
        |> showBrowseRemoteStatus (showBrowseLocations palette id model.browseProbablyTutorial)
        )
    ]

showBrowseLocations : Palette -> ObjectId -> Bool -> Zipper BrowseLocation -> Element Msg
showBrowseLocations palette id browseProbablyTutorial locations =
  showBrowseItems
    { palette = palette
    , tagger = SelectBrowseLocation
    , header = (browseLocationListHeader palette)
    , label = (showBrowseLocationDetail id)
    , inTutorial = browseLocationInTutorial
    , items = locations
    , browseProbablyTutorial = browseProbablyTutorial
    }

showBrowsePlacements : Time.Zone -> Palette -> ObjectId -> Bool -> Zipper BrowsePlacement -> Element Msg
showBrowsePlacements zone palette id browseProbablyTutorial placements =
  showBrowseItems
    { palette = palette
    , tagger = SelectBrowsePlacement
    , header = (browsePlacementListHeader palette)
    , label = (showBrowsePlacementDetail zone id)
    , inTutorial = browsePlacementInTutorial
    , items = placements
    , browseProbablyTutorial = browseProbablyTutorial
    }

showBrowseItems :
  { palette : Palette
  , tagger : (a -> Msg)
  , header : (Zipper a -> Element Msg)
  , label : (a -> Element Msg)
  , inTutorial : (a -> Bool)
  , items : Zipper a
  , browseProbablyTutorial : Bool
  } -> Element Msg
showBrowseItems {palette, tagger, header, label, inTutorial, items, browseProbablyTutorial} =
  let
    filteredItems = items
      |> Zipper.toList
      |> (\list ->
        if browseProbablyTutorial then
          list
        else
          List.filter (not << inTutorial) list
        )
  in
  filteredItems
    |> List.map (showBrowseItem
      { palette = palette
      , tagger = tagger
      , label = label
      , current = (Zipper.current items)
      })
    |> (::) (header items)
    |> (::) (previousNextButtons palette browseProbablyTutorial filteredItems)
    |> column [ spacing 0, width fill, height fill, scrollbarY ]

showBrowseRemoteStatus : (a -> Element msg) -> RemoteData a -> Element msg
showBrowseRemoteStatus withData remote =
  case remote of
    NotRequested ->
      twoPartMessage "Not Requested" "This may be a bug"
    NotAvailable ->
      twoPartMessage "No Data Available" "Extremely common objets are not indexed"
    Loading ->
      el [ centerX, centerY ] <| text "Loading"
    Data data ->
      withData data
    Failed error ->
      showError error

lifeListHeader =
  row
    [ width fill
    , Font.size 10
    , Font.underline
    ]
    [ lifeDetailHeader
    , el [ width (px 30) ] (el [ centerX ] (text "Lin"))
    , el [ width (px 30) ] (el [ centerX ] (text "Tree"))
    ]

objectListHeader : Model -> Element Msg
objectListHeader model =
  column
    [ Font.size 16
    , padding 6
    , width fill
    ]
    [ spanDateRange model
    , row
      [ spacing 10
      , width fill
      ]
      [ row [ spacing 10, alignLeft, Font.underline ]
        [ el [ width (px 30) ] <| text "Icon"
        , Input.checkbox [ spacing 2 ]
          { onChange = ToggleAllObjects
          , checked = (areAllObjectChecked model)
          , label = Input.labelHidden "toggle all"
          , icon = Input.defaultCheckbox
          }
        , text "Name"
        ]
      , row [ alignRight, spacing 2 ]
        [ model.totalMatchingObjects
          |> String.fromInt
          |> text
        , text "/"
        , model.maxiumMatchingObjects
          |> Maybe.map String.fromInt
          |> Maybe.withDefault "all"
          |> text
        , text " "
        , Input.checkbox [ spacing 2 ]
          { onChange = (\checked -> SelectMaximumObjects
              (if checked then Nothing else Just 20)
            )
          , checked = case model.maxiumMatchingObjects of
              Just _ -> False
              Nothing -> True
          , label = Input.labelRight [] (text "Show all")
          , icon = Input.defaultCheckbox
          }
        ]
      ]
    ]

spanDateRange : Model -> Element msg
spanDateRange model =
  case model.spanData of
    Just span ->
      if model.dataAnimated then
        row [ width fill ]
          [ row [ alignRight ]
            [ span.start |> (dateYearMonthDayHourMinute model.zone) |> text
            , text " to "
            , span.end |> (dateYearMonthDayHourMinute model.zone) |> text
            ]
          ]
      else
        row [ width fill ]
          [ row [ alignRight ]
            [ text "as of "
            , span.end |> (dateYearMonthDayHourMinute model.zone) |> text
            ]
          ]
    Nothing ->
      none

lockedObjectListHeader : Model -> Element Msg
lockedObjectListHeader model =
  let
    palette = (themePalette model.theme)
  in
  row
    [ Font.size 16
    , padding 6
    , spacing 10
    , width fill
    ]
    [ row [ spacing 10, alignLeft, Font.underline ]
      [ el [ width (px 30) ] <| text "Icon"
      , el [ width (px 32) ] <| text "Del"
      , el [ alignLeft ] <| text "Name"
      ]
    , row [ spacing 10, alignRight ]
        [ {-Input.button
            [ padding 2
            , Border.color palette.divider
            , Border.width 1
            , Border.rounded 6
            , Background.color palette.control
            ]
            { onPress = Just DownloadLocked
            , label = row [ centerX, spacing 4 ]
              [ text "Download"
              ]
            }
        , -}Input.button
            [ padding 2
            , Border.color palette.divider
            , Border.width 1
            , Border.rounded 6
            , Background.color palette.control
            ]
            { onPress = Just ClearLocked
            , label = row [ centerX, spacing 4 ]
              [ el [ Font.size 14 ] <| icon "cancel-circle"
              , text "Clear All"
              ]
            }
        ]
    ]

browseObjectsHeader : Model -> ObjectId -> Element Msg
browseObjectsHeader model id =
  let
    (title, _) = objectNameParts model id
    palette = (themePalette model.theme)
  in
    row [ width fill ]
      [ Input.button
        [ padding 2
        , Border.color palette.divider
        , Border.widthEach
          { bottom = 1
          , left = 0
          , right = 0
          , top = 0
          }
        , Background.color palette.control
        , width fill
        ]
        { onPress = Just ExitBrowseLocations
        , label = row [ centerX, spacing 4 ]
          [ el [ Font.size 14 ] <| icon "cancel-circle"
          , objectImageWithSwatch id title
          , el [ padding 6 ] <| objectTitle id title ""
          ]
        }
      ]

browseLocationListHeader : Palette -> Zipper BrowseLocation -> Element Msg
browseLocationListHeader palette locations =
  row
    [ Font.size 16
    , Font.underline
    , padding 6
    , spacing 10
    , width fill
    , alignLeft
    ]
    [ el [ width (px 16) ] <| none
    , el [ width (px 70), Font.alignRight ] <| text "x"
    , el [ width (px 70), Font.alignRight ] <| text "y"
    ]

browsePlacementListHeader : Palette -> Zipper BrowsePlacement -> Element Msg
browsePlacementListHeader palette placements =
  row
    [ Font.size 16
    , Font.underline
    , padding 6
    , spacing 10
    , width fill
    , alignLeft
    ]
    [ el [ width (px 16) ] <| none
    , el [ width (px 70), Font.alignRight ] <| text "x"
    , el [ width (px 70), Font.alignRight ] <| text "y"
    , el [ width (px 110), Font.alignLeft ] <| text "time"
    ]

previousNextButtons : Palette -> Bool -> List a -> Element Msg
previousNextButtons palette browseProbablyTutorial items =
  row [ width fill, spacing 10, padding 6 ]
    [ Input.button
      [ padding 2
      , Border.color palette.divider
      , Border.width 1
      , Border.rounded 6
      , Background.color palette.control
      ]
      { onPress = Just Previous
      , label = row [ centerX, spacing 4 ]
        [ el [ Font.size 14 ] <| icon "rewind"
        , text "Prev"
        ]
      }
    , Input.button
      [ padding 2
      , Border.color palette.divider
      , Border.width 1
      , Border.rounded 6
      , Background.color palette.control
      ]
      { onPress = Just Next
      , label = row [ centerX, spacing 4 ]
        [ text "Next"
        , el [ Font.size 14 ] <| icon "forward"
        ]
      }
    , Input.checkbox [ spacing 4 ]
      { onChange = ToggleBrowseProbablyTutorial
      , checked = browseProbablyTutorial
      , label = Input.labelRight [] (text "tutorial")
      , icon = Input.defaultCheckbox
      }
    , List.length items
      |> String.fromInt
      |> text
      |> el [ alignRight ]
    ]

showMatchingLife model life =
  row
    [ if Just life == model.focusLife then
        Background.color (themePalette model.theme).highlight
      else
        Background.color (themePalette model.theme).background
    ]
    [ Input.button []
      { onPress = Just (SelectMatchingLife life)
      , label = showMatchingLifeDetail model life
      }
    , Input.button [ width(px 30), padding 10 ]
      { onPress = Just (SelectLineage life)
      , label = el [ centerX ] <| icon "users"
      }
    , newTabLink [ width(px 30), padding 10 ]
      { url = lineageUrl model.lineageUrl life.serverId life.epoch life.playerid
      , label = el [ centerX ] <| icon "tree"
      }
    ]

showMatchingObject : Model -> ObjectId -> Element Msg
showMatchingObject model id =
  let
    (title, attrs) = objectNameParts model id
    (count, indexed) = objectStats model id
    palette = themePalette model.theme
  in
  row [ padding 6, width fill ]
    [ objectIconControl (effectiveImageObjects model) id title
      |> el [ moveLeft 6 ]
    , column [ width (fill |> maximum 250), clipX ]
      [ row [width fill ]
        [ Input.checkbox [ spacing 8 ]
          { onChange = ToggleMatchingObject id
          , checked = Set.member id model.selectedMatchingObjects
          , label = Input.labelRight [ ] <| objectTitle id title attrs
          , icon = Input.defaultCheckbox
          }
        ]
      , Input.checkbox [ spacing 8, Font.size 16, width shrink ]
        { onChange = ToggleLockObject id
        , checked = Set.member id model.lockedObjects
        , label = Input.labelHidden "lock/unlock"
        , icon = (\checked ->
          if checked then
            icon "lock"
          else
            icon "unlocked"
          )
        }
      ]
    , el [ alignRight ]
      (if indexed then
        Input.button
          [ height fill
          , padding 6
          , Background.color palette.control
          ]
          { onPress = Just (SelectMatchingObject id)
          , label = row []
            [ count
              |> String.fromInt
              |> text
            , icon "forward"
            ]
          }
      else
        Input.button
          [ height fill
          , padding 6
          , Background.color palette.background
          , Font.color palette.divider
          ]
          { onPress = Nothing
          , label = row []
            [ count
              |> String.fromInt
              |> text
              |> el [ Font.strike ]
            , icon "forward"
            ]
          }
        )
    ]

showLockedObject : Model -> ObjectId -> Element Msg
showLockedObject model id =
  let (title, attrs) = objectNameParts model id in
  row []
    [ objectIconControl (effectiveImageObjects model) id title
    , column [ padding 6 ]
      [ row [ spacing 8]
        [ Input.button [ Font.size 14, Region.description "remove from locked" ]
          { onPress = Just (ToggleLockObject id False)
          , label = icon "cancel-circle"
          }
        , objectTitle id title attrs
        ]
      ]
    ]

showBrowseItem :
  { palette : Palette
  , tagger : (a -> msg)
  , label : (a -> Element msg)
  , current : a
  } -> a -> Element msg
showBrowseItem {palette, tagger, label, current} item =
  el
    [ if item == current then
        Background.color palette.highlight
      else
        Background.color palette.background
    , width fill
    ]
    <| Input.button [ width fill ]
      { onPress = Just (tagger item)
      , label = label item
      }

showBrowseLocationDetail : ObjectId -> BrowseLocation -> Element Msg
showBrowseLocationDetail id (BrowseLocation x y ) =
  row [ padding 6, spacing 10, width fill ]
    [ el [ width (px 16), Font.color (objectColor id) ] (icon "locate")
    , x
      |> String.fromInt
      |> text
      |> el [ width (px 70), Font.alignRight ]
    , y
      |> String.fromInt
      |> text
      |> el [ width (px 70), Font.alignRight ]
    ]

showBrowsePlacementDetail : Time.Zone -> ObjectId -> BrowsePlacement -> Element Msg
showBrowsePlacementDetail zone id (BrowsePlacement x y t) =
  row [ padding 6, spacing 10, width fill ]
    [ el [ width (px 16), Font.color (objectColor id) ] (icon "locate")
    , x
      |> String.fromInt
      |> text
      |> el [ width (px 70), Font.alignRight ]
    , y
      |> String.fromInt
      |> text
      |> el [ width (px 70), Font.alignRight ]
    , t
      |> dateMonthDayHourMinute zone
      |> text
      |> el [ width (px 110) ]
    ]

objectTitle : ObjectId -> String -> String -> Element Msg
objectTitle id title attrs =
  el
    [ (400 // (String.length title))
      |> clamp 8 20
      |> Font.size
    , ((String.fromInt id) ++ " " ++ title)
      |> Html.Attributes.title
      |> htmlAttribute
    , below (row [ Font.size 16, spacing 4 ]
        [ attrs |> text
        ])
    ] (title |> text)

objectSwatch : ObjectId -> Element Msg
objectSwatch id =
  el [ Font.color (objectColor id) ] (icon "locate")

objectImage : ObjectId -> String -> Element Msg
objectImage id title =
  Html.img
    [ Html.Attributes.width 40
    , Html.Attributes.class "object-icon"
    , Html.Attributes.src ("static/sprites/obj_" ++ (String.fromInt id) ++ ".png")
    , Html.Attributes.alt title
    ] []
    |> html
    |> el
      [ centerX
      , centerY
      , width (px 40)
      , height (px 40)
      ]

objectImageWithSwatch : ObjectId -> String -> Element Msg
objectImageWithSwatch id title =
  objectImage id title
    |> el [ inFront (el [ moveDown 20, moveRight 20 ] (objectSwatch id)) ]

objectImageWithSwatchPrimary : ObjectId -> String -> Element Msg
objectImageWithSwatchPrimary id title =
  objectImage id title
    |> el [ alpha 0.5 ]
    |> el [ inFront (el [ moveDown 20, moveRight 20 ] (objectSwatch id)) ]

objectIconControl : Set ObjectId -> ObjectId -> String -> Element Msg
objectIconControl imageObjects id title =
  Input.checkbox
    [ "click to change icon display mode"
      |> Html.Attributes.title
      |> htmlAttribute
    ]
    { onChange = ToggleIconDisplay id
    , checked = Set.member id imageObjects
    , label = Input.labelHidden "icon display mode"
    , icon = (\checked ->
        if checked then
          objectImage id title
        else
          objectImageWithSwatchPrimary id title
      )
    }

lifeDetailHeader =
  column [ padding 4 ]
    [ row
      [ Font.size 16
      , spacing 10
      , width fill
      ]
      [ el [ width (px 30) ]
        (text "Age")
      , el [ width (px 140) ]
        (text "Born")
      , el [ width (px 30) ]
        (text "Gen")
      ]
    ]


showMatchingLifeDetail model life =
  column [ padding 4 ]
    [ life.name
        |> Maybe.withDefault "nameless"
        |> text
    , row
      [ Font.size 16
      , spacing 10
      ]
      [ el [ width (px 30) ]
        (life.age
          |> Maybe.map ceiling
          |> Maybe.map String.fromInt
          |> Maybe.withDefault ""
          |> text
        )
      , el [ width (px 140) ]
        ( life.birthTime
          |> dateYearMonthDayHourMinute model.zone
          |> text
        )
      , el [ width (px 30) ]
        ( life.generation
          |> String.fromInt
          |> text
        )
      ]
    ]

objectNameParts : Model -> ObjectId -> (String, String)
objectNameParts model objectId =
  case objectName model objectId |> String.split "#" of
    title :: attrs :: _ -> (String.trim title, String.trim attrs)
    title :: _ -> (String.trim title, "")
    _ -> ("unknown", "")

objectName : Model -> ObjectId -> String
objectName model objectId =
  model.selectedServer
    |> Maybe.andThen (\id -> Dict.get id model.servers)
    |> Maybe.map .objects
    |> Maybe.andThen (Dict.get objectId)
    |> Maybe.withDefault "unknown"

objectStats : Model -> ObjectId -> (Int, Bool)
objectStats model objectId =
  model.spanData
    |> Maybe.map (if model.dataAnimated then .logObjectSearchIndex else .keyObjectSearchIndex)
    |> Maybe.map (RemoteData.withDefault Dict.empty)
    |> Maybe.andThen (Dict.get objectId)
    |> Maybe.withDefault (0, False)

dateYearMonthDayHourMinute : Time.Zone -> Posix -> String
dateYearMonthDayHourMinute zone time =
  let
    year = Time.toYear zone time |> String.fromInt
    month = Time.toMonth zone time |> formatMonth
    day = Time.toDay zone time |> String.fromInt |> String.padLeft 2 '0'
    hour = Time.toHour zone time |> String.fromInt |> String.padLeft 2 '0'
    minute = Time.toMinute zone time |> String.fromInt |> String.padLeft 2 '0'
  in
    year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute

dateMonthDayHourMinute : Time.Zone -> Posix -> String
dateMonthDayHourMinute zone time =
  let
    month = Time.toMonth zone time |> formatMonth
    day = Time.toDay zone time |> String.fromInt |> String.padLeft 2 '0'
    hour = Time.toHour zone time |> String.fromInt |> String.padLeft 2 '0'
    minute = Time.toMinute zone time |> String.fromInt |> String.padLeft 2 '0'
  in
    month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute

dateWithSeconds : Time.Zone -> Posix -> String
dateWithSeconds zone time =
  let
    second = Time.toSecond zone time |> String.fromInt |> String.padLeft 2 '0'
  in
    (dateYearMonthDayHourMinute zone time) ++ ":" ++ second

dateWithZone : Time.Zone -> Posix -> Palette -> Element Msg
dateWithZone zone time palette =
  row []
    [ text (dateWithSeconds zone time)
    , text " "
    , (if zone == Time.utc then
        "utc"
      else
        "local"
      )
        |> text
        |> el [ Font.size 16, Font.color palette.deemphasis, alignBottom, padding 2 ]
    ]

formatMonth : Time.Month -> String
formatMonth month =
  case month of
    Time.Jan -> "01"
    Time.Feb -> "02"
    Time.Mar -> "03"
    Time.Apr -> "04"
    Time.May -> "05"
    Time.Jun -> "06"
    Time.Jul -> "07"
    Time.Aug -> "08"
    Time.Sep -> "09"
    Time.Oct -> "10"
    Time.Nov -> "11"
    Time.Dec -> "12"

--yearOfArc : Posix -> Arc -> Maybe String
yearOfArc time arc =
  let
    arcStart = arc.start |> Time.posixToMillis
    arcEnd = arc.end |> Maybe.withDefault time |> Time.posixToMillis
    ms = time |> Time.posixToMillis
  in
    if arcStart <= ms && ms <= arcEnd then
      ((toFloat (ms - arcStart)) / (60 * 1000))
        |> round
        |> String.fromInt
        |> (\s -> "year " ++ s)
        |> Just
    else
      Nothing

lineageUrl : String -> Int -> Int -> Int -> String
lineageUrl base serverId epoch playerid =
  Url.custom (Url.CrossOrigin base) [] []
    (Url.toQuery
      [ Url.int "server_id" serverId
      , Url.int "epoch" epoch
      , Url.int "playerid" playerid
      ]
      |> String.dropLeft 1
      |> Just
    )

dataFilter : Model -> Element Msg
dataFilter model =
  el [ width fill, height fill, scrollbarY] <|
    column
      [ width (fill |> maximum 300)
      , height fill
      , spacing 10
      ]
      [ dataAction model
      , dateRangeSelect model
      , dataOptions model
      , serverSelect model.servers model.selectedServer (themePalette model.theme)
      , presets model
      ]

dataAction model =
  let
    palette = themePalette model.theme
  in
  el [ width fill, padding 5 ] <|
    case model.dataLayer of
      NotRequested -> dataButtonEnabled palette
      NotAvailable -> dataButtonDisabled palette
      Loading -> dataButtonDisabled palette
      Data _ -> dataButtonEnabled palette
      Failed _ -> dataButtonEnabled palette

dataButtonEnabled : Palette -> Element Msg
dataButtonEnabled palette =
  Input.button
    [ width fill
    , padding 5
    , Border.color palette.divider
    , Border.width 1
    , Border.rounded 6
    , Background.color palette.control
    ]
    { onPress = Just SelectShow
    , label = el [ centerX ] <| text "Show"
    }

dataButtonDisabled : Palette -> Element Msg
dataButtonDisabled palette =
  Input.button
    [ width fill
    , padding 5
    , Border.color palette.divider
    , Border.width 1
    , Border.rounded 6
    , Background.color palette.background
    , Font.color palette.divider
    ]
    { onPress = Nothing
    , label = el [ centerX ] <| text "Show"
    }

startTimeSelect : Model -> Element Msg
startTimeSelect model =
  let
    selectedServer = model.selectedServer |> Maybe.andThen (\id -> Dict.get id model.servers)
    controlColor = (themePalette model.theme).control
  in
  column [ width fill, spacing 2 ]
    [ Input.slider
      [ Background.color controlColor ]
      { onChange = round
        >> ((*) 1000)
        >> Time.millisToPosix
        >> CoarseStartTime
      , label = Input.labelAbove [] <|
        row []
          [ text "Coarse Start "
          , ( model.coarseStartTime
              |> dateYearMonthDayHourMinute model.zone
              |> text
            )
          ]
      , min = serverMinTime model.servers selectedServer
      , max = serverMaxTime model.servers selectedServer
      , value = model.coarseStartTime |> posixToFloat 0
      , thumb = Input.defaultThumb
      , step = Just 1
      }
    , Input.slider
      [ Background.color controlColor ]
      { onChange = round
        >> ((*) 1000)
        >> Time.millisToPosix
        >> StartTime
      , label = Input.labelAbove [] <|
        row []
          [ text "Fine Start "
          , ( model.startTime
              |> dateYearMonthDayHourMinute model.zone
              |> text
            )
          ]
      , min = model.coarseStartTime |> posixToFloat -7
      , max = model.coarseStartTime |> posixToFloat 7
      , value = model.startTime |> posixToFloat 0
      , thumb = Input.defaultThumb
      , step = Just 1
      }
    ]

dateRangeSelect : Model -> Element Msg
dateRangeSelect model =
  column
    [ width fill
    , spacing 2
    , padding 4
    , Border.width 1
    , Border.color (themePalette model.theme).divider
    ]
    [ Input.radioRow [ spacing 10 ]
        { onChange = SelectTimeMode
        , selected = Just model.timeMode
        , label = Input.labelAbove [ paddingXY 10 0 ] (text "Time Range")
        , options = 
          [ Input.option ServerRange (text "Server")
          , Input.option FromNow (text "Now")
          , Input.option ArcRange (text "Arc")
          ]
        }
    , case model.timeMode of
      FromNow ->
        timeBeforeSelect model
      ServerRange ->
        column [ width fill, spacing 2 ]
          [ timeAfterSelect model
          , startTimeSelect model
          ]
      ArcRange ->
        arcSelect model
    ]

hoursText : String -> Int -> String
hoursText label totalHours =
  let
    days = totalHours // 24
    hours = totalHours |> modBy 24
  in
    [
      if days > 0 then
        Just ((days |> String.fromInt) ++ "d")
      else
        Nothing
    , if hours > 0 then
        Just ((hours |> String.fromInt) ++ "h")
      else
        Nothing
    , Just label
  ]
    |> List.filterMap identity
    |> String.join " "

logSlider attributes slider =
  Input.slider
    attributes
    { slider
    | onChange = (\x -> e^x) >> slider.onChange
    , min = slider.min |> logBase e
    , max = slider.max |> logBase e
    , value = slider.value |> logBase e
    , step = Nothing
    }

reverseLogSlider attributes slider =
  let
    logMax = slider.max |> logBase e
  in
  Input.slider
    attributes
    { slider
    | onChange = (\x -> e^(logMax - x)) >> slider.onChange
    , min = slider.min |> logBase e
    , max = slider.max |> logBase e
    , value = slider.value
      |> logBase e
      |> (\x -> logMax - x)
    , step = Nothing
    }


timeAfterSelect : Model -> Element Msg
timeAfterSelect model =
  logSlider
    [ Background.color (themePalette model.theme).control ]
    { onChange = round >> HoursAfter
    , label = Input.labelAbove [] <|
      text (hoursText "After" model.hoursPeriod)
    , min = 1
    , max = 7*24
    , value = model.hoursPeriod |> toFloat
    , thumb = Input.defaultThumb
    , step = Nothing
    }

timeBeforeSelect : Model -> Element Msg
timeBeforeSelect model =
  reverseLogSlider
    [ Background.color (themePalette model.theme).control ]
    { onChange = round >> HoursBefore
    , label = Input.labelAbove [] <|
      text (hoursText "Before" model.hoursPeriod)
    , min = 1
    , max = 7*24
    , value = model.hoursPeriod |> toFloat
    , thumb = Input.defaultThumb
    , step = Nothing
    }

arcSelect : Model -> Element Msg
arcSelect model =
  let
    arcs = currentArcs model
    controlColor = (themePalette model.theme).control
  in
  case arcs of
    NotRequested -> none
    NotAvailable -> none
    Loading -> showLoading
    Failed error -> showError error
    Data list ->
      let
        coarseIndex = list
          |> List.indexedMap (\i a -> (i, a))
          |> List.filterMap (\(i,a) ->
            if Just a == model.coarseArc then
              Just i
            else
              Nothing
            )
          |> List.head
          |> Maybe.withDefault 0
          |> toFloat
        arcIndex = list
          |> List.indexedMap (\i a -> (i, a))
          |> List.filterMap (\(i,a) ->
            if Just a == model.currentArc then
              Just i
            else
              Nothing
            )
          |> List.head
          |> Maybe.withDefault 0
          |> toFloat
      in
      column [ width fill, spacing 2 ]
      [ Input.slider
          [ Background.color controlColor ]
          { onChange = round >> (arcForIndex model) >> SelectArcCoarse
          , label = Input.labelAbove [] <| text "Coarse"
          , min = 0
          , max = ((List.length list) - 1) |> toFloat
          , value = coarseIndex
          , thumb = Input.defaultThumb
          , step = Nothing
          }
      , Input.slider
          [ Background.color controlColor ]
          { onChange = round >> (arcForIndex model) >> SelectArc
          , label = Input.labelAbove [] <| text "Fine"
          , min = max (coarseIndex - 7) 0
          , max = min (coarseIndex + 7) (((List.length list) - 1) |> toFloat)
          , value = arcIndex
          , thumb = Input.defaultThumb
          , step = Nothing
          }
      , case model.currentArc of
          Just arc ->
            column []
              [ text <| dateYearMonthDayHourMinute model.zone arc.start
              , text <| dateYearMonthDayHourMinute model.zone (arc.end |> Maybe.withDefault model.time)
              ]
          Nothing ->
            text "Arc"
      ]

serverMinTime : Dict Int Server -> Maybe Server -> Float
serverMinTime =
  serverTime .minTime List.minimum

serverMaxTime : Dict Int Server -> Maybe Server -> Float
serverMaxTime =
  serverTime .maxTime List.maximum

serverTime
  :  (Server -> Posix)
  -> (List Float -> Maybe Float)
  -> Dict Int Server
  -> Maybe Server
  -> Float
serverTime field aggragate servers current =
  case current of
    Just server ->
      server |> field |> posixToFloat 0
    Nothing ->
      servers
        |> Dict.values
        |> List.map field
        |> List.map (posixToFloat 0)
        |> aggragate
        |> Maybe.withDefault 0

posixToFloat : Int -> Posix -> Float
posixToFloat offsetDays =
  Time.posixToMillis
    >> (\x -> x // 1000 + offsetDays*24*60*60)
    >> toFloat

arcForIndex : Model -> Int -> Maybe Arc
arcForIndex model index =
  case currentArcs model of
    Data list ->
      list
        |> List.drop index
        |> List.head
    _ -> Nothing

dataOptions : Model -> Element Msg
dataOptions model =
  column []
    [ Input.checkbox [ padding 10, spacing 2 ]
      { onChange = ToggleEvesOnly
      , checked = model.evesOnly
      , label = Input.labelRight [ padding 6 ] (text "Eves Only")
      , icon = Input.defaultCheckbox
      }
    ]

serverSelect : Dict Int Server -> Maybe Int -> Palette -> Element Msg
serverSelect servers serverId palette =
    Input.radioRow [ padding 10, spacing 2, htmlAttribute (Html.Attributes.class "server-select") ]
      { onChange = SelectServer
      , selected = serverId
      , label = Input.labelAbove [] (text "Server")
      , options = servers |> Dict.values |> List.map (serverItem palette)
      }

serverItem : Palette -> Server -> Input.Option Int Msg
serverItem palette server =
  Input.optionWith server.id
    (serverIcon palette server)

serverDisplayName : String -> String
serverDisplayName serverName =
  serverName
    |> String.split "."
    |> List.head
    |> Maybe.withDefault serverName

serverIcon : Palette -> Server -> Input.OptionState -> Element Msg
serverIcon palette server =
  serverIconForName palette server.serverName

serverIconForName : Palette -> String -> Input.OptionState -> Element Msg
serverIconForName palette serverName status =
  let
    name = serverDisplayName serverName
  in
  el [ htmlAttribute (Html.Attributes.title serverName) ] <|
  if "Band" == serverName then
    el
      [ width (px 45)
      , padding 3
      , Border.width 1
      , Border.color palette.foreground
      , Border.rounded 8
      , Background.color (if status == Input.Selected then palette.selected else palette.control)
      ]
      (el [ centerX ] (text "Band"))
  else if String.endsWith "oho.life" serverName then
    el
      [ width (px 45)
      , padding 3
      , Border.width 1
      , Border.color palette.foreground
      , Border.rounded 8
      , Background.color (if status == Input.Selected then palette.selected else palette.control)
      ]
      (el [ centerX ] (text "ccm"))
  else if String.endsWith "twohoursonelife.com" serverName then
    el
      [ width (px 30)
      , padding 3
      , Border.width 1
      , Border.color palette.foreground
      , Border.rounded 8
      , Background.color (if status == Input.Selected then palette.selected else palette.control)
      ]
      (el [ centerX ] (text "2h"))
  else if String.startsWith "server" name then
    let
      number = String.replace "server" "" name
    in
      el
        [ width (px 30)
        , padding 3
        , Border.width 1
        , Border.color palette.foreground
        , Border.rounded 8
        , Background.color (if status == Input.Selected then palette.selected else palette.control)
        ]
        (el [ centerX ] (text number))
  else if String.startsWith "bigserver" name then
    let
      number = String.replace "bigserver" "" name
    in
      el
        [ width (px 30)
        , Border.width 4
        , Border.color palette.foreground
        , Border.rounded 8
        , Font.heavy
        , Font.color (if status == Input.Selected then palette.background else palette.foreground)
        , Background.color (if status == Input.Selected then palette.selected else palette.control)
        ]
        (el [ centerX ] (text number))
  else
    text name

presets : Model -> Element Msg
presets model =
  column []
    [ text "Presets"
    , wrappedRow [ padding 10 ]
      [ link [ Font.color (themePalette model.theme).selected ]
          { url = centerUrl model.location model.mapTime Yesterday model.selectedServer model.center
          , label = text "Ambient Yesterday"
          }
      ]
    , wrappedRow [ padding 10 ]
      [ link [ Font.color (themePalette model.theme).selected ]
          { url = centerUrl model.location model.mapTime DailyReview model.selectedServer model.center
          , label = text "Daily Review"
          }
      ]
    ]

cosmetics : Model -> Element Msg
cosmetics model =
  let
    palette = themePalette model.theme
  in
  el [ width fill, height fill, scrollbarY] <|
    column
      [ width (fill |> maximum 300)
      , height fill
      , spacing 10
      ]
      [ lifeOptions palette model
      , monumentOptions palette model
      , objectOptions palette model
      , activityMapOptions palette model
      , animationOptions palette model
      , applicationOptions palette model
      ]

lifeOptions : Palette -> Model -> Element Msg
lifeOptions palette model =
  subsectionControl
    { tagger = ToggleShowLifeData
    , checked = model.lifeDataVisible
    , label = "Lives"
    , palette = palette
    }
    [ Input.radio [ padding 10, spacing 2 ]
      { onChange = SelectPointColor
      , selected = Just model.pointColor
      , label = Input.labelAbove [] (text "Color")
      , options =
        [ Input.option LineageColor (text "Lineage")
        , Input.option BirthTimeColor (text "Birth Time")
        , Input.option ChainColor (text "Generation")
        , Input.option CauseOfDeathColor (text "Cause of Death")
        , Input.option AgeColor (text "Age")
        ]
      }
    , Input.radio [ padding 10, spacing 2 ]
      { onChange = SelectPointLocation
      , selected = Just model.pointLocation
      , label = Input.labelAbove [] (text "Location")
      , options =
        [ Input.option BirthLocation (text "Birth")
        , Input.option DeathLocation (text "Death")
        ]
      }
    ]

objectOptions : Palette -> Model -> Element Msg
objectOptions palette model =
  column [ width fill, spacing 10 ]
    [ inverseHeading palette (text "Objects")
    , Input.checkbox [ padding 10, spacing 2 ]
      { onChange = ToggleFadeTallObjects
      , checked = model.fadeTallObjects
      , label = Input.labelRight [ padding 6 ] (text "Fade Tall Objects")
      , icon = Input.defaultCheckbox
      }
    , Input.slider
      [ Background.color palette.control ]
      { onChange = round >> SelectNaturalObjectZoom
      , label = Input.labelAbove [] <|
        row []
          [ text ("Natural Objects > Zoom ")
          , savedSetting
          , text (": " ++ (model.showNaturalObjectsAboveZoom |> String.fromInt))
          ]
      , min = 24
      , max = 32
      , value = model.showNaturalObjectsAboveZoom |> toFloat
      , thumb = Input.defaultThumb
      , step = Just 1
      }
    ]

activityMapOptions : Palette -> Model -> Element Msg
activityMapOptions palette model =
  subsectionControl
    { tagger = ToggleShowActivityMap
    , checked = model.activityMapVisible
    , label = "Activity Map"
    , palette = palette
    }
    [ Input.slider
      [ Background.color palette.control ]
      { onChange = round >> SelectActivityMapSampleSize
      , label = Input.labelAbove [] <|
          text ("Sample Size: " ++ (2^(model.activityMapSampleSize-1) |> String.fromInt) ++ ":1")
      , min = 1
      , max = 9
      , value = model.activityMapSampleSize |> toFloat
      , thumb = Input.defaultThumb
      , step = Just 1
      }
    , Input.slider
      [ Background.color palette.control ]
      { onChange = round >> SelectActivityMapZoom
      , label = Input.labelAbove [] <|
        row []
          [ text ("Show < Zoom ")
          , savedSetting
          , text (": " ++ (model.showActivityMapBelowZoom |> String.fromInt))
          ]
      , min = 23
      , max = 31
      , value = model.showActivityMapBelowZoom |> toFloat
      , thumb = Input.defaultThumb
      , step = Just 1
      }
    ]

animationOptions : Palette -> Model -> Element Msg
animationOptions palette model =
  subsectionControl
    { tagger = ToggleAnimated
    , checked = model.dataAnimated
    , label = "Animated"
    , palette = palette
    }
    [ logSlider
        [ Background.color palette.control ]
        { onChange = round >> GameSecondsPerSecond
        , label = Input.labelAbove [] <|
          text (gameTimeText model.gameSecondsPerSecond)
        , min = 1
        , max = 60*60*10
        , value = model.gameSecondsPerSecond |> toFloat
        , thumb = Input.defaultThumb
        , step = Nothing
        }
    , logSlider
        [ Background.color palette.control ]
        { onChange = round >> FramesPerSecond
        , label = Input.labelAbove [] <|
          text ((model.framesPerSecond |> String.fromInt) ++ " Frames/Second")
        , min = 1
        , max = 60
        , value = model.framesPerSecond |> toFloat
        , thumb = Input.defaultThumb
        , step = Nothing
        }
    ]

monumentOptions : Palette -> Model -> Element Msg
monumentOptions palette model =
  subsectionControl
    { tagger = ToggleShowMonuments
    , checked = model.monumentsVisible
    , label = "Monuments"
    , palette = palette
    }
    [ Input.checkbox [ padding 10, spacing 2 ]
      { onChange = ToggleShowOnlyCurrentMonuments
      , checked = model.showOnlyCurrentMonuments
      , label = Input.labelRight [ padding 6 ] (text "Show Only Current Monuments")
      , icon = Input.defaultCheckbox
      }
    ]

applicationOptions : Palette -> Model -> Element Msg
applicationOptions palette model =
  column [ width fill, spacing 10 ]
    [ inverseHeading palette (text "Application")
    , timeZoneControl model.zone
    , themeControl model.theme
    ]

timeZoneControl : Time.Zone -> Element Msg
timeZoneControl zone =
  Input.radioRow [ padding 10, spacing 20 ]
    { onChange = ToggleUTC
    , selected = Just (zone == Time.utc)
    , label = Input.labelAbove [] (text "Time Zone")
    , options =
      [ Input.option False (text "Local")
      , Input.option True (text "UTC")
      ]
    }

themeControl : Theme -> Element Msg
themeControl theme =
 Input.radioRow [ padding 10, spacing 20 ]
  { onChange = ChangeTheme
  , selected = Just theme
  , label = Input.labelAbove [] (row [] [text "Theme ", savedSetting])
  , options =
    [ Input.option Theme.Dark (text "Dark")
    , Input.option Theme.Light (text "Light")
    ]
  }


subsectionControl :
  { tagger : Bool -> msg
  , checked : Bool
  , label : String
  , palette : Palette
  } -> List (Element msg) -> Element msg
subsectionControl {tagger, checked, label, palette} contents =
  column [ width fill, spacing 10 ]
    ((inverseHeading palette <| Input.checkbox [ spacing 8 ]
      { onChange = tagger
      , checked = checked
      , label = Input.labelRight [] (text label)
      , icon = Input.defaultCheckbox
      })
    :: if checked then contents else []
    )

gameTimeText : Int -> String
gameTimeText totalSeconds =
  let
    hours = totalSeconds // 3600
    minutes = totalSeconds // 60 |> modBy 60
    seconds = totalSeconds |> modBy 60
  in
    [ if hours > 0 then
        Just ((hours |> String.fromInt) ++ "h")
      else
        Nothing
    , if minutes > 0 then
        Just ((minutes |> String.fromInt) ++ "m")
      else
        Nothing
    , if seconds > 0 then
        Just ((seconds |> String.fromInt) ++ "s")
      else
        Nothing
    , Just "Game Time/Second"
  ]
    |> List.filterMap identity
    |> String.join " "

showLoading : Element Msg
showLoading =
  el [ centerX, centerY ] <| text "Loading"

showError : Http.Error -> Element msg
showError error =
  el [ centerX, centerY ] <|
    case error of
      Http.BadUrl url ->
        twoPartMessage
          "Bad Url"
          "This *really* shouldn't happen."
      Http.Timeout ->
        twoPartMessage
          "Timeout"
          "Wondible is cheap and you are getting this for free."
      Http.NetworkError ->
        twoPartMessage
          "Network Error"
          "Either you or the server went offline"
      Http.BadStatus code ->
        case code of
          404 ->
            twoPartMessage
              (code |> String.fromInt)
              "No Data"
          _ ->
            twoPartMessage
              (code |> String.fromInt)
              "Server is being naughty again."
      Http.BadBody body ->
        twoPartMessage
          "Bad Body"
          body

twoPartMessage : String -> String -> Element msg
twoPartMessage header body =
  column [ centerX, centerY ]
    [ el [ centerX, Font.size (scaled 2)] <|
      text header
    , el [ centerX, Font.size (scaled 1)] <|
      text body
    ]

icon : String -> Element msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]
  |> html

savedSetting : Element msg
savedSetting =
  el [htmlAttribute (Html.Attributes.title "This option is saved in your browser")] (icon "lock")

inverseHeading : Palette -> Element msg -> Element msg
inverseHeading palette title =
  column [ width fill ]
    [ el [ height (px 8) ] none
    , el
        [ Font.size (scaled 3)
        , Region.heading 3
        , Font.color palette.background
        , Background.color palette.foreground
        , width fill
        , paddingXY 10 2
        ]
        title
    ]

onChange : (String -> msg) -> Attribute msg
onChange tagger =
  targetValue Json.Decode.string tagger
    |> on "change"
    |> htmlAttribute

targetValue : Json.Decode.Decoder a -> (a -> msg) -> Json.Decode.Decoder msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

onEnter : msg -> Attribute msg
onEnter msg =
  Json.Decode.field "key" Json.Decode.string
    |> Json.Decode.andThen
      (\key ->
        if key == "Enter" then
          Json.Decode.succeed msg
        else
          Json.Decode.fail "Not the enter key"
      )
    |> on "keyup"
    |> htmlAttribute

themeClass : Theme -> Attribute Msg
themeClass theme =
  theme
    |> Theme.toString
    |> Html.Attributes.class
    |> htmlAttribute

themePalette : Theme -> Palette
themePalette theme =
  case theme of
    Theme.Light -> lightTheme
    Theme.Dark -> darkTheme

type alias Palette =
  { foreground: Color
  , background: Color
  , highlight: Color
  , divider: Color
  , control: Color
  , input: Color
  , selected: Color
  , deemphasis: Color
  }

lightTheme : Palette
lightTheme =
  { foreground = rgb 0.1 0.1 0.1
  , background = rgb 0.98 0.98 0.98
  , highlight = rgb 0.8 0.8 0.8
  , divider = rgb 0.7 0.7 0.7
  , control = rgb 0.90 0.90 0.90
  , input = rgb 1.0 1.0 1.0
  , selected = rgb 0.23 0.6 0.98
  , deemphasis = rgb 0.4 0.4 0.4
  }

darkTheme : Palette
darkTheme =
  { foreground = rgb 0.8 0.8 0.8
  , background = rgb 0.1 0.1 0.1
  , highlight = rgb 0.4 0.4 0.4
  , divider = rgb 0.4 0.4 0.4
  , control = rgb 0.2 0.2 0.2
  , input = rgb 0.0 0.0 0.0
  , selected = rgb 0.23 0.6 0.98
  , deemphasis = rgb 0.3 0.3 0.3
  }

objectColor : ObjectId -> Element.Color
objectColor id =
  (id * 3079 |> remainderBy 359 |> toFloat, 100, 50)
    |> SolidColor.fromHSL
    |> SolidColor.toRGB
    |> (\(rf, gf, bf) -> rgb (rf/255) (gf/255) (bf/255))

scaled = modular 16 1.25 >> round
