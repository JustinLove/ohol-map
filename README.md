# One Hour One Life Map UI

[Leaflet](https://leafletjs.com/) map of the [One Hour One Life](https://onehouronelife.com/) game world, with an Elm sidebar interface for selecting and filtering data overlays. It relies on a separate [data server](https://github.com/JustinLove/ohol-data-server) for life data. This project does not include image tile generation.

## Site Usage

Default view is probably uninteresting. It's showing the current time, but we probably don't have the current map seed available yet.

Coordinates are linkable, and moving the map updates the url for easy copying.

### Top level map controls

Upper left: Zoom in and out buttons. Also works with mouse wheel.
Upper right: Layers control, switch to past maps and toggle data and other overlays.
Lower left: Scale control, also includes current zoom level (eg z24 for zoom 24) A clock button also appears when appropriate to toggle time replay mode.
Lower right: Life search, data selection, data formatting options.

### Layers Control (upper right)

Hover to open panel. There are two sections:

#### Base Layers

Different stages of the games development, (or other community servers) Only one may be selected at a time. Base layers can automatically change when viewing data to the appropriate map for the selected time period.

The "Arc Age" layer will change depending the current time, and may not be blank at the current time.

The "Jungle Age" layer is somewhat special. It was the first map type, and is based on pre-rendered images. There is much higher detail available in a small area around origin from the early days of the limited rift box.

Uncertainty is just blank map, for use at times without a known map seed.

#### Overlays

- Graticule: Grid lines for help with judging distances or locating coordinates. Lower grids are on 40-spacing, which is the spring placement at current settings.

- Rift: Shows the boundary area to which play is currently limited. The rift size should update based on data times, though the size cannot be directly controlled.

- Life Data: Shows the birth or death location of characters based on the games lifelogs. By default it shows the most recent two days, on bigserver2, other servers and times can be selected by opening the data menu (lower right) Changing this layer will automatically toggle the fade overlay for better visibility.

You can select points in the data view to view them in the search result list. Selecting points in the animated layer respects the time setting.

- Monuments: Shows locations of bell towers based on the websites monument log. By default shows bigserver2, it follows the server selection in the data menu (lower right).
- Fade: fade out the base map for better data visibility.

#### Data Menu

The filter/funnel at lower left opens up a data sidebar with three tabs. Note that all data comes from the lifelogs, which are only posted once a day. I fetch them around 9:00 UTC.

##### Search

You may look up a character by name or lifelog hash. A list will be shown along with birth locations on the map.

Select the life to show that position on the map.

Lineage (heads) will change the life list that characters lineage.

Tree will open a new tab with the characters family tree.

##### Data

Filter and select life data for display on the map.

Show: runs the currently selected search. This may be slow, please be patient.

The time in history to show is primarily set by the start or end of the period. This can operate in three modes. Updating the should also set the corresponding map.

- Server: The time selection depends on the currently selected server and when it has been operating. Since these time periods can be very large, it is selected in two stages: "coarse" selects the time over all possible values with very low precision, "fine" can then select a more precise start point within a week to either side.
- Now: Select the most recent data. Due to lifelog delays, there may be no data recent enough, but it is useful for selecting data that starts "24 hours ago" etc.
- Arc: Select the time range of one of the game arcs.

Range control: e.g. "2d Before": Picks the amount of history to be shown.

Server: Picks the game server to retrieve data for. Servers are iconified; smaller 1-15 are server1-server15, larger 1-2 are bigserver1 and bigserver2. As of this writing, most people play on bigserver2, which is the default.

Server control also affects which monuments are shown in the monument layer.

Presets: Offers and easy, bookmarkable, way to configure several settings at once to show "Ambient Yesterday": bigserver2 at same time yesterday, with time advancing 1-1 scale.

##### Format

Data cosmetics for life data points.

Color: several coloring methods
- Lineage: Random color for each family line (color collisions can occur)
- Birth Time
- Generation
- Cause of death

Location: display points at the birth or death location

Animated: Control to toggle whole or animated view. This can also be done from clock button in lower left when data is shown. When enabled, also provides a control for the animation playback scale = history time per animation frame.

Fade Tall Objects: For arc maps with object placement data, slightly fades large objects to reveal things hidden behind them.

#### Time Controls

When the data layer is on, a clock button appears at lower left. This toggles data replay mode and opens a time control. From here you can step, play, pause and scrub through the currently selected data range.

Clicking the date/time text will toggle between UTC and local timezone.

The (X) button will close the time control and end animated replay mode.

## Building

Built using [Elm](http://elm-lang.org/)

My build command:

> elm-make src/MapUI.elm --output public/map-ui.js

`bin/monitor.bat` has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, public/index.html` can be opened locally from the filesystem, or set up for a local or internet server as you wish. (A server may be required for some relative url references).

Tile paths and data server are configured in `public/config.js`

## Credits

Icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))
Object Images based on [based on](https://github.com/JustinLove/onetech/tree/mapping) [Onetech](https://github.com/Kazetsukai/onetech)
