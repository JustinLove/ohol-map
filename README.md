# One Hour One Life Map UI

[Leaflet](https://leafletjs.com/) map of the [One Hour One Life](https://onehouronelife.com/) game world, with an Elm sidebar interface for selecting and filtering data overlays. It relies on a separate [data server](https://github.com/JustinLove/ohol-data-server) for life data. This project does not include tile generation.

## Building

Built using [Elm](http://elm-lang.org/)

My build command:

> elm-make src/MapUI.elm --output public/map-ui.js

`bin/monitor.bat` has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, public/index.html` can be opened locally from the filesystem, or set up for a local or internet server as you wish. (A server may be required for some relative url references).

Tile paths and data server are configured in `public/config.js`

## Credits

Icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))
