- dark mode
  - persistent
  - search point color
  - slider handles
- clientside object search
- server time selections dont set animation range
- default position with no active monuments - precalculation
- server search indexes

- services
  - versions per service
  - object data
- refactoring

- tapout radius
- expired objects
- time window range graph
- in-app help
- rendering map data
  - find bottleneck - network or render?
  - load time garbage collection
  - use changing object layers to avoid visual flashing?
  - biome caching?
  - only load needed sprites, need drawing ready management
  - objects to show in pixel layer, color table
  - rift
  - sprite lods?
  - sprite sheets?
  - glitch with maxzoom on object pixel layer https://github.com/Leaflet/Leaflet/pull/6310
  - floor fade effect
  - do we need all four sprite bounds?
  - custom layer methods
    - single layer
    - webworker
    - typed arrays allow setting whole palette color at once
- always load certain objects (walls/floors)
- canvas perf
  - render all points of color at once
  - pre-downscale sprites
  - use whole numbers for positions
  - composting mode tricks
- may not go to time with latest monument
- other monument reset times
- optimize monument updates (most time updates will not cross a time that changes anything)
- biome graphics
- last arc of random oil shows grid placement in final state

- bookmarking layers
- sidebar icon toggle
- map resize handling and sidebar
- historical replay
  - Only fetch reasonable amount of data for historical
  - center view on main area?
  - interpolate to death time and location
  - interpolate to child birth locations
- more logatrimthim slider scale?
- better lineage color - bright except for problem colors
- better generatoin color - longer on top?
- better setting of default server
- paging
- different coloration methods
  - account
- grouped heat map
- leaflet handlers - click events?
- more accurate biome age times
- generation update right before an update. Span gets assigned to prior world, but the new generation world gets selected as current for the latest time period
  http://localhost:8000/public/index.html#x=-10652&y=124&z=25&s=17
  t=1584061390?
- rebuild selection in-layer
  - base times where end of log can skip key

  - x limits for screenshot layers
    - x 218 (electrum) through 232 (259 bear cave)

## release

- updated style
