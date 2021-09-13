## bugs
- x remove band simulation
- remove notable on other servers
- tab switching on life search

- ?? too man calls to location or history api in a short time - time scrolling while object seach is open - cannot reproduce
- ?? when objects search is open and location selected, cannot pan - cannot reproduce

## server work

## Features
- x why does anim toggle need both toggleAnimated if we are calling baseLayerByTime?
- time window range graph
  - x first range select fails
  - x switching data modes
  - x worlds cover more history
  - x what if data doesn't cover whole timeline range
  - x when to dislay min/max time
  - x snap to edges
  - x monuments?
    - x snap, jump to location
  - x separate dragging uncommited drag selection
  - x truncate world name in header
  - full integration
    - x opening timeline on no-anim servers
    - x play button with no animation?
    - x anim remains avaiable with life data from other server
    - x releaseing drag button out of window
    - ?? timeline default selection?
    - ?? change timeline when changing servers?
    - ?? arc control feedback?
  - x lazy background
  - x selection drag not visible
  - other data types
  - live publish range? - server range
  - data hit detection??
- object search header locked
- assembling log search spans?
- in-app help
- town cluster labels
- center view on main area?
- always load certain objects (walls/floors)
- tapout radius
- expired objects
- other monument reset times
- biome graphics
- bookmarking layers
- interpolate to death time and location
- interpolate to child birth locations
- paging

## services
- versions per service
- object data

## code quality
- animated small objects
  - Visible objects are from static base layer - needs to be handled on client
- layer and cache object naming schemes
- refactoring
- better setting of default server
- leaflet handlers - click events?
- pure object layer?


## rendering optimization
- animated view, maybe just object search
- TileKey cache
- find bottleneck - network or render?
- load time garbage collection
- use changing object layers to avoid visual flashing?
- biome caching?
- sprite lods?
- sprite sheets?
- glitch with maxzoom on object pixel layer https://github.com/Leaflet/Leaflet/pull/6310
- floor fade effect
- do we need all four sprite bounds?
- custom layer methods
  - single layer
  - webworker
  - typed arrays allow setting whole palette color at once
- canvas perf
  - render all points of color at once
  - use whole numbers for positions
  - composting mode tricks

## network optimization
- tile not found in anim view? - may be no activity, possible to use ending key?
- Only fetch reasonable amount of data for lives (per column?)
- rate limit object search loading - progressive loading needs rework of tile data loading, it is all one big promise

## other optimzation
- optimize monument updates (most time updates will not cross a time that changes anything)

## Minor bugs
- last arc of random oil shows grid placement in final state
- map resize handling and sidebar
- better lineage color - bright except for problem colors
- better generatoin color - longer on top?
- more accurate biome age times
- generation update right before an update. Span gets assigned to prior world, but the new generation world gets selected as current for the latest time period
  http://localhost:8000/public/index.html#x=-10652&y=124&z=25&s=17
  t=1584061390?

  - x limits for screenshot layers
    - x 218 (electrum) through 232 (259 bear cave)

## release
