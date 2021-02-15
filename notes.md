## server work
- default position with no active monuments - precalculation
- server search indexes
  - x load timed logs, how to display
    - assembling spans?
  - x load counts when changing time
  - x counts for maplog data?
    - x should show time range instead of static time
  - x should load during interactive search?
    - x debounced
    - x number limit - map doesn't have any idea what we are doing, just has highlight list
  - interactive span changes
    - x ui
    - x? map
  - select only one a time?
    - x jump to instances?
    - x back/forward
    - x log data
    - x count of items on browse tab
    - x what about server/datatime context? -> spandata?
  - x sort objects by count
  - x exclude probably tutorial in browse
  - x default map position?
    - test if no span data
    - update notable when changing span
      - if no data
  - x special layers for key objects (towers in progress, apocalyse, wells/mines/oil)?
    - x experiment with current data, consider optimizing what works
    - x preprocesed object list - picking objects

## Features
- option to show object search as images
- not selecting arc index because arcs in list have been updated with a datatime, records do not match
- object search by id
- player search by id?
- animation range when going to url timestamp
- time window range graph
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
