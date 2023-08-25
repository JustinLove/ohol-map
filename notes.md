## bugs
x fetch data failed 2023_08August_23_Wednesday 
  Object { error: "BadBody", message: "1,110: looking for newline\n1,110: looking for carriage-newline\n1,110: something other than newline after record" }

- lives on non-map servers
- hard to use on ipad (touch)
- ?? too man calls to location or history api in a short time - time scrolling while object seach is open - cannot reproduce
- ?? when objects search is open and location selected, cannot pan - cannot reproduce

wondible-com-ohol-data-server-1-2022-12-04-04-48
## server work
- last eight hours

## Features
- lifelog death update
https://github.com/jasonrohrer/OneLife/commit/965e1937203467e809db36078ce0e047291e8174
- fix picking search soure data in LifeSearch (cleared when clicking map point, rough patched)
- direct lifelog display
  - remove refs to servers
  - fuzzy name search
  - processed lifelog display?
  - eves only??
  - life search
  - x option to uncap file limit?
  - x ui updates to explain changes
  - life search data types
    - needs data.life for hash
    - needs data.life for comparision with raw data
    - results directly displayed by view, without hash
- x check for leaflet updates

- object search header locked
- animated status when zoom too large for visible layers - still lives
- assembling log search spans?
- in-app help
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
