"use strict";

;(function() {
  var app = Elm.MapUI.init({flags: oholMapConfig})

  var worlds = []

  var objectLayerOptions = {
    pane: 'objectPane',
    fadeTallObjects: false,
    showNaturalObjectsAboveZoom: 26,
  }

  var actmapSampleSize = 2;
  var actmapMaxZoom = 24;
  var actmapLayerOptions = function(sampleSize) {
    var zoomOffset = Math.max(0, Math.min(22, sampleSize - 1))
    return {
      minNativeZoom: 2+zoomOffset,
      maxNativeZoom: Math.min(31, 24+zoomOffset),
      zoomOffset: -zoomOffset,
      tileSize: Math.pow(2, 8+zoomOffset),
    }
  }

  var barrierRadius = null
  var riftOptions = {
    fill: false,
    color: 'black',
  }
  var rift250 = L.rectangle([[-250,-250], [250,250]], riftOptions)
  var rift354 = L.rectangle([[-354,-354], [354,354]], riftOptions)
  var rift500 = L.rectangle([[-500,-500], [500,500]], riftOptions)
  var rift1000 = L.rectangle([[-1000,-1000], [1000,1000]], riftOptions)
  var riftGone = L.layerGroup([])
  var riftOverlay = L.layerGroup([
    riftGone,
  ])
  var riftHistory = [
    { ms: Date.parse("2019-07-25 20:00:00-05:00"), layer: rift250 , radius: 250 },
    { ms: Date.parse("2019-07-25 23:06:38-05:00"), layer: rift1000, radius: 1000 },
    { ms: Date.parse("2019-07-26 02:00:00-05:00"), layer: rift500, radius: 500 },
    { ms: Date.parse("2019-07-26 17:30:00-05:00"), layer: rift354, radius: 354 },
    { ms: Date.parse("2019-08-13 10:57:00-05:00"), layer: riftGone, radius: null },
    { ms: Date.parse("2019-08-24 16:57:00-05:00"), layer: rift354, radius: 354 },
    { ms: Date.parse("2019-11-08 16:30:00-06:00"), layer: riftGone, radius: null },
  ]

  var barrierObjects = [
    3097,
    3101,
    3105,
    3098,
    3102,
    3106,
    3099,
    3103,
    3107,
    3100,
    3104,
    3108,
  ]

  var specialMapPlacements = [
    {
      msStart: Date.parse("2019-08-02 20:11:00-05:00"),
      x: 0,
      y: 0,
      id: 3112,
    },
    {
      msStart: Date.parse("2024-02-03 14:45:00-05:00"),
      x: 1,
      y: 0,
      id: 4950,
    },
  ]

  var CELL_D = 128
  var objectBoundsPadding = 15
  var objectIDMask = 0x0001FFFF;

  var scale = Math.pow(2, 24)
  var crs = L.extend({}, L.CRS.Simple, {
    transformation: new L.transformation(1/scale, 0.5/scale, -1/scale, -0.5/scale)
  })

  var mapTime = Date.now()
  var mapServer = 17
  var timelineVisible = false
  var dataAnimated = false
  var biomeLayerVisible = false

  var attribution = '<a href="https://onehouronelife.com">Jason Rohrer</a> wondible ' +
    '<a href="https://twitter.com/wondible" title="@wondible"><svg class="icon icon-twitter"><use xlink:href="symbol-defs.svg#icon-twitter"></use></svg></a>' +
    '<a href="https://github.com/JustinLove/ohol-map" title="Frontend: ohol-map"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>' +
    '<a href="https://github.com/JustinLove/ohol-data-server" title="Backend: ohol-data-server"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>' +
    '<a href="https://github.com/JustinLove/OneLife/tree/mapping" title="Tile generation: OneLife/mapping"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>' +
    '<a href="https://github.com/JustinLove/onetech/tree/mapping" title="Object images based on Onetech"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>'
  var baseAttributionLayer = L.layerGroup([], {attribution: attribution})

  var biomeImageLayer = L.tileLayer(oholMapConfig.mainTiles, {
    className: 'crisp biome-image-layer',
    errorTileUrl: 'ground_U.png',
    minZoom: 2,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
  })
  biomeImageLayer.name = 'biome image layer'

  var screenshotImageLayer = L.tileLayer(oholMapConfig.mainTiles, {
    className: 'screenshot-image-layer',
    //errorTileUrl: 'ground_U.png',
    minZoom: 2,
    minZoom: 25,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 29,
    attribution: attribution,
    bounds: [[-512, -512], [511, 511]],
  })
  screenshotImageLayer.name = 'screenshot image layer'

  var base = {};

  var oholBase = L.layerGroup([], {className: 'ohol-base'})
  oholBase.name = 'ohol base'
  //base['OHOL'] = oholBase

  var crucibleBase = L.tileLayer(oholMapConfig.crucibleTiles, {
    className: 'crisp crucible',
    errorTileUrl: 'ground_U.png',
    minZoom: 2,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 25,
    attribution: attribution,
  });
  crucibleBase.name = 'cruicible'
  //base['Crucible'] = crucibleBase

  var dataOverlay = L.layerGroup([], {className: 'data-overlay', pane: 'overlayPane'})
  dataOverlay.name = 'data overlay'
  dataOverlay.on('add', function(ev) {
    var map = ev.target._map
    legendControl.redraw()
    map.addControl(pointLegendControl)
    map.addControl(legendControl)
    setTimeout(function() {
      toggleAnimationControls(map)
      //map.addLayer(baseFade)
    }, 0)
  })
  dataOverlay.on('remove', function(ev) {
    var map = ev.target._map
    map.removeControl(pointLegendControl)
    map.removeControl(legendControl)
    setTimeout(function() {
      toggleAnimationControls(map)
      //map.removeLayer(baseFade)
    })
  })

  var clusterOverlay = L.layerGroup([], {
    className: 'cluster-overlay',
    pane: 'overlayPane',
  })
  clusterOverlay.name = 'cluster overlay'

  var objectOverlay = L.layerGroup([], {className: 'object-overlay'})
  objectOverlay.name = 'object overlay'

  var activityOverlay = L.layerGroup([], {className: 'activity-overlay'})
  activityOverlay.name = 'activity overlay'

  var baseFade = L.layerGroup([], {className: 'base-fade'})
  baseFade.name = 'base fade'
  baseFade.on('add', function(ev) {
    var map = ev.target._map
    L.DomUtil.setOpacity(map.getPane('tilePane'), 0.3)
    L.DomUtil.setOpacity(map.getPane('baseimagePane'), 0.3)
    L.DomUtil.setOpacity(map.getPane('objectPane'), 0.3)
    L.DomUtil.setOpacity(map.getPane('activityPane'), 0.3)
    L.DomUtil.addClass(map.getPane('tilePane'), 'blur')
    L.DomUtil.addClass(map.getPane('baseimagePane'), 'blur')
    L.DomUtil.addClass(map.getPane('objectPane'), 'blur')
    L.DomUtil.addClass(map.getPane('activityPane'), 'blur')
  })
  baseFade.on('remove', function(ev) {
    var map = ev.target._map
    L.DomUtil.setOpacity(map.getPane('tilePane'), 1.0)
    L.DomUtil.setOpacity(map.getPane('baseimagePane'), 1.0)
    L.DomUtil.setOpacity(map.getPane('objectPane'), 1.0)
    L.DomUtil.setOpacity(map.getPane('activityPane'), 1.0)
    L.DomUtil.removeClass(map.getPane('tilePane'), 'blur')
    L.DomUtil.removeClass(map.getPane('baseimagePane'), 'blur')
    L.DomUtil.removeClass(map.getPane('objectPane'), 'blur')
    L.DomUtil.removeClass(map.getPane('activityPane'), 'blur')
  })

  var biomeLayerToggle = L.layerGroup([], {className: 'biome-layer-toggle'})
  biomeLayerToggle.name = 'biome layer toggle'
  biomeLayerToggle.on('add', function(ev) {
    var map = ev.target._map
    if (!biomeLayerVisible) {
      biomeLayerVisible = true
      baseLayerByTime(map, mapTime, "biome layer toggle")
    }
  })
  biomeLayerToggle.on('remove', function(ev) {
    var map = ev.target._map
    if (biomeLayerVisible) {
      biomeLayerVisible = false
      baseLayerByTime(map, mapTime, "biome layer toggle")
    }
  })

  var monumentOverlay = L.layerGroup([], {
    className: 'monument-overlay',
    showOnlyCurrentMonuments: true,
    pane: 'landmarkPane',
  })
  monumentOverlay.name = 'monument overlay'

  var notableOverlay = L.layerGroup([], {
    className: 'notable-overlay',
    pane: 'landmarkPane',
  })
  notableOverlay.name = 'notable overlay'

  var overlays = {
    graticule: null,
    "Rift": riftOverlay,
    "Bands": null,
    "Life Data": dataOverlay,
    "Family Names": clusterOverlay,
    "Object Search": objectOverlay,
    "Fade": baseFade,
    "Monuments": monumentOverlay,
    "Notable": notableOverlay,
    "Activity Map": activityOverlay,
    "Biomes": biomeLayerToggle,
  }

  var lifeSearchOverlay = L.layerGroup([], {className: 'search-overlay'})
  lifeSearchOverlay.name = 'life search overlay'
  var focusMarker = null;

  var updateMonumentLayer = function(layer, data) {
    loadIcons([{id: 835}])
    var now = new Date()
    if (data) {
      L.Util.setOptions(layer, {data: data})
    } else {
      data = layer.options.data || [];
    }
    layer.clearLayers()
    var monumentIcon = icons[835] // TODO: defer until icon available
    data.forEach(function(point) {
      var date = new Date(point.date*1000)
      var end = point.end ? new Date(point.end*1000) : now
      var age = Math.abs(now - date) / (24 * 60 * 60 * 1000)
      var props = {
        opacity: Math.max(0.4, Math.min(1.0, 1.0 - (age / (age+30)))),
        pane: 'landmarkPane',
      }
      // leaflet crashes if icon is present but undefined
      if (monumentIcon) props.icon = monumentIcon
      point.monument = L.marker([point.y, point.x], props)
        .bindPopup(date.toString())
        .addTo(layer)
      //L.circle([point.y, point.x], {radius: 21000, fill: false}).addTo(layer)
    })
  }

  var nameIcons = {}

  var updateClusterLayer = function(layer, data, zoom) {
    var lineages = legendControl.getLineages()
    var points = {}
    if (data) {
      L.Util.setOptions(layer, {data: data})
    } else {
      data = layer.options.data || [];
    }
    layer.clearLayers()
    for (var lineageId in data) {
      var life = lineages[lineageId]
      var name = lineageId
      if (life && life.name) {
        var words = life.name.split(' ')
        name = (words[1] || 'unnamed')
      }
      data[lineageId].forEach(function(point) {
        var key = point.x.toString()+","+point.y.toString()
        var offset = points[key] || 0
        points[key] = offset + 1

        var iconKey = name + offset
        if (!nameIcons[iconKey]) {
          nameIcons[iconKey] = L.divIcon({
            html: "<div>"+name+"</div>",// + " " + point.members.toString(),
            className: 'cluster-label',
            //iconSize: [12, 12],
            iconAnchor: [-12, 12 + 12 + (offset * 12)],
          })
        }

        point.marker = L.marker([point.y + offset, point.x], {
            icon: nameIcons[iconKey],
            pane: 'overlayPane',
            interactive: false,
          })
          //.bindPopup(lineageId.toString())
          .addTo(layer)
        //L.circle([point.y, point.x], {radius: 200, fill: false}).addTo(layer)
      })
    }
  }

  var monumentsByTime = function(layer, ms, reason) {
    var now = new Date()
    ms = ms || now
    var data = layer.options.data || [];
    //console.log('updating monuments', ms, data, reason)
    data.forEach(function(point) {
      var date = new Date(point.date*1000)
      var end = point.end ? new Date(point.end*1000) : ms
      if (!layer.options.showOnlyCurrentMonuments || (date <= ms && ms <= end)) {
        layer.addLayer(point.monument)
      } else {
        layer.removeLayer(point.monument)
      }
    })
  }

  var icons = []
  var iconScale = 1/8

  var loadIcons = function(placements) {
    if (objectBounds.length < 1) return
    placements.forEach(function(placement) {
      var id = placement.id
      if (id != 0 && !icons[id]) {
        var bounds= objectBounds[id]
        if (!bounds) return;
        var iw = (bounds[2] - bounds[0]) * iconScale
        var ih = (bounds[3] - bounds[1]) * iconScale
        var ix = -bounds[0] * iconScale
        var iy = bounds[3] * iconScale
        var icon = L.icon({
          iconUrl: 'static/sprites/obj_'+id+'.png',
          iconSize: [iw, ih],
          iconAnchor: [ix, iy]
        })
        icons[placement.id] = icon
      }
    })
  }

  var updateNotableLayer = function(layer, data) {
    loadIcons(data)
    var now = new Date()
    if (data) {
      L.Util.setOptions(layer, {data: data})
    } else {
      data = layer.options.data || [];
    }
    layer.clearLayers()
    data.forEach(function(point) {
      point.monument = L.marker([point.y, point.x], {
          icon: icons[point.id],
          pane: 'landmarkPane',
          interactive: false,
        })
        .addTo(layer)
    })
  }

  L.LayerGroup.ObjectImageLayer = L.LayerGroup.extend({
    options: {
      className: 'object-image-overlay-static',
      pane: 'overlayPane',
    },
    initialize: function(cache, options) {
      this._layers = {};
      this._cache = cache;
      options = L.Util.setOptions(this, options);
    },
    redraw: function() {
      var layer = this
      var options = layer.options
      var highlightObjects = options.highlightObjectImages
      layer.clearLayers()
      if (!highlightObjects) {
        return
      }
      layer._cache.loadTile({}, {time: options.dataTime, server: mapServer, highlightObjects: highlightObjects}).then(function(data) {
        loadIcons(data)
        data.forEach(function(point) {
          point.monument = L.marker([point.y, point.x], {
              icon: icons[point.id],
              pane: 'overlayPane',
            })
            .addTo(layer)
        })
      })
    },
  })

  var updatePlacementLayer = function(layer, data) {
    layer.clearLayers()
    L.Util.setOptions(layer, {data: data})
    data.forEach(function(point) {
      L.marker([point.y, point.x], {})
        .bindPopup(point.id.toString())
        .addTo(layer)
      //L.circle([point.y, point.x], {radius: 21000, fill: false}).addTo(layer)
    })
  }

  L.GridLayer.CheckerLayer = L.GridLayer.extend({
    options: {
      name: 'checker layer',
      className: 'checker-layer',
      pane: 'overlayPane',
    },
    createTile: function (coords, done) {
      var tile = document.createElement('canvas');
      var tileSize = this.getTileSize();
      tile.setAttribute('width', tileSize.x);
      tile.setAttribute('height', tileSize.y);

      setTimeout(this.drawTile.bind(this), 0, tile, coords, done)

      return tile;
    },
    drawTile(tile, coords, done) {
      var tileSize = this.getTileSize();

      var ctx = tile.getContext('2d');
      ctx.globalAlpha = 0.3
      if ((coords.x + coords.y) % 2) {
        ctx.fillStyle = 'white'
      } else {
        ctx.fillStyle = 'black'
      }
      ctx.fillRect(0, 0, tile.width, tile.height)

      if ((coords.x + coords.y) % 2) {
        ctx.fillStyle = 'black'
      } else {
        ctx.fillStyle = 'white'
      }
      var height = tile.width/5
      ctx.font = height + 'px sans-serif'
      var text = coords.x + ',' + coords.y
      var metrics = ctx.measureText(text)

      ctx.fillText(text, (tile.width - metrics.width)/2, (tile.height + height)/2)
      done(null, tile)
    },
  })

  //overlays['Checker'] = new L.GridLayer.CheckerLayer()


  // fractal generation copying https://github.com/jasonrohrer/OneLife/blob/master/commonSource/fractalNoise.cpp
  // which cites https://bitbucket.org/runevision/random-numbers-testing/

  var XX_PRIME32_1 = 2654435761
  var XX_PRIME32_2 = 2246822519
  var XX_PRIME32_3 = 3266489917
  //var XX_PRIME32_4 = 668265263
  var XX_PRIME32_5 = 374761393

  var xxSeedA = 0
  var xxSeedB = 0

  var hex = function(x) {
    if (x < 0) {
      return (-x>>16 ^ 0xffff).toString(16).padStart(4, '0') + (x&0xffff).toString(16).padStart(4, '0') + ' ' + x
    } else {
      return x.toString(16).padStart(8, '0') + ' ' + x
    }
  }

  var xxTweakedHash2D = function(inX, inY) {
    var h32 = xxSeedA + inX + XX_PRIME32_5
    h32 += Math.imul(inY, XX_PRIME32_3)
    h32 = Math.imul(h32, XX_PRIME32_2)
    h32 ^= h32 >>> 13
    h32 += xxSeedB
    h32 = Math.imul(h32, XX_PRIME32_3)
    h32 ^= h32 >>> 16
    h32 >>>= 0
    return h32
  }

  var getXYRandomBN = function(inX, inY) {
    var floorX = inX + (inX < 0 ? -1 : 0) >> 0
    var ceilX = floorX + 1
    var floorY = inY + (inY < 0 ? -1 : 0) >> 0
    var ceilY = floorY + 1

    var cornerA1 = xxTweakedHash2D(floorX, floorY)
    var cornerA2 = xxTweakedHash2D(ceilX, floorY)

    var cornerB1 = xxTweakedHash2D(floorX, ceilY)
    var cornerB2 = xxTweakedHash2D(ceilX, ceilY)

    var xOffset = inX - floorX
    var yOffset = inY - floorY

    var topBlend = cornerA2 * xOffset + (1-xOffset) * cornerA1
    var bottomBlend = cornerB2 * xOffset + (1-xOffset) * cornerB1

    return bottomBlend * yOffset + (1-yOffset) * topBlend
  }

  var oneOverIntMax = 1.0 / 4294967295;
  var invMAXPlusOne = 1.0 / (4294967295 + 1);

  var getXYRandom = function(inX, inY) {
    return xxTweakedHash2D(inX, inY) * oneOverIntMax
  }

  var getXYFractal = function(inX, inY, inRoughness, inScale) {
    var b = inRoughness
    var a = 1 - b

    var sum =
      a * getXYRandomBN(inX / (32 * inScale), inY / (32 * inScale))
      +
      b * (
        a * getXYRandomBN(inX / (16 * inScale), inY / (16 * inScale))
        +
        b * (
          a * getXYRandomBN(inX / (8 * inScale), inY / (8 * inScale))
          +
          b * (
            a * getXYRandomBN(inX / (4 * inScale), inY / (4 * inScale))
            +
            b * (
              a * getXYRandomBN(inX / (2 * inScale), inY / (2 * inScale))
              +
              b * (
                getXYRandomBN(inX / inScale, inY / inScale)
              )
            )
          )
        )
      )

    return sum * oneOverIntMax
  }

  // implemented based on Jason Rohrer
  // https://github.com/jasonrohrer/minorGems/blob/master/util/random/CustomRandomSource.h
  // https://github.com/jasonrohrer/minorGems/blob/master/util/random/RandSource32.h
  var CustomRandomSource = function(state) {
    this.state = state
  }

  var genRand32 = function(state) {
    //console.log('i', state)
    var custnum1 = Math.imul( state            , 0xFEA09B9D) + 1
    var custnum2 = Math.imul((state ^ custnum1), 0x9C129511) + 1
    //console.log('2', custnum2 >>> 0)
    var custnum3 = Math.imul( state            , 0x2512CFB8) + 1
    var custnum4 = Math.imul((state ^ custnum3), 0xB89C8895) + 1
    //console.log('4', custnum4 >>> 0)
    var custnum5 = Math.imul( state            , 0x6BF962C1) + 1
    var custnum6 = Math.imul((state ^ custnum5), 0x4BF962C1) + 1
    //console.log('6', custnum6 >>> 0)
    var out = (custnum2 ^ (custnum4 >>> 11) ^ (custnum6 >>> 22)) >>> 0
    //console.log('o', out)
    return out
  }

  CustomRandomSource.prototype.genRand32 = function() {
    this.state = genRand32(this.state)
    return this.state
  }

  CustomRandomSource.prototype.getRandomBoundedInt = function(inRangeStart, inRangeEnd) {
    var randFloat = this.genRand32() * invMAXPlusOne
    var onePastRange = inRangeEnd + 1
    var magnitude = (randFloat * (onePastRange - inRangeStart))>>>0
    //console.log(randFloat, onePastRange, magnitude, inRangeStart + magnitude)
    return inRangeStart + magnitude
  }

  var versions = []
  var objectBounds = []
  var objectSize = []

  var competeMapBiomeIndex = function(inX, inY, options, secondPlace) {
    var maxValue = -Number.MAX_VALUE
    var pickedBiome = -1
    var secondPlaceBiome = -1
    var secondPlaceGap = 0
    var numBiomes = options.biomeMap.length
    var scale = options.biomeOffset + options.biomeScale * numBiomes
    var roughness = options.biomeFractalRoughness
    for (var i = 0;i < numBiomes;i++) {
      var biome = options.biomeMap[i]

      xxSeedA = biome * options.biomeSeedScale + options.biomeRandSeedA
      xxSeedB = options.biomeRandSeedB
      var randVal = getXYFractal(inX, inY, roughness, scale)

      if (randVal > maxValue) {
        secondPlaceBiome = pickedBiome
        secondPlaceGap = randVal - maxValue

        maxValue = randVal
        pickedBiome = i
      } else if (randVal > maxValue - secondPlaceGap) {
        secondPlaceBiome = i
        secondPlaceGap = maxValue - randVal
      }
    }

    if (secondPlace) {
      secondPlace.biome = secondPlaceBiome
      secondPlace.gap = secondPlaceGap
    }

    return pickedBiome
  }

  var getSpecialBiomeIndexForYBand = function(inY, options) {
    var radius = (options.specialBiomeBandThickness / 2) >>> 0
    for(var i = 0;i < options.specialBiomeBandOrder.length;i++) {
      var yCenter = options.specialBiomeBandYCenter[i]
      if (Math.abs(inY - yCenter) <= radius) {
        return options.specialBiomeBandIndexOrder[i]
      }
    }
    return options.specialBiomeBandDefaultIndex
  }

  var topographicMapBiomeIndex = function(inX, inY, options, secondPlace) {
    var numBiomes = options.biomeMap.length
    var regularBiomesLimit = numBiomes - options.numSpecialBiomes
    var scale = options.biomeOffset + options.biomeScale * numBiomes
    var roughness = options.biomeFractalRoughness
    var weights = options.biomeCumuWeights

    xxSeedA = options.biomeRandSeedA
    xxSeedB = options.biomeRandSeedB
    var randVal = getXYFractal(inX, inY, roughness, scale)
    //console.log('initial', randVal)

    randVal -= 0.099668
    randVal *= 1.268963
    //console.log('scaled', randVal)

    var i = randVal * options.biomeTotalWeight
    //console.log('weighted', i)

    var pickedBiome = 0
    while (pickedBiome < numBiomes-1 &&
           i > weights[pickedBiome]) {
      pickedBiome++
    }
    //console.log('picked', pickedBiome)

    if (pickedBiome >= regularBiomesLimit) {
      if (options.specialBiomeBandOrder.length > 0) {
        pickedBiome = getSpecialBiomeIndexForYBand(inY, options)
        secondPlaceBiome = regularBiomesLimit - 1
      } else {
        pickedBiome = -1
        scale = options.biomeSpecialOffset + options.biomeSpecialScale * options.numSpecialBiomes
        roughness = options.biomeSpecialRoughness

        var maxValue = -10
        var secondMaxValue = -10
        var secondPlaceBiome = -1

        for (var i = regularBiomesLimit;i < numBiomes;i++) {
          var biome = options.biomeMap[i]

          xxSeedA = biome * options.biomeSeedScale + options.biomeRandSeedA + options.biomeSeedSpecialOffset
          xxSeedB = options.biomeRandSeedB

          var randVal = getXYFractal(inX, inY, roughness, scale)

          if (randVal > maxValue) {
            if (maxValue != -10) {
              secondMaxValue = maxValue
            }
            maxValue = randVal
            pickedBiome = i
          }
        }

        if (maxValue - secondMaxValue < options.biomeSpecialBoundary) {
          secondPlaceBiome = pickedBiome
          pickedBiome = regularBiomesLimit - 1
        } else {
          secondPlaceBiome = regularBiomesLimit - 1
        }
      }
    } else {
      secondPlaceBiome = pickedBiome - 1
      if (secondPlaceBiome < 0) {
        secondPlaceBiome = pickedBiome + 1
      }
    }

    if (secondPlace) {
      switch (options.secondPlaceBiomeObjects) {
        case 'SecondPlaceObjects':
        case 'SecondPlaceStaticObjects':
          secondPlace.biome = secondPlaceBiome
          secondPlace.gap = 0.1
          break;
        case 'NoMovingObjects':
          secondPlace.biome = pickedBiome
          secondPlace.gap = 1.0
          break;
        case 'NoSecondPlace':
          secondPlace.biome = secondPlaceBiome
          secondPlace.gap = 10.0
          break;
      }
    }

    return pickedBiome
  }

  var computeMapBiomeIndexFunctions = {
    "competeMapBiomeIndex": competeMapBiomeIndex,
    "topographicMapBiomeIndex": topographicMapBiomeIndex,
  }

  // inKnee in 0..inf, smaller values make harder knees
  // intput in 0..1
  // output in 0..1

  // from Simplest AI trick in the book:
  // Normalized Tunable SIgmoid Function 
  // Dino Dini, GDC 2013
  var sigmoid = function(inInput, inKnee) {
    // change in to -1..1
    var shiftedInput = inInput * 2 - 1

    var k = -1 - inKnee

    var out = shiftedInput * k / (1 + k - Math.abs(shiftedInput))

    return (out + 1) * 0.5
  }

  var getBaseMap = function(inX, inY, options, grid) {

    var pickedBiome = -1
    var secondPlace = {}

    // grid objects
    for (var i = 0;i < options.gridPlacements.length;i++) {
      var object = options.gridPlacements[i]
      var gp = object.gridPlacement
      if ((inX + gp.phaseX) % gp.spacingX == 0 && (inY + gp.phaseY) % gp.spacingY == 0) {
        pickedBiome = options.computeMapBiomeIndex(inX, inY, options, secondPlace)
        if (pickedBiome == -1) {
          return 0;
        }

        if (object.biomes.indexOf(options.biomeMap[pickedBiome]) != -1) {
          if (grid) grid.grid = true
          return object.id
        }
      }
    }

    xxSeedA = options.densitySeed
    xxSeedB = 0
    var density = getXYFractal(inX, inY, options.densityRoughness, options.densityScale);
    density = sigmoid(density, options.densitySmoothness)
    density *= options.density

    xxSeedA = options.presentSeed
    xxSeedB = 0
    if (getXYRandom(inX, inY) >= density) {
      return 0
    }

    if (pickedBiome == -1) {
      pickedBiome = options.computeMapBiomeIndex(inX, inY, options, secondPlace)
    }

    if (pickedBiome == -1) {
      return 0;
    }

    // second place check
    xxSeedA = options.secondPlaceSeed
    xxSeedB = 0
    var firstPlaceChance = options.secondPlaceOffset + options.secondPlaceScale * secondPlace.gap
    if (getXYRandom(inX, inY) > firstPlaceChance) {
      pickedBiome = secondPlace.biome
    }

    var biome = options.biomes[options.biomeMap[pickedBiome]]
    if (!biome) return 0
    var biomeObjects = biome.objects
    var numObjects = biomeObjects.length

    // jackpot chance
    var specialObjectIndex = -1
    var maxValue = -Number.MAX_VALUE

    var roughness = options.jackpotRoughness
    var scale = options.jackpotOffset + numObjects * options.jackpotScale

    for (var i = 0;i < numObjects;i++) {
      xxSeedA = options.jackpotSeedOffset + i * options.jackpotSeedScale
      xxSeedB = 0
      var randVal = getXYFractal( inX, inY, roughness, scale)

      if (randVal > maxValue) {
        maxValue = randVal
        specialObjectIndex = i
      }
    }

    var oldSpecialChance = biomeObjects[specialObjectIndex].mapChance
    var newSpecialChance = oldSpecialChance * 10
    biomeObjects[specialObjectIndex].mapChance = newSpecialChance
    var totalChance = biome.totalChanceWeight - oldSpecialChance + newSpecialChance

    /*
    console.log('special object',
      specialObjectIndex,
      numObjects,
      biome.totalChanceWeight,
      totalChance)
      */

    // weighted object pick
    xxSeedA = options.objectSeed
    xxSeedB = 0
    var randValue = getXYRandom(inX, inY) * totalChance

    var i = 0
    var weightSum = 0

    while (weightSum < randValue && i < numObjects) {
      weightSum += biomeObjects[i].mapChance
      i++
    }

    i--

    // fix jackpot chance
    biomeObjects[specialObjectIndex].mapChance = oldSpecialChance

    if (i < 0) {
      return 0
    }

    var returnId = biomeObjects[i].id

    //console.log('rand', randValue, i, returnId)

    // eliminate off-biome moving objects
    if (options.secondPlaceBiomeObjects != 'SecondPlaceObjects' && pickedBiome == secondPlace.biome) {
      if (biomeObjects[i].moving) {
        return 0
      }
    }

    return returnId
  }

  var getMapObjectRaw = function(inX, inY, options) {
    return getPossibleBarrier(inX, inY, options) ||
      getTweakedBaseMap(inX, inY, options)
  }

  var getPossibleBarrier = function(inX, inY, options) {
    if (barrierRadius == null) return 0

    if (inX == barrierRadius ||
      inX == -barrierRadius ||
      inY == barrierRadius ||
      inY == -barrierRadius) {
      if (-barrierRadius <= inX && inX <= barrierRadius
       && -barrierRadius <= inY && inY <= barrierRadius) {
        xxSeedA = options.barrierSeed
        xxSeedB = 0
        var numOptions = barrierObjects.length
        if (numOptions < 1) return 0

        var pick = Math.floor(numOptions * getXYRandom(inX*10, inY*10))
        if (pick >= numOptions) {
          pick = numOptions - 1
        }

        return barrierObjects[pick]
      }
    }

    return 0
  }

  var getTweakedBaseMap = function(inX, inY, options) {
    var grid = {grid: false}
    var result = getBaseMap(inX, inY, options, grid)
    if (result <= 0) {
      return 0
    }

    var o = options.objects[result]
    if (o && o.wide) {
      for (var dx = -(o.leftBlockingRadius + 1); dx <= (o.rightBlockingRadius + 1); dx++) {
        if (dx == 0) continue
        var nid = getBaseMap(inX + dx, inY, options)
        var no = options.objects[nid]
        if (!no || !no.wide) continue
        if (dx < 0) {
          var minDist = no.rightBlockingRadius + o.leftBlockingRadius
          var dist = -dx
        } else {
          var minDist = no.leftBlockingRadius + o.rightBlockingRadius
          var dist = dx
        }
        if (dist <= minDist) {
          return 0
        }
      }
    } else if (!grid.grid && objectBounds[result][3]-objectBoundsPadding < options.smallHeight) {
      var sid = getBaseMap(inX, inY - 1, options)
      if (sid > 0 && objectBounds[sid][3]-objectBoundsPadding >= options.tallHeight) {
        //console.log('tall blocked')
        return 0
      }
      var s2id = getBaseMap(inX, inY - 2, options)
      if (s2id > 0 && objectBounds[s2id][3]-objectBoundsPadding >= options.veryTallHeight) {
        //console.log('very tall blocked')
        return 0
      }
    }

    return result
  }

  L.GridLayer.FractalLayer = L.GridLayer.extend({
    options: {
      biomeOffset: 0.83332,
      biomeScale: 0.08333,
      biomeFractalRoughness: 0.55,
      seed: 0 * 263 + 723,
      minZoom: 2,
      maxZoom: 31,
      //minNativeZoom: 24,
      maxNativeZoom: 24,
      attribution: attribution,
    },
    createTile: function (coords) {
      var tile = document.createElement('canvas');
      var tileSize = this.getTileSize();
      tile.setAttribute('width', tileSize.x);
      tile.setAttribute('height', tileSize.y);

      this.drawTile(tile, coords)

      return tile;
    },
    drawTile(tile, coords) {
      var tileSize = this.getTileSize();

      var ctx = tile.getContext('2d', {alpha: false});
      ctx.clearRect(0, 0, tile.width, tile.height)

      var pnw = L.point(coords.x * tileSize.x, coords.y * tileSize.y)
      var llnw = crs.pointToLatLng(pnw, coords.z)

      var stride = Math.pow(2, 24 - coords.z)
      var w = tile.width
      var h = tile.height
      var startX = llnw.lng + 0.5
      var startY = llnw.lat - 0.5

      var scale = this.options.biomeOffset + this.options.biomeScale * 7
      var roughness = this.options.biomeFractalRoughness

      var imageData = ctx.createImageData(tile.width, tile.height)
      var d = imageData.data

      xxSeed = this.options.seed

      for (var y = 0;y < h;y++) {
        for (var x = 0;x < w;x++) {
          var i = (y * w + x) * 4
          var wx = startX + x*stride
          var wy = startY - (y*stride)
          var v = getXYFractal(wx, wy, roughness, scale)
          d[i+0] = v * 256
          d[i+1] = v * 256
          d[i+2] = v * 256
          d[i+3] = 255
        }
      }

      ctx.putImageData(imageData, 0, 0)
    },
  })

  /*
  base['Fractal'] = new L.GridLayer.FractalLayer({})

  base['Density'] = new L.GridLayer.FractalLayer({
    biomeOffset: 0.25,
    biomeScale: 0,
    biomeFractalRoughness: 0.1,
    seed: 5379,
  })
  */

  //http://axonflux.com/handy-rgb-to-hsl-and-rgb-to-hsv-color-model-c
  /**
  * Converts an HSV color value to RGB. Conversion formula
  * adapted from http://en.wikipedia.org/wiki/HSV_color_space.
  * Assumes h, s, and v are contained in the set [0, 1] and
  * returns r, g, and b in the set [0, 255].
  *
  * @param   Number  h       The hue
  * @param   Number  s       The saturation
  * @param   Number  v       The value
  * @return  Array           The RGB representation
  */
  function hsvToRgb(h, s, v){
    var r, g, b;

    var i = Math.floor(h * 6);
    var f = h * 6 - i;
    var p = v * (1 - s);
    var q = v * (1 - f * s);
    var t = v * (1 - (1 - f) * s);

    switch(i % 6){
      case 0: r = v, g = t, b = p; break;
      case 1: r = q, g = v, b = p; break;
      case 2: r = p, g = v, b = t; break;
      case 3: r = p, g = q, b = v; break;
      case 4: r = t, g = p, b = v; break;
      case 5: r = v, g = p, b = q; break;
    }

    return [r * 255, g * 255, b * 255];
  }

  function rgbToString(color) {
    return 'rgb(' + color[0] + ',' + color[1] + ',' + color[2] + ')'
  }

  var jungleBiomeMap = [
    0,
    3,
    4,
    5,
    2,
    1,
    6,
  ]
  var desertBiomeMap = [
    0,
    3,
    4,
    5,
    2,
    1,
  ]
  var arcticBiomeMap = [
    0,
    3,
    4,
    2,
    1,
  ]
  var badlandsBiomeMap = [
    0,
    3,
    2,
    1,
  ]

  var topographicBiomeMap = [
    1,
    0,
    2,
    6,
    5,
    3,
    4,
  ]

  var topographicBiomeWeights = [
    0.32,
    0.11,
    0.08,
    0.05,
    0.05,
    0.13,
    0.25,
  ]

  var topographicBiomeTotalWeight = 0
  var topographicBiomeCumuWeights = []
  topographicBiomeWeights.forEach(function(weight, i) {
    topographicBiomeTotalWeight += weight
    topographicBiomeCumuWeights[i] = topographicBiomeTotalWeight
  })

  var specialBiomeMap = [
    1,
    0,
    2,
    3,
    6,
    5,
    4,
  ]

  var specialBiomeWeights = [
    0.32,
    0.12,
    0.09,
    0.11,
    0.11,
    0.11,
    0.13,
  ]

  var specialBiomeTotalWeight = 0
  var specialBiomeCumuWeights = []
  specialBiomeWeights.forEach(function(weight, i) {
    specialBiomeTotalWeight += weight
    specialBiomeCumuWeights[i] = specialBiomeTotalWeight
  })

  var greenColor = hsvToRgb(89/360, 0.49, 0.67)
  var swampColor = hsvToRgb(253/360, 0.17, 0.65)
  var plainsColor = hsvToRgb(36/360, 0.75, 0.90)
  var badlandsColor = hsvToRgb(40/360, 0.16, 0.36)
  var arcticColor = hsvToRgb(0/360, 0.00, 1.0)
  var desertColor = hsvToRgb(37/360, 0.65, 0.62)
  var jungleColor = hsvToRgb(90/360, 0.87, 0.48)
  var tholWaterColor = hsvToRgb(210/360, 0.52, 0.80)

  var Infinity = 1/0
  var bandRectangles = function(bands) {
    var top = Infinity
    var bottom = Infinity
    return L.layerGroup(bands.map(function(band) {
      top = Math.min(bottom, band.center+(bandThickness/2)+0.5)
      bottom = band.center-(bandThickness/2)-0.5
      return L.rectangle([
        [top,-bandWidth],
        [bottom,bandWidth]
      ], {color: rgbToString(band.color), interactive: false})
    }))
  }

  var bandWidth = Math.pow(2, 31)
  var bandThickness = 200
  var band200 = bandRectangles([
    {center: 300, color: arcticColor},
    {center: 100, color: badlandsColor},
    {center: -100, color: jungleColor},
    {center: -300, color: desertColor},
  ])
  var band200plus20 = bandRectangles([
    {center: 320, color: arcticColor},
    {center: 120, color: badlandsColor},
    {center: -80, color: jungleColor},
    {center: -282, color: desertColor},
  ])
  var bandGone = L.layerGroup([])
  var bandsOverlay = L.layerGroup([ bandGone ])
  var bandHistory = [
    { ms: Date.parse("2020-10-29 13:54:08-05:00"), layer: band200 },
    { ms: Date.parse("2020-12-19 14:59:55-06:00"), layer: band200plus20 },
  ]

  overlays['Bands'] = bandsOverlay

  var biomeGenerationOptions = {
    computeMapBiomeIndex: competeMapBiomeIndex,
    biomeOffset: 0.83332,
    biomeScale: 0.08333,
    biomeFractalRoughness: 0.55,
    biomeRandSeedA: 723, // now 727
    biomeRandSeedB: 0, // now 941
    biomeSeedScale: 263,
    biomeMap: jungleBiomeMap,
    numSpecialBiomes: 0,
    specialBiomeBandThickess: 0,
    specialBiomeBandOrder: [],
    specialBiomeBandIndexOrder: [],
    specialBiomeBandDefaultIndex: 1,
    biomeSeedSpecialOffset: 38475,
    biomeSpecialOffset: 2.4999,
    biomeSpecialScale: 0.2499,
    biomeSpecialRoughness: 0.55,
    biomeSpecialBoundary: 0.03,
    biomeTotalWeight: topographicBiomeTotalWeight,
    biomeCumuWeights: topographicBiomeCumuWeights,
  }

  L.GridLayer.BiomeLayer = L.GridLayer.extend({
    options: Object.assign({
      name: 'biome layer',
      className: 'crisp biome-layer',
      biomeColors: [
        greenColor,
        swampColor,
        plainsColor,
        badlandsColor,
        arcticColor,
        desertColor,
        jungleColor,
        tholWaterColor,
      ],
      minZoom: 2,
      maxZoom: 31,
      //minNativeZoom: 24,
      maxNativeZoom: 24,
      attribution: attribution,
    }, biomeGenerationOptions),
    createTile: function (coords, done) {
      var tile = document.createElement('canvas');
      var tileSize = this.getTileSize();
      //console.log(tileSize)
      tile.setAttribute('width', tileSize.x);
      tile.setAttribute('height', tileSize.y);

      setTimeout(this.drawTile.bind(this), 0, tile, coords, done)

      return tile;
    },
    drawTile(tile, coords, done) {
      var tileSize = this.getTileSize();

      var ctx = tile.getContext('2d', {alpha: false});
      ctx.clearRect(0, 0, tile.width, tile.height)

      var pnw = L.point(coords.x * tileSize.x, coords.y * tileSize.y)
      //console.log(coords, pnw)
      var llnw = crs.pointToLatLng(pnw, coords.z)
      //console.log(coords, llnw)

      var stride = Math.pow(2, 24 - coords.z)
      var w = tile.width
      var h = tile.height
      var startX = llnw.lng + 0.5
      var startY = llnw.lat - 0.5

      //console.log(coords, startX, startY)

      var scale = this.options.biomeOffset + this.options.biomeScale * 7
      var roughness = this.options.biomeFractalRoughness

      var biomeMap = this.options.biomeMap
      var colors = this.options.biomeColors
      var imageData = ctx.createImageData(tile.width, tile.height)
      var d = imageData.data

      for (var y = 0;y < h;y++) {
        for (var x = 0;x < w;x++) {
          var i = (y * w + x) * 4
          var wx = startX + x*stride
          var wy = startY - (y*stride)
          var bi = this.options.computeMapBiomeIndex(wx, wy, this.options)
          var v = biomeMap[bi]
          d[i+0] = colors[v][0]
          d[i+1] = colors[v][1]
          d[i+2] = colors[v][2]
          d[i+3] = 255
        }
      }

      ctx.putImageData(imageData, 0, 0)
      done(null, tile)
    },
  })

  var objectGenerationOptions = Object.assign({
    biomes: [],
    gridPlacements: [],
    randPlacements: [],
    placements: [],
    objects: [],
    randSeed: 124567,
    gridSeed: 9753,
    densitySeed: 5379,
    densityRoughness: 0.1,
    densityScale: 0.25,
    densitySmoothness: 0.1,
    density: 0.4,
    presentSeed: 9877,
    objectSeed: 4593873,
    secondPlaceOffset: 0.5,
    secondPlaceScale: 10,
    secondPlaceSeed: 348763,
    jackpotSeedOffset: 123,
    jackpotSeedScale: 793,
    jackpotRoughness: 0.3,
    jackpotOffset: 0.15,
    jackpotScale: 0.016666,
    barrierSeed: 9238597,
    smallHeight: CELL_D, // height which is blocked
    tallHeight: 2, // heights that block
    veryTallHeight: 3,
    secondPlaceBiomeObjects: 'SecondPlaceStaticObjects',
  }, biomeGenerationOptions)

  L.GridLayer.SpriteLayer = L.GridLayer.extend({
    options: {
      offset: 0,
      supersample: 1,
      fadeTallobjects: false,
      minZoom: 24,
      maxZoom: 31,
      //minNativeZoom: 24,
      maxNativeZoom: 31,
      attribution: '<a href="https://onetech.info" title="Object generated by OneTech">OneTech</a> ' +
    '<a href="https://github.com/Kazetsukai/onetech" title="OneTech"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>',
    },
    drawTile(tile, coords, done) {
      var cellSize = Math.pow(2, coords.z - (24 - this.options.supersample))
      var fadeTallObjects = this.options.fadeTallObjects
      var offset = this.options.offset
      //console.log(cellSize, tile._keyplace)

      var ctx = tile.getContext('2d', {alpha: true});
      ctx.clearRect(0, 0, tile.width, tile.height)

      ctx.save()
      ctx.scale(cellSize, cellSize)
      ctx.translate(0.5, 0.5)
      tile._keyplace.forEach(function(placement) {
        if (placement.id == 0) {
          return
        }
        var img = objectImages[placement.id]
        var ox = objectBounds[placement.id][0]/CELL_D
        var oy = -objectBounds[placement.id][3]/CELL_D
        if (fadeTallObjects) {
          if (!placement.floor && oy < -0.7) {
            ctx.globalAlpha = 0.4
          } else {
            ctx.globalAlpha = 1
          }
        } else {
          ctx.globalAlpha = 1
        }
        if (img && img.complete) {
          var iw = img._iw
          var ih = img._ih
          ctx.drawImage(img, placement.x + ox + offset, placement.y + oy + offset, iw, ih)
        } else {
          ctx.fillStyle = colorobject(placement.id)
          var iw = objectBounds[placement.id][2]/CELL_D - ox
          var ih = objectBounds[placement.id][1]/CELL_D + oy
          ctx.globalAlpha = 0.5
          ctx.fillRect(placement.x + ox, placement.y + oy, iw, ih)
        }
      })
      ctx.restore()

      if (done) done(null, tile)
    },
  })

  L.GridLayer.ObjectLayerPixel = L.GridLayer.extend({
    options: Object.assign({
      pane: 'overlayPane',
      name: 'object layer pixel',
      className: 'crisp object-layer-pixel',
      minZoom: 24,
      maxZoom: 31,
      //minNativeZoom: 24,
      maxNativeZoom: 24,
      attribution: attribution,
    }, objectGenerationOptions),
    createTile: function (coords, done) {
      var tile = document.createElement('canvas');
      var tileSize = this.getTileSize();
      //console.log(tileSize)
      tile.setAttribute('width', tileSize.x);
      tile.setAttribute('height', tileSize.y);

      setTimeout(this.drawTile.bind(this), 0, tile, coords, done)

      return tile;
    },
    drawTile(tile, coords, done) {
      var tileSize = this.getTileSize();

      var ctx = tile.getContext('2d', {alpha: true});
      ctx.clearRect(0, 0, tile.width, tile.height)

      var pnw = L.point(coords.x * tileSize.x, coords.y * tileSize.y)
      //console.log(coords, pnw)
      var llnw = crs.pointToLatLng(pnw, coords.z)
      //console.log(coords, llnw)

      var stride = Math.pow(2, 24 - coords.z)
      var w = tile.width
      var h = tile.height
      var startX = llnw.lng + 0.5
      var startY = llnw.lat - 0.5

      //console.log(coords, startX, startY)

      var scale = this.options.biomeOffset + this.options.biomeScale * 7
      var roughness = this.options.biomeFractalRoughness

      var imageData = ctx.createImageData(tile.width, tile.height)
      var d = imageData.data

      for (var y = 0;y < h;y++) {
        for (var x = 0;x < w;x++) {
          var i = (y * w + x) * 4
          var wx = startX + x*stride
          var wy = startY - (y*stride)
          var v = getMapObjectRaw(wx, wy, this.options)
          var color = hsvToRgb(v * 3079 % 359 / 360, 1, 1)
          d[i+0] = color[0]
          d[i+1] = color[1]
          d[i+2] = color[2]
          d[i+3] = (v == 0 ? 0 : 255)
        }
      }

      ctx.putImageData(imageData, 0, 0)
      done(null, tile)
    },
  })

  /*
  var objectOverlayPixel = new L.GridLayer.ObjectLayerPixel({
    opacity: 0.5,
  })
  overlays['Object Pixel'] = objectOverlayPixel
  */

  L.GridLayer.ObjectLayerSprite = L.GridLayer.SpriteLayer.extend({
    options: Object.assign({
      pane: 'overlayPane',
      showNaturalObjectsAboveZoom: objectLayerOptions.showNaturalObjectsAboveZoom,
    }, objectGenerationOptions),
    createTile: function (coords, done) {
      var layer = this
      var options = layer.options
      if (coords.z < options.showNaturalObjectsAboveZoom) {
        done()
        return document.createElement('div');
      }
      var tile = document.createElement('canvas');
      var tileSize = layer.getTileSize();
      var superscale = Math.pow(2, options.supersample)
      tile.setAttribute('width', tileSize.x*superscale);
      tile.setAttribute('height', tileSize.y*superscale);
      var paddingX = 2;
      var paddingUp = 2;
      var paddingDown = 4;

      var pnw = L.point(coords.x * tileSize.x, coords.y * tileSize.y)
      var pse = L.point(pnw.x + tileSize.x, pnw.y + tileSize.y)
      //console.log('pnw', coords, pnw)
      var llnw = crs.pointToLatLng(pnw, coords.z)
      var llse = crs.pointToLatLng(pse, coords.z)
      //console.log('llnw', coords, llnw)

      var startX = llnw.lng + 0.5
      var startY = llnw.lat - 0.5
      var endX = llse.lng + 0.5
      var endY = llse.lat - 0.5
      //console.log('start', startX, startY)
      //console.log('end', endX, endY)

      //console.log(coords)
      var right = (endX - startX) + paddingX
      var bottom = (startY - endY) + paddingDown
      var minSize = 1.5 * Math.pow(2, 31 - coords.z)
      var kp = tile._keyplace = []
      //console.log(-paddingUp, bottom, -paddingX, right)
      for (var y = -paddingUp;y < bottom;y++) {
        for (var x = -paddingX;x < right;x++) {
          var wx = startX + x
          var wy = startY - y
          var v = getMapObjectRaw(wx, wy, options)
          if (v == 0) continue
          var size = objectSize[v]
          var tooSmall = !size || size <= minSize
          if (!tooSmall) {
            kp.push({
              x: x,
              y: y,
              id: v,
            })
          }
        }
      }
      loadImages(tile._keyplace, function() {
        layer.drawTile(tile, coords, done)
      })
      return tile
    },
  })

  /*
  var objectOverlaySprite = new L.GridLayer.ObjectLayerSprite({
    //opacity: 0.2,
    //offset: 0.2,
  })
  overlays['Object Sprite'] = objectOverlaySprite
  */

  var badlandsBaseBiome = new L.GridLayer.BiomeLayer({
    biomeMap: badlandsBiomeMap,
  })

  var greenColor3 = [50, 200, 50]
  var swampColor3 = [150, 70, 150]
  var plainsColor3 = [250, 250, 30]
  var badlandsColor3 = [150, 150, 150]

  var server3Biome = new L.GridLayer.BiomeLayer({
    biomeMap: badlandsBiomeMap,
    biomeColors: [
      greenColor3,
      swampColor3,
      plainsColor3,
      badlandsColor3,
    ]
  })

  var server3Map = L.imageOverlay('overlays/server3.png',
    [[-1170.5, -695.5], [-401.5, 1252.5 - 695]], {
      className: 'crisp',
      pane: 'baseimagePane',
      attribution: '<a href="https://onehouronelife.com/forums/viewtopic.php?id=236">rosden</a>',
    })

  var server3 = new L.layerGroup([server3Biome, server3Map])

  var badlandsAge = new L.layerGroup([badlandsBaseBiome])

  var tw = 734
  var th = 585
  var tx = 378.5
  var ty = 298
  var hscale = 3.02
  var vscale = 3.02
  var tholMap = L.imageOverlay('overlays/thol.png',
    [[(th-ty)*-vscale, tx*-hscale], [ty*vscale, (tw-tx)*hscale]], {
      pane: 'overlayPane',
      attribution: 'GetInMyVan',
      opacity: 0.7,
    })

  var biomeLayers = {
    "badlandsAge": badlandsAge,
    "screenshot": L.layerGroup([biomeImageLayer, screenshotImageLayer]),
    "tholMap": tholMap,
  }

  var updateWorlds = function(worldData) {
    //console.log(worldData);
    worlds = worldData.map(function(world) {
      world.generation.computeMapBiomeIndex = computeMapBiomeIndexFunctions[world.generation.computeMapBiomeIndex]
      if (!world.generation.computeMapBiomeIndex) {
        console.log(world, "no biome index function")
      }
      world.generation.specialBiomeBandIndexOrder =
        world.generation.specialBiomeBandOrder.map(function(id) {
          return world.generation.biomeMap.indexOf(id);
        });
      world.generation.specialBiomeBandDefaultIndex = world.generation.biomeMap.indexOf(world.generation.specialBiomeBandDefault);
      if (world.biomeLayer == 'tholMap') {
        world.biomeLayer = L.layerGroup([
          new L.GridLayer.BiomeLayer(world.generation),
          tholMap,
        ])
        world.biomeLayer.name = world.name + ' Biome'
      } else if (world.biomeLayer) {
        world.biomeLayer = biomeLayers[world.biomeLayer]
        if (!world.biomeLayer) {
          console.log(world, "no biome layer")
        }
      }
      return world
    })
  }

  var createWorldLayers = function(world) {
    if (!world.biomeLayer && world.generation.biomeRandSeedA) {
      //console.log('biome layer', world.name)
      world.biomeLayer = new L.GridLayer.BiomeLayer(world.generation)
      world.biomeLayer.name = world.name + ' Biome'
    }
    if (world.spans.length > 0) {
      world.spans.forEach(function(span) {
        if (span.keyPlacementLayer) return
        //console.log('maplog layer', world.name)
        var keyPlacementLayer = createArcKeyPlacementLayer(span.dataTime, world.generation)
        keyPlacementLayer.name = "key placement"
        span.keyPlacementLayer = keyPlacementLayer
        var maplogLayer = createArcMaplogLayer(span.msStart, span.dataTime, span.base, world.generation)
        maplogLayer.name = "maplog"
        span.maplogLayer = maplogLayer
        L.Util.setOptions(keyPlacementLayer, {alternateAnim: maplogLayer})
        L.Util.setOptions(maplogLayer, {alternateStatic: keyPlacementLayer})

        var keyPlacementPoint = createArcKeyPlacementPointOverlay(span.dataTime, world.generation)
        keyPlacementPoint.name = "object point static"
        span.keyPlacementPoint = keyPlacementPoint
        var maplogPoint = createArcMaplogPointOverlay(span.msStart, span.dataTime, span.base, world.generation)
        maplogPoint.name = "object point anim"
        span.maplogPoint = maplogPoint
        L.Util.setOptions(keyPlacementPoint, {alternateAnim: maplogPoint})
        L.Util.setOptions(maplogPoint, {alternateStatic: keyPlacementPoint})

        var keySearchPoint = createArcKeySearchPointOverlay(span.dataTime, world.generation)
        keySearchPoint.name = "object search static"
        span.keySearchPoint = keySearchPoint
        var maplogSearchPoint = createArcMaplogSearchPointOverlay(span.msStart, span.dataTime, world.generation)
        maplogSearchPoint.name = "object search anim"
        span.maplogSearchPoint = maplogSearchPoint
        L.Util.setOptions(keySearchPoint, {alternateAnim: maplogSearchPoint})
        L.Util.setOptions(maplogSearchPoint, {alternateStatic: keySearchPoint})
        var keySearchImage = createArcKeySearchImageOverlay(span.dataTime)
        keySearchImage.name = "object search image"
        span.keySearchImage = keySearchImage

        var actmap = createArcActivityMapLayer(span.dataTime)
        actmap.name = "activity map"
        span.actmap = actmap
      })
    }
    if (world.generation.biomeRandSeedA) {
      if (world.objectLayer) return
      //console.log('object layer', world.name)
      var objectLayer = new L.GridLayer.ObjectLayerSprite(Object.assign({}, world.generation, objectLayerOptions))
      //var objectLayer = new L.GridLayer.ObjectLayerPixel(world.generation)
      objectLayer.name = world.name + " Objects"
      world.objectLayer = objectLayer
    }
  }

  var chooseRandPlacements = function(world) {
    if (world.generation.placements) return;
    world.generation.placements = specialMapPlacements.concat()
    var options = Object.assign({}, objectGenerationOptions, world.generation)
    if (options.biomes.length < 1
     || options.randPlacements.length < 1
     || options.randSeed == null
     || options.biomeRandSeedA == null) {
       return
    }
    var safeR = 354 - 2
    var placementRandomSource = new CustomRandomSource(options.randSeed)
    //console.log('------------------------', options.randSeed, safeR)
    //console.log('rand placements', world.name)
    options.randPlacements.forEach(function(place) {
      //console.log('vvvv  seeking', place.id, place.biomes)
      var toPlace = place.randPlacement
      var crazy = 0
      while (toPlace > 0 && crazy++ < 2000) {
        var pickX = placementRandomSource.getRandomBoundedInt(-safeR, safeR)
        var pickY = placementRandomSource.getRandomBoundedInt(-safeR, safeR)
        var bi = options.computeMapBiomeIndex(pickX, pickY, options)
        var pickB = options.biomeMap[bi]
        //console.log(pickX, pickY, pickB)
        if (place.biomes.indexOf(pickB) != -1) {
          //console.log('hit')
          world.generation.placements.push({
            msStart: world.msStart,
            x: pickX,
            y: pickY,
            id: place.id,
          })
          toPlace--
        }
      }
      //console.log(world.generation.placements, crazy, world.name)
    })
  }

  var TileDataCache = L.Class.extend({
    options: {
      dataminzoom: 24,
      datamaxzoom: 24,
    },
    getDataTileUrl: function(coords, data) {
      return L.Util.template(this._url, L.Util.extend(data, coords))
    },
    initialize: function(url, options) {
      this._url = url;
      this._list = []
      this._index = {}
      options = L.Util.setOptions(this, options);
    },
    dataZoom: function(coords) {
      return Math.max(this.options.dataminzoom, Math.min(this.options.datamaxzoom, coords.z))
    },
    dataCoords: function(coords) {
      var dataZoom = this.dataZoom(coords)
      var cellSize = Math.pow(2, coords.z - dataZoom)
      return {
        x: Math.floor(coords.x/cellSize),
        y: Math.floor(coords.y/cellSize),
        z: dataZoom,
      }
    },
    expire: function() {
      while (this._list.length > 100) {
        var record = this._list.shift()
        delete this._index[record.url]
      }
    },
    loadTile: function(coords, data) {
      var cache = this
      if (data.time == '0') return cache.transformTile(Promise.resolve(''), coords)
      var datacoords = cache.dataCoords(coords)
      //console.log(datacoords)
      var url = cache.getDataTileUrl(datacoords, data)
      //console.log(url)
      if (cache._index[url]) {
        var record = cache._index[url]
        cache._list.splice(cache._list.indexOf(record),1)
        cache._list.push(record)
        return record.promise
      } else {
        //console.log(url)
        var record = {
          url: url,
          promise: fetch(url).then(function(response) {
            if (response.status % 100 == 4) {
              return cache.transformTile(Promise.resolve(''), coords)
            }
            return cache.transformTile(response.text(), coords)
          }),
        }
        cache._list.push(record)
        cache._index[url] = record
        cache.expire()
        //console.log(cache._list.length)
        return record.promise
      }
    },
    transformTile: function(promise, debugInfo) {
      return promise
    },
  })

  var TileIndexCache = TileDataCache.extend({
    transformTile: function(promise, debugInfo) {
      return promise.then(parseTileIndex)
    },
  })

  var TileKeyValueXYCache = TileDataCache.extend({
    transformTile: function(promise, debugInfo) {
      return promise.then(function(text) {
        return decodeKeyValueYX(text, debugInfo)
      })
    },
  })

  var TileLogValueYXTCache = TileDataCache.extend({
    transformTile: function(promise, debugInfo) {
      return promise.then(function(text) {
        return decodeLogValueYXT(text, debugInfo)
      })
    },
  })

  var parseTileIndex = function(indexText) {
    //console.log(indexText)
    var tileTime = {}
    var currentTime = 0
    indexText.split("\n").forEach(function(line) {
      if (line[0] == 't') {
        currentTime = line.slice(1)
      } else {
        var parts = line.split(' ')
        var y = parts[0]
        parts.slice(1).forEach(function(x) {
          tileTime[[x,y].join(' ')] = currentTime
        })
      }
    })
    //console.log(tileTime)
    return tileTime;
  }

  var IndexedTileDataCache = L.Class.extend({
    options: {
    },
    initialize: function(indexCache, dataCache, options) {
      this._indexCache = indexCache;
      this._dataCache = dataCache;
      options = L.Util.setOptions(this, options);
    },
    expire: function() {
      this._indexCache.expire()
      this._dataCache.expire()
    },
    loadTile: function(coords, data) {
      var cache = this
      return cache._indexCache.loadTile(coords, data).then(function(tileTime){
        var datacoords = cache._dataCache.dataCoords(coords)
        var time = tileTime[[datacoords.x, datacoords.y].join(' ')]
        if (time) {
          data.time = time || data.time
          //console.log(datacoords, time, data.time)
          return cache._dataCache.loadTile(coords, data)
        } else {
          return []
        }
      })
    },
  })

  var SearchIndexCache = TileDataCache.extend({
    transformTile: function(promise) {
      return promise.then(parseSearchIndex)
    },
  })

  var parseSearchIndex = function(indexText) {
    //console.log(indexText)
    var objectCount = []
    var lines = indexText.split("\n")
    for (var i in lines) {
      var line = lines[i]
      if (line[0] == '-') {
        //console.log(objectCount)
        return objectCount;
      } else {
        var parts = line.split(' ')
        if (parts.length != 2) return
        objectCount[parseInt(parts[0], 10)] = parseInt(parts[1], 10)
      }
    }
    return objectCount;
  }

  var IndexedSearchDataCache = L.Class.extend({
    options: {
    },
    initialize: function(indexCache, dataCache, options) {
      this._indexCache = indexCache;
      this._dataCache = dataCache;
      options = L.Util.setOptions(this, options);
    },
    expire: function() {
      this._indexCache.expire()
      this._dataCache.expire()
    },
    loadTile: function(coords, data) {
      var cache = this
      return cache._indexCache.loadTile(coords, data).then(function(objectCounts){
        if (!objectCounts) return []
        return Promise.all(data.highlightObjects.map(function(id) {
          if (objectCounts[id]) {
            data.id = id
            return cache._dataCache.loadTile(coords, data)
          } else {
            return []
          }
        })).then(function(results) {
          //console.log(results)
          return results.flat()
        });
      })
    },
  })


  var firstDecode = function(s) {
    var first = s.charCodeAt(0)
    if (first < 97) {
      return parseInt((String.fromCharCode(first - (65-48)) + s.slice(1)), 10) * -1
    } else {
      return parseInt((String.fromCharCode(first - (97-48)) + s.slice(1)), 10)
    }
  }

  var decodeKeyPlain = function(text, coords) {
    try {
      return text.split("\n").filter(function(line) {
        return line != "";
      }).map(function(line) {
        var parts = line.split(" ")
        try {
        var out = {
          x: parseInt(parts[0],10),
          y: parseInt(parts[1],10),
          id: parseInt(parts[2].replace('f', ''),10) & objectIDMask,
          floor: parts[2][0] == 'f',
        }
        } catch (e) {
          console.log(e, parts, line)
          throw "tile decode failed"
        }
        return out
      })
    } catch (e) {
      console.log(e, coords)
      return []
    }
  }

  var decodeKeyValueYX = function(text, coords) {
    var currentValue = 0
    var id = '0'
    var floor = false
    var y = 0
    var x = 0
    var output = []
    try {
      text.split("\n").filter(function(line) {
        return line != "";
      }).map(function(line) {
        if (line[0] == 'v') {
          currentValue = line.slice(1)
          id = parseInt(currentValue.replace('f', ''),10) & objectIDMask
          floor = currentValue[0] == 'f'
        } else {
          var col = line.match(/(-?\d+)(.*)/)
          try {
            y = y + parseInt(col[1],10)
            col[2].match(/[A-Ja-j]\d*/g).forEach(function(edx) {
              x = x + firstDecode(edx)
              var out = {
                x: x,
                y: y,
                id: id,
                floor: floor,
              }
              output.push(out)
            })
          } catch (e) {
            console.log(e, col, line)
            throw "tile decode error"
          }
        }
      })
    } catch (e) {
      console.log(e, coords)
    }
    return output
  }

  var decodeLogDiff = function(text, coords) {
    try {
      var t = 0
      var x = 0
      var y = 0
      return text.split("\n").filter(function(line) {
        return line != "";
      }).map(function(line) {
        var parts = line.split(" ")
        try {
        t = t + (parseInt(parts[0],10)*10)
        x = x + parseInt(parts[1],10)
        y = y + parseInt(parts[2],10)
        var out = {
          t: t,
          x: x,
          y: y,
          id: parseInt(parts[3].replace('f', ''),10) & objectIDMask,
          floor: parts[3][0] == 'f',
        }
        } catch (e) {
          console.log(e, parts, line)
          throw "tile decode failed"
        }
        return out
      })
    } catch (e) {
      console.log(e, coords)
      return []
    }
  }

  var decodeLogValueYXT = function(text, coords) {
    var currentValue = 0
    var id = '0'
    var floor = false
    var y = 0
    var x = 0
    var t = 0
    var output = []
    try {
      text.split("\n").filter(function(line) {
        return line != "";
      }).map(function(line) {
        if (line[0] == 'v') {
          currentValue = line.slice(1)
          id = parseInt(currentValue.replace('f', ''),10) & objectIDMask
          floor = currentValue[0] == 'f'
        } else if (line[0] == 'y') {
          y = y + parseInt(line.slice(1),10)
        } else {
          var col = line.match(/(-?\d+)(.*)/)
          try {
            x = x + parseInt(col[1],10)
            col[2].match(/[A-Ja-j]\d*/g).forEach(function(edt) {
              t = t + firstDecode(edt)*10
              var out = {
                t: t,
                x: x,
                y: y,
                id: id,
                floor: floor,
              }
              output.push(out)
            })
          } catch (e) {
            console.log(e, col, line)
            throw "tile decode error"
          }
        }
      })
    } catch (e) {
      console.log(e, coords)
    }
    return output
  }

  var calculateTileBounds = function(coords, tileSize) {
    var pnw = L.point(coords.x * tileSize, coords.y * tileSize)
    var pse = L.point(pnw.x + tileSize, pnw.y + tileSize)
    //console.log('pnw', coords, pnw)
    var llnw = crs.pointToLatLng(pnw, coords.z)
    var llse = crs.pointToLatLng(pse, coords.z)
    //console.log('llnw', coords, llnw)

    var worldStartX = llnw.lng + 0.5
    var worldStartY = llnw.lat - 0.5
    var worldEndX = llse.lng + 0.5
    var worldEndY = llse.lat - 0.5
    //console.log('start', worldStartX, worldStartY)
    //console.log('end', worldEndX, worldEndY)

    var paddingX = 2;
    var paddingUp = 2;
    var paddingDown = 4;

    var tileStartX = -paddingX
    var tileStartY = -paddingUp
    var tileEndX = (worldEndX - worldStartX) + paddingX
    var tileEndY = (worldStartY - worldEndY) + paddingDown

    return {
      worldStartX: worldStartX,
      worldStartY: worldStartY,
      worldEndX: worldEndX,
      worldEndY: worldEndY,
      tileStartX: tileStartX,
      tileStartY: tileStartY,
      tileEndX: tileEndX,
      tileEndY: tileEndY,
      inFrame: function(placement) {
        var isValid = !isNaN(placement.id) && placement.id < 50000
        if (!isValid) return false
        var inFrame =
          (tileStartX <= placement.x && placement.x < tileEndX) &&
          (tileStartY <= placement.y && placement.y < tileEndY)
        if (!inFrame) return false
        return true
      }
    }
  }

  var TileKey = L.Class.extend({
    options: {
    },
    initialize: function(cache, options) {
      this._cache = cache;
      options = L.Util.setOptions(this, options);
    },
    getTile: function(coords, data, generation) {
      //console.time('data tile ' + JSON.stringify(coords))
      return this._cache.loadTile(coords, data).then(function(rawPlacements) {
        //console.timeEnd('data tile ' + JSON.stringify(coords))
        var occupied = {}

        var t = parseInt(data.time, 10)*1000


        var tileBounds = calculateTileBounds(coords, generation.tileSize)
        var worldStartX = tileBounds.worldStartX
        var worldStartY = tileBounds.worldStartY
        var worldEndX = tileBounds.worldEndX
        var worldEndY = tileBounds.worldEndY

        //console.time('data processing ' + JSON.stringify(coords))
        var placements = rawPlacements.map(function(raw) {
          var place = {
            x: raw.x - worldStartX,
            y: -(raw.y - worldStartY),
            id: raw.id,
            floor: raw.floor,
            t: t,
            key: [raw.x, raw.y].join(' '),
          }
          occupied[place.key] = true
          return place
        })

        var special = []
        generation.placements.filter(function(place) {
          return (place.msStart/1000 < generation.dataTime
                  && worldStartX <= place.x && place.x <= worldEndX
                  && worldEndY <= place.y && place.y <= worldStartY)
        }).forEach(function(place) {
          var out = {
            t: t,
            x: place.x - worldStartX,
            y: -(place.y - worldStartY),
            id: place.id,
            floor: false,
          }
          out.key = [out.x, out.y].join(' ')
          if (!occupied[out.key]) {
            occupied[out.key] = true
            special.push(out)
          }
        })

        var tileStartX = tileBounds.tileStartX
        var tileStartY = tileBounds.tileStartY
        var tileEndX = tileBounds.tileEndX
        var tileEndY = tileBounds.tileEndY
        var minSize = 1.5 * Math.pow(2, 31 - coords.z)
        var natural = []
        if (coords.z >= generation.showNaturalObjectsAboveZoom && generation.biomeRandSeedA) {
          for (var y = tileStartY;y < tileEndY;y++) {
            for (var x = tileStartX;x < tileEndX;x++) {
              if (occupied[[x, y].join(' ')]) {
                continue
              }
              var wx = worldStartX + x
              var wy = worldStartY - y
              var v = getMapObjectRaw(wx, wy, generation)
              if (v == 0) continue
              natural.push({
                t: t,
                x: x,
                y: y,
                id: v,
                floor: false,
              })
            }
          }
        }

        var keyplace =
          [].concat(natural, special, placements)

          .filter(tileBounds.inFrame)
          .sort(sortTypeAndDrawOrder)

        return keyplace
      })
    },
  })

  var TileSearch = L.Class.extend({
    options: {
    },
    initialize: function(cache, options) {
      this._cache = cache;
      options = L.Util.setOptions(this, options);
    },
    getTile: function(coords, data, options) {
      //console.time('data tile ' + JSON.stringify(coords))
      return this._cache.loadTile(coords, data).then(function(rawPlacements) {
        //console.timeEnd('data tile ' + JSON.stringify(coords))
        var tileBounds = calculateTileBounds(coords, options.tileSize)
        var worldStartX = tileBounds.worldStartX
        var worldStartY = tileBounds.worldStartY

        //console.time('data processing ' + JSON.stringify(coords))
        var placements = rawPlacements.map(function(raw) {
          var place = {
            x: raw.x - worldStartX,
            y: -(raw.y - worldStartY),
            id: raw.id,
            floor: raw.floor,
          }
          return place
        })

        var keyplace = placements
          .filter(tileBounds.inFrame)

        return keyplace
      })
    },
  })

  var TileLog = L.Class.extend({
    options: {
    },
    initialize: function(cache, key, options) {
      this._cache = cache;
      this._key = key;
      options = L.Util.setOptions(this, options);
    },
    getTile: function(coords, logData, baseData, generation) {
      //console.time('data tile ' + JSON.stringify(coords))
      return Promise.all([
        this._cache.loadTile(coords, logData),
        this._key.getTile(coords, baseData, generation),
      ]).then(function(results) {
        var rawPlacements = results[0]
        var keyplace = results[1]
        if (rawPlacements.length == 0) return keyplace

        var tileBounds = calculateTileBounds(coords, generation.tileSize)
        var worldStartX = tileBounds.worldStartX
        var worldStartY = tileBounds.worldStartY

        var placements = rawPlacements.map(function(raw) {
          var place = {
            x: raw.x - worldStartX,
            y: -(raw.y - worldStartY),
            id: raw.id,
            floor: raw.floor,
            t: raw.t,
            key: [raw.x, raw.y].join(' '),
          }
          return place
        }).filter(tileBounds.inFrame)
        .sort(function(a, b) {
          return a.t - b.t
        })
        return [].concat(keyplace, placements)
      })
    },
  })

  var LogSearch = L.Class.extend({
    options: {
    },
    initialize: function(cache, options) {
      this._cache = cache;
      options = L.Util.setOptions(this, options);
    },
    getTile: function(coords, data, options) {
      //console.time('data tile ' + JSON.stringify(coords))
      return this._cache.loadTile(coords, data).then(function(rawPlacements) {
        //console.timeEnd('data tile ' + JSON.stringify(coords))
        var tileBounds = calculateTileBounds(coords, options.tileSize)
        var worldStartX = tileBounds.worldStartX
        var worldStartY = tileBounds.worldStartY

        //console.time('data processing ' + JSON.stringify(coords))
        var placements = rawPlacements.map(function(raw) {
          var place = {
            x: raw.x - worldStartX,
            y: -(raw.y - worldStartY),
            t: raw.t,
            id: raw.id,
            floor: raw.floor,
          }
          return place
        })

        var keyplace = placements
          .filter(tileBounds.inFrame)
          .sort(function(a, b) {
            return a.t - b.t
          })

        return keyplace
      })
    },
  })

  var objectImages = []

  var loadImages = function(placements, done) {
    placements.forEach(function(placement) {
      if (placement.id != 0 && !objectImages[placement.id]) {
        if (barrierObjects.indexOf(placement.id) != -1) {
          var canvas = document.createElement('canvas')
          objectImages[placement.id] = canvas
          var img = new Image()
          img.onload = function() {
            var w = img.naturalWidth
            var h = img.naturalHeight
            canvas.width = w
            canvas.height = h
            canvas._iw = w/CELL_D
            canvas._ih = h/CELL_D
            var ctx = canvas.getContext('2d', {alpha: true});
            ctx.clearRect(0, 0, w, h)
            ctx.drawImage(img, 0, 0, w, h)
            ctx.fillStyle = 'black'
            ctx.globalCompositeOperation = 'source-atop'
            ctx.fillRect(0, 0, w, h)
            canvas.complete = true
          }
          img.src = 'static/sprites/obj_'+placement.id+'.png'
        } else {
          var img = new Image()
          objectImages[placement.id] = img
          img.onload = function() {
            img._iw = img.naturalWidth/CELL_D
            img._ih = img.naturalHeight/CELL_D
          }
          img.src = 'static/sprites/obj_'+placement.id+'.png'
        }
      }
    })

    var checkLoaded = function() {
      for (var i = 0;i < placements.length;i++) {
        var placement = placements[i]
        if (placement.id != 0 && !objectImages[placement.id].complete) {
          setTimeout(checkLoaded,100)
          return false
        }
      }
      //console.log('images done')
      setTimeout(done,0)
      return true
    }
    return checkLoaded()
  }

  L.GridLayer.KeyPlacementSprite = L.GridLayer.SpriteLayer.extend({
    options: Object.assign({
      showNaturalObjectsAboveZoom: objectLayerOptions.showNaturalObjectsAboveZoom,
    }, objectGenerationOptions),
    initialize: function(cache, options) {
      this._cache = cache;
      options = L.Util.setOptions(this, options);
    },
    createTile: function (coords, done) {
      var layer = this
      var options = layer.options
      var tile = document.createElement('canvas');
      var tileSize = layer.getTileSize();
      var superscale = Math.pow(2, options.supersample)
      tile.setAttribute('width', tileSize.x*superscale);
      tile.setAttribute('height', tileSize.y*superscale);
      //console.log(coords)

      var minSize = 1.5 * Math.pow(2, 31 - coords.z)

      //console.log(datacoords)
      //console.log('data tile ' + JSON.stringify(coords))
      layer._cache.getTile(coords, {time: options.dataTime, server: mapServer}, options).then(function(keyplace) {
        tile._keyplace = keyplace.filter(placementsLargerThan(minSize))

        //console.timeEnd('data processing ' + JSON.stringify(coords))
        //console.time('load images' + JSON.stringify(coords))
        //console.time('load images call' + JSON.stringify(coords))
        if (loadImages(tile._keyplace, function() {
          //console.timeEnd('load images' + JSON.stringify(coords))
          layer.drawTile(tile, coords, done)
        })) {
          //done()
        } else {
          //layer.drawTile(tile, coords, done)
        }
        //console.timeEnd('load images call' + JSON.stringify(coords))
      })

      return tile
    },
  })


  var placementsLargerThan = function(minSize) {
    return function(placement) {
      if (placement.floor) return true
      var size = objectSize[placement.id]
      var tooSmall = !size || size <= minSize
      if (tooSmall) return false
      return true
    }
  }

  var sortTypeAndDrawOrder = function(a, b) {
    if (a.floor != b.floor) {
      return (b.floor - a.floor)
    } else if (a.y - b.y == 0) {
      return a.x - b.x
    } else {
      return a.y - b.y
    }
  }

  var sortDrawOrder = function(a, b) {
    if (a.y - b.y == 0) {
      return a.x - b.x
    } else {
      return a.y - b.y
    }
  }

  var tileAt = function(tile, time) {
    var maplog = tile._maplog
    if (!maplog) return;
    var lastTime = tile._time || 0
    tile._time = time
    var objects = tile._objects = tile._objects || {}
    var floors = tile._floors = tile._floors || {}
    if (time < lastTime) {
      lastTime = 0
      objects = tile._objects = {}
      floors = tile._floors = {}
    }
    maplog.forEach(function(placement) {
      if (lastTime < placement.t && placement.t <= time) {
        if (placement.floor) {
          floors[placement.key] = placement
        } else {
          objects[placement.key] = placement
        }
      }
    })
    var floorPlacements = Object.values(floors)
      .sort(sortDrawOrder)
    var objectPlacements = Object.values(objects)
      .sort(sortDrawOrder)
    return floorPlacements.concat(objectPlacements)
  }

  L.GridLayer.MaplogSprite = L.GridLayer.SpriteLayer.extend({
    options: Object.assign({
      time: 0,
      showNaturalObjectsAboveZoom: objectLayerOptions.showNaturalObjectsAboveZoom,
    }, objectGenerationOptions),
    initialize: function(cache, options) {
      this._cache = cache;
      options = L.Util.setOptions(this, options);
    },
    createTile: function (coords, done) {
      var layer = this
      var options = layer.options
      var tile = document.createElement('canvas');
      var tileSize = layer.getTileSize();
      var superscale = Math.pow(2, options.supersample)
      tile.setAttribute('width', tileSize.x*superscale);
      tile.setAttribute('height', tileSize.y*superscale);

      //console.log('start', startX, startY)
      //console.log('end', endX, endY)

      //console.log(coords)
      //console.log(datacoords)
      layer._cache.getTile(coords, {time: options.dataTime, server: mapServer}, {time: options.base, server: mapServer}, options).then(function(maplog) {

        tile._maplog = maplog

        var time = layer.options.time
        tile.coords = coords
        layer.tileAt(tile, time)
        done()
      })

      return tile
    },
    tileAt: function(tile, time) {
      var minSize = 1.5 * Math.pow(2, 31 - tile.coords.z)
      tile._keyplace = tileAt(tile, time)
        .filter(placementsLargerThan(minSize))
      var layer = this
      if (loadImages(tile._keyplace, function() {
        layer.drawTile(tile, tile.coords)
      })) {
      } else {
        layer.drawTile(tile, tile.coords)
      }
    },
    updateTiles: function(ms) {
      L.Util.setOptions(this, {time: ms})
      for (var key in this._tiles) {
        var tile = this._tiles[key]
        if (tile.el._maplog) {
          this.tileAt(tile.el, ms)
        }
      }
    },
  })

  var arcUpdateTiles = function(ms) {
    oholBase.eachLayer(function(sub) {
      if (sub.eachLayer) {
        sub.eachLayer(function(layer) {
          if (layer.updateTiles) {
            layer.updateTiles(ms)
          }
        })
      }
    })
    objectOverlay.eachLayer(function(sub) {
      if (sub.eachLayer) {
        sub.eachLayer(function(layer) {
          if (layer.updateTiles) {
            layer.updateTiles(ms)
          }
        })
      }
    })
  }

  var actIndexCache = new TileIndexCache(oholMapConfig.actIndex, {
    dataminzoom: 2,
    datamaxzoom: 24,
  })

  var keyIndexCache = new TileIndexCache(oholMapConfig.keyIndex, {
    dataminzoom: 24,
    datamaxzoom: 24,
  })
  var keyDataCache = new TileKeyValueXYCache(oholMapConfig.keyPlacements, {
    dataminzoom: 24,
    datamaxzoom: 24,
  })
  var keyPlacementCache = new IndexedTileDataCache(keyIndexCache, keyDataCache, {})

  var keyPlacementKey = new TileKey(keyPlacementCache, {})

  var createArcKeyPlacementLayer = function(end, gen) {
    return new L.layerGroup([
      baseAttributionLayer,
      new L.GridLayer.KeyPlacementSprite(keyPlacementKey, Object.assign({
        dataTime: end.toString(),
      }, gen, objectLayerOptions))
    ])
  }

  var createArcKeyPlacementPointOverlay = function(end, gen) {
    return new L.layerGroup([
      baseAttributionLayer,
      new L.GridLayer.KeyPlacementPoint(keyPlacementKey, Object.assign({
        dataTime: end.toString(),
      }, gen, objectLayerOptions))
    ])
  }

  var keySearchIndexCache = new SearchIndexCache(oholMapConfig.keySearchIndex, {})

  var keySearchDataCache = new TileKeyValueXYCache(oholMapConfig.keySearch, {})
  var keySearchCache = new IndexedSearchDataCache(keySearchIndexCache, keySearchDataCache, {})

  var keySearchKey = new TileSearch(keySearchCache, {})

  var createArcKeySearchPointOverlay = function(end, gen) {
    return new L.layerGroup([
      baseAttributionLayer,
      new L.GridLayer.KeySearchPoint(keySearchKey, Object.assign({
        dataTime: end.toString(),
        animate: false,
      }, gen, objectLayerOptions))
    ])
  }

  var createArcKeySearchImageOverlay = function(end) {
    return new L.layerGroup([
      baseAttributionLayer,
      new L.LayerGroup.ObjectImageLayer(keySearchCache, Object.assign({
        dataTime: end.toString(),
      }, objectLayerOptions))
    ])
  }

  var maplogCache = new TileLogValueYXTCache(oholMapConfig.maplog, {
    dataminzoom: 24,
    datamaxzoom: 27,
  })

  var maplogLog = new TileLog(maplogCache, keyPlacementKey, {})

  var createArcMaplogLayer = function(msStart, sEnd, sBase, gen) {
    var ms = sEnd*1000
    if (msStart < mapTime && mapTime < sEnd*1000) {
      ms = mapTime
    }
    return new L.layerGroup([
      baseAttributionLayer,
      new L.GridLayer.MaplogSprite(maplogLog, Object.assign({
        dataTime: sEnd.toString(),
        base: sBase.toString(),
        time: ms,
      }, gen, objectLayerOptions))
    ])
  }

  var createArcMaplogPointOverlay = function(msStart, sEnd, sBase, gen) {
    var ms = sEnd*1000
    if (msStart < mapTime && mapTime < sEnd*1000) {
      ms = mapTime
    }
    return new L.layerGroup([
      baseAttributionLayer,
      new L.GridLayer.MaplogPoint(maplogLog, Object.assign({
        dataTime: sEnd.toString(),
        base: sBase.toString(),
        time: ms,
      }, gen, objectLayerOptions))
    ])
  }

  var maplogSearchIndexCache = new SearchIndexCache(oholMapConfig.logSearchIndex, {})

  var maplogSearchDataCache = new TileLogValueYXTCache(oholMapConfig.logSearch, {})
  var maplogSearchCache = new IndexedSearchDataCache(maplogSearchIndexCache, maplogSearchDataCache, {})

  var maplogSearch = new LogSearch(maplogSearchCache, {})

  var createArcMaplogSearchPointOverlay = function(msStart, sEnd, gen) {
    var ms = sEnd*1000
    if (msStart < mapTime && mapTime < sEnd*1000) {
      ms = mapTime
    }
    return new L.layerGroup([
      baseAttributionLayer,
      new L.GridLayer.KeySearchPoint(maplogSearch, Object.assign({
        dataTime: sEnd.toString(),
        time: ms,
        animate: true,
        alsoShowImagePoints: true,
        maxZoom: 26,
      }, gen, objectLayerOptions))
    ])
  }

  L.TileLayer.SparseTileLayer = L.TileLayer.extend({
    initialize: function(url, cache, options) {
      this._cache = cache
      L.TileLayer.prototype.initialize.call(this, url, options)
    },
    dataZoom: function(coords) {
      return Math.max(this.options.minZoom, Math.min(this.options.maxZoom, this._getZoomForUrl()))
    },
    dataCoords: function(coords) {
      var dataZoom = this.dataZoom(coords)
      var cellSize = Math.pow(2, this._getZoomForUrl() - dataZoom)
      return {
        x: Math.floor(coords.x/cellSize),
        y: Math.floor(coords.y/cellSize),
        z: dataZoom,
      }
    },
    //modified from leaflet TileLayer
    createTile: function (coords, done) {
      var layer = this
      var options = layer.options
      var tile = document.createElement('img');

      L.DomEvent.on(tile, 'load', L.Util.bind(this._tileOnLoad, this, done, tile));
      L.DomEvent.on(tile, 'error', L.Util.bind(this._tileOnError, this, done, tile));

      if (this.options.crossOrigin || this.options.crossOrigin === '') {
        tile.crossOrigin = this.options.crossOrigin === true ? '' : this.options.crossOrigin;
      }

      /*
        Alt tag is set to empty string to keep screen readers from reading URL and for compliance reasons
        http://www.w3.org/TR/WCAG20-TECHS/H67
        */
      tile.alt = '';

      /*
        Set role="presentation" to force screen readers to ignore this
        https://www.w3.org/TR/wai-aria/roles#textalternativecomputation
        */
      tile.setAttribute('role', 'presentation');

      var datacoords = layer.dataCoords(coords)
      var tileUrl = layer.getTileUrl(coords); // uses current layer zoom, disregards coords.z
      this._cache.loadTile(datacoords, {time: options.time, server: mapServer}).then(function(tileTime) {
        //console.log(coords, datacoords, time)
        if (tileTime[[datacoords.x, datacoords.y].join(' ')]) {
          tile.src = tileUrl
        } else {
          done("sparse tile miss", tile)
        }
      });
      return tile;
    },
  })

  var createArcActivityMapLayer = function(end) {
    var options = actmapLayerOptions(actmapSampleSize)
    return new L.TileLayer.SparseTileLayer(oholMapConfig.actmap, actIndexCache, Object.assign({
      pane: 'activityPane',
      server: 17,
      time: end,
      className: 'crisp activity-map-layer',
      opacity: 0.5,
      minZoom: 2,
      maxZoom: actmapMaxZoom,
      attribution: attribution,
    }, options))
  }

  var setObjectLayerOptions = function(options) {
    Object.assign(objectLayerOptions, options)
    worlds.forEach(function(world) {
      world.spans.forEach(function(span) {
        if (span.keyPlacementLayer) {
          span.keyPlacementLayer.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
        if (span.keyPlacementPoint) {
          span.keyPlacementPoint.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
        if (span.keySearchPoint) {
          span.keySearchPoint.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
        if (span.keySearchImage) {
          span.keySearchImage.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
        if (span.maplogLayer) {
          span.maplogLayer.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
        if (span.maplogPoint) {
          span.maplogPoint.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
        if (span.maplogSearchPoint) {
          span.maplogSearchPoint.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
      })
      if (world.objectLayer) {
        var layer = world.objectLayer
        L.Util.setOptions(layer, options)
        layer.redraw && layer.redraw()
      }
    })
  }

  var setObjectHighlightOptions = function(options) {
    Object.assign(objectLayerOptions, options)
    worlds.forEach(function(world) {
      world.spans.forEach(function(span) {
        if (span.keyPlacementPoint) {
          span.keyPlacementPoint.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
        if (span.keySearchPoint) {
          span.keySearchPoint.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
        if (span.keySearchImage) {
          span.keySearchImage.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
        if (span.maplogPoint) {
          span.maplogPoint.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
        if (span.maplogSearchPoint) {
          span.maplogSearchPoint.eachLayer(function(layer) {
            L.Util.setOptions(layer, options)
            layer.redraw && layer.redraw()
          })
        }
      })
    })
  }

  var setActivityMapOptions = function(options) {
    worlds.forEach(function(world) {
      world.spans.forEach(function(span) {
        if (span.actmap) {
          var layer = span.actmap
          L.Util.setOptions(layer, options)
          layer._map && layer._resetView && layer._resetView()
          layer.redraw && layer.redraw()
        }
      })
    })
  }

  var colorobject = function(id) {
    var hue = id * 3079 % 359
    return "hsl(" + hue + ", 100%, 50%)"
  }

  var colormap = function(id) {
    return '#' + (((id * 49157) % 12582917).toString(16))
  }

  var colorlineage = function(id, theme) {
    id = id || 0
    var hue = (id * 49157) % 359
    var sat = (id * 24593) % 67 + 33
    var light = (id * 12289) % 53 + (theme == 'dark' ? 47 : 20)
    var greenish = Math.abs(hue - 120)
    if (greenish < 60) {
      light = Math.min(light, greenish * 30 / 60 + 20)
    }
    return "hsl(" + hue + ", " + sat + "%, " + light + "%)"
  }

  var colorhash = function(hash) {
    return '#' + hash.slice(0,6)
  }

  var colorlinear = function(time, min, max) {
    var hue = (time - min) * 90 / (max-min) - 30
    var light = (time - min) * 30 / (max-min) + 20
    return "hsl(" + hue + ", 100%, " + light + "%)"
  }

  var colorage = function(age) {
    var hue = (age) * 300 / (60) - 60
    return "hsl(" + hue + ", 100%, 50%)"
  }

  var colorcause = function(cause, theme) {
    if (cause && cause.match('killer')) {
      return "#ff0000";
    }
    switch(cause) {
      case 'hunger': return "hsl(60, 50%, 40%)"
      case 'oldAge':
        if (theme == 'dark') return "hsl(0, 0%, 80%)"
        else return "hsl(0, 0%, 20%)"
      case 'disconnect': return "hsla(180, 100%, 40%)"
      case 'unknown': return "hsl(290, 100%, 50%)"
      default: return "hsla(90, 100%, 50%, 0)"
    }
  }

  L.GridLayer.LifePointOverlay = L.GridLayer.extend({
    options: {
      pane: 'overlayPane',
      alternateAnim: null,
      alternateStatic: null,
      color: 'defaultColor',
      location: 'birth',
      theme: 'dark',
    },
    createTile: function (coords) {
      var tile = document.createElement('canvas');
      var tileSize = this.getTileSize();
      //console.log(tileSize)
      tile.setAttribute('width', tileSize.x);
      tile.setAttribute('height', tileSize.y);

      this.drawTile(tile, coords)

      return tile;
    },
    drawTile(tile, coords, time) {
      var layer = this
      var options = layer.options
      if (!options.data) {
        return
      }
      if ('time' in options) {
        time = time || options.time/1000
      }
      var tileSize = layer.getTileSize();
      var cellSize = Math.pow(2, coords.z - 24)

      var ctx = tile.getContext('2d');
      ctx.clearRect(0, 0, tile.width, tile.height)
      var padding = 10

      var origin = L.point(coords.x * tileSize.x, coords.y * tileSize.y)
      var pnw = L.point(coords.x * tileSize.x - padding, coords.y * tileSize.y - padding)
      var pse = L.point(pnw.x + tileSize.x + padding*2, pnw.y + tileSize.y + padding*2)
      //console.log(coords, pnw, pse)
      var llnw = crs.pointToLatLng(pnw, coords.z)
      var llse = crs.pointToLatLng(pse, coords.z)
      //console.log(coords, llnw, llse)

      var color = options.color
      if (color == 'lineageColor') color = color + options.theme
      if (color == 'causeOfDeathColor') color = color + options.theme
      var location = options.location
      var fadeTime = 60*60
      var r = 3 + Math.pow(cellSize, 0.6)
      options.data.forEach(function(point) {
        //console.log(point)

        var t = 1
        var ring = 1;
        var x = point.birth_x;
        var y = point.birth_y;
        if (location == 'death') {
          x = point.death_x || x;
          y = point.death_y || y;
        }
        if ( llnw.lng < x && x < llse.lng
          && llse.lat < y && y < llnw.lat) {

          var death_time = (point.death_time || (point.birth_time + fadeTime))
          if (time) {
            if (time < point.birth_time || death_time < time) {
              return
            }

            t = (time - point.birth_time) / (death_time - point.birth_time)
            var a = Math.pow(1 - t, 0.4)
            ctx.globalAlpha = a
            ring = Math.pow(10 * t, -6 * t)
          } else {
            t = 1
            ctx.globalAlpha = 0.5
          }
          var ll = L.latLng(y, x)
          var p = crs.latLngToPoint(ll, coords.z)
          //console.log(ll, p, pnw)
          p.x = p.x - origin.x
          p.y = p.y - origin.y
          //console.log(p)

          var age = point.age
          if (time) {
            if (point.chain == 1) {
              age = 14 + (age - 14) * t
            } else {
              age = age * t
            }
          }

          if (time && color == 'ageColor') {
            ctx.fillStyle = ctx.strokeStyle = colorage(age)
          } else if (color == 'defaultColor') {
            if (options.theme == 'dark') {
              ctx.fillStyle = ctx.strokeStyle = '#fff'
            } else {
              ctx.fillStyle = ctx.strokeStyle = '#000'
            }
          } else {
            ctx.fillStyle = ctx.strokeStyle = point[color]
          }

          if (point.gender == 'M') {
            ctx.beginPath();
            ctx.fillRect(p.x - r, p.y - r, r*2, r*2)
            ctx.fill();
          } else if (14 <= age && age <= 40) {
            ctx.beginPath();
            ctx.arc(p.x, p.y, r*0.7, 0, 2*Math.PI, false);
            ctx.lineWidth = r*0.7;
            ctx.stroke();
          } else {
            ctx.beginPath();
            ctx.arc(p.x, p.y, r, 0, 2*Math.PI, false);
            ctx.fill();
          }

          if (point.chain == 1) {
            ctx.beginPath();
            ctx.arc(p.x, p.y, r + 7 * ring, 0, 2*Math.PI, false);
            if (time) {
              ctx.lineWidth = 1;
            } else {
              ctx.lineWidth = 0.5;
            }
            ctx.stroke();
          }
        }
      })
    },
    updateTiles: function(ms) {
      L.Util.setOptions(this, {time: ms})
      var time = ms/1000
      for (var key in this._tiles) {
        var tile = this._tiles[key]
        this.drawTile(tile.el, tile.coords, time)
      }
    },
    selectPoints: function(ev) {
      var center = ev.layerPoint
      var padding = 5
      var pnw = L.point(center.x - padding, center.y - padding)
      var pse = L.point(pnw.x + padding*2, pnw.y + padding*2)
      //console.log(center, pnw, pse)
      var layer = ev.target
      if (!layer.options.data) return;
      var map = layer._map
      var zoom = map.getZoom()
      var llnw = map.layerPointToLatLng(pnw, zoom)
      var llse = map.layerPointToLatLng(pse, zoom)
      //console.log(center, llnw, llse)
      var location = layer.options.location || 'birth'

      var hit = layer.options.data.filter(function(point) {
        var x = point.birth_x
        var y = point.birth_y
        if (location == 'death') {
          x = point.death_x || x;
          y = point.death_y || y;
        }
        return llnw.lng < x && x < llse.lng
            && llse.lat < y && y < llnw.lat
      })
      if ('time' in this.options) {
        var time = this.options.time/1000
        hit = hit.filter(function(point) {
          return point.birth_time < time && time < point.death_time
        })
      }
      if (hit.length > 0) {
        app.ports.leafletEvent.send({
          kind: 'selectPoints',
          lives: { data: hit.slice(0, 100) },
        })
      }
    },
  })

  var lifePointOverlay = new L.GridLayer.LifePointOverlay({
    className: 'interactive life-point-overlay',
    pane: 'overlayPane',
  })
  lifePointOverlay.name = 'life point overlay'

  var lifeAnimOverlay = new L.GridLayer.LifePointOverlay({
    className: 'interactive life-anim-overlay',
    time: 0,
    alternateStatic: lifePointOverlay,
    pane: 'overlayPane',
  })
  lifeAnimOverlay.name = 'life anim overlay'
  L.Util.setOptions(lifePointOverlay, {alternateAnim: lifeAnimOverlay})

  if (dataAnimated) {
    lifeAnimOverlay.addTo(dataOverlay)
  } else {
    lifePointOverlay.addTo(dataOverlay)
  }

  var resultPoints = new L.GridLayer.LifePointOverlay().addTo(lifeSearchOverlay)

  var setDataLayers = function(data, sendRange) {
    var theme = legendControl.options.theme
    var min = null;
    var max = null;
    var minChain = null;
    var maxChain = null;
    data.forEach(function(point) {
      if (min == null || point.birth_time < min) {
        min = point.birth_time
      }
      if (max == null || point.death_time > max) {
        max = point.death_time
      }

      if (minChain == null || point.chain < minChain) {
        minChain = point.chain
      }
      if (maxChain == null || point.chain > maxChain) {
        maxChain = point.chain
      }
    })
    data.forEach(function(point) {
      point.lineageColordark = colorlineage(point.lineage, 'dark')
      point.lineageColorlight = colorlineage(point.lineage, 'light')
      if (point.hash) {
        point.hashColor = colorhash(point.hash)
      }
      point.birthTimeColor = colorlinear(point.birth_time, min, max)
      point.chainColor = colorlinear(point.chain, minChain, maxChain)
      point.causeOfDeathColordark = colorcause(point.cause, 'dark')
      point.causeOfDeathColorlight = colorcause(point.cause, 'light')
      point.ageColor = colorage(point.age)
    })
    L.Util.setOptions(lifeAnimOverlay, {
      data: data,
      min: min,
      max: max,
    })
    lifeAnimOverlay.redraw()
    L.Util.setOptions(lifePointOverlay, {
      data: data,
      min: min,
      max: max,
    })
    lifePointOverlay.redraw()
    L.Util.setOptions(dataOverlay, {
      min: min,
      max: max,
    })
    L.Util.setOptions(legendControl, {
      min: min,
      max: max,
      minChain: minChain,
      maxChain: maxChain,
      data: data,
    })
    legendControl.redraw()
    if (sendRange && min && max) {
      app.ports.leafletEvent.send({
        kind: 'dataRange',
        min: min,
        max: max,
      })
    }
  }

  var setTheme = function(theme) {
    L.Util.setOptions(resultPoints, { theme: theme })
    L.Util.setOptions(lifeAnimOverlay, { theme: theme })
    lifeAnimOverlay.redraw()
    L.Util.setOptions(lifePointOverlay, { theme: theme })
    lifePointOverlay.redraw()
    L.Util.setOptions(legendControl, { theme: theme })
    legendControl.redraw()
    switch (theme) {
      case 'light':
        graticule.setLineColor('#111')
        break
      case 'dark':
        graticule.setLineColor('#eee')
        break
    }
  }

  L.GridLayer.ObjectSwatchOverlay = L.GridLayer.extend({
    options: {
      pane: 'overlayPane',
      minZoom: 24,
      maxZoom: 31,
      animate: false,
    },
    drawTile(tile, coords, time) {
      var layer = this
      var options = layer.options
      if (!tile._keyplace) {
        return tile
      }
      if ('time' in options) {
        time = time || options.time
      }
      if (!options.animate) time = undefined
      var tileSize = layer.getTileSize();
      var cellSize = Math.pow(2, coords.z - 24)

      var ctx = tile.getContext('2d');
      ctx.clearRect(0, 0, tile.width, tile.height)


      ctx.save()
      ctx.scale(cellSize, cellSize)
      ctx.translate(0.5, 0.5)
      var fadeTime = 60*60*1000
      var r = 5/Math.pow(cellSize, 0.8)
      tile._keyplace.forEach(function(placement) {
        //console.log(placement)

        var in_time = placement.t
        if (time && in_time) {
          var out_time = in_time + fadeTime
          if (time < in_time || out_time < time) {
            //console.log('out of range', in_time, time, out_time)
            return
          }

          var t = (time - in_time) / (out_time - in_time)
          var a = Math.pow(1 - t, 4)
          ctx.globalAlpha = a
        } else {
          ctx.globalAlpha = 0.5
        }

        ctx.fillStyle = colorobject(placement.id)

        ctx.save()
        ctx.beginPath();
        ctx.moveTo(placement.x - r, placement.y + 0)
        ctx.lineTo(placement.x - 0, placement.y + r)
        ctx.lineTo(placement.x + r, placement.y + 0)
        ctx.lineTo(placement.x + 0, placement.y - r)
        ctx.fill();
        ctx.restore()
      })
      ctx.restore()
    },
    updateTiles: function(ms) {
      L.Util.setOptions(this, {time: ms})
      for (var key in this._tiles) {
        var tile = this._tiles[key]
        this.drawTile(tile.el, tile.coords, ms)
      }
    },
  })

  L.GridLayer.KeyPlacementPoint = L.GridLayer.ObjectSwatchOverlay.extend({
    options: Object.assign({
      className: 'object-point-overlay-static',
      animate: false,
    }, objectGenerationOptions),
    initialize: function(cache, options) {
      this._cache = cache;
      options = L.Util.setOptions(this, options);
    },
    createTile: function (coords, done) {
      var layer = this
      var options = layer.options
      var highlightObjects = options.highlightObjectSwatches
      var tile = document.createElement('canvas');
      if (!highlightObjects) {
        if (done) done(null, tile)
        return tile
      }
      var tileSize = layer.getTileSize();
      tile.setAttribute('width', tileSize.x);
      tile.setAttribute('height', tileSize.y);
      //console.log(coords)


      //console.log('data tile ' + JSON.stringify(coords), options.dataTime, mapServer)
      layer._cache.getTile(coords, {time: options.dataTime, server: mapServer}, options).then(function(keyplace) {
        tile._keyplace = keyplace.filter(function(placement) {
          return highlightObjects.indexOf(placement.id) != -1
        })

        //console.timeEnd('data processing ' + JSON.stringify(coords))
        layer.drawTile(tile, coords)
        done(null, tile)
      })

      return tile
    },
  })

  L.GridLayer.MaplogPoint = L.GridLayer.ObjectSwatchOverlay.extend({
    options: Object.assign({
      time: 0,
      className: 'object-point-overlay-anim',
      animate: false,
    }, objectGenerationOptions),
    initialize: function(cache, options) {
      this._cache = cache;
      options = L.Util.setOptions(this, options);
    },
    createTile: function (coords, done) {
      var layer = this
      var options = layer.options
      var highlightObjects = options.highlightObjectSwatches
      var tile = document.createElement('canvas');
      if (!highlightObjects) {
        if (done) done(null, tile)
        return tile
      }
      var tileSize = layer.getTileSize();
      tile.setAttribute('width', tileSize.x);
      tile.setAttribute('height', tileSize.y);
      //console.log(coords)

      layer._cache.getTile(coords, {time: options.dataTime, server: mapServer}, {time: options.base, server: mapServer}, options).then(function(maplog) {

        tile._maplog = maplog

        var time = layer.options.time
        tile.coords = coords
        layer.tileAt(tile, time)
        done()
      })

      return tile
    },
    tileAt: function(tile, time) {
      var highlightObjects = this.options.highlightObjectSwatches
      if (!highlightObjects) {
        console.log('anim - no objects')
        return
      }
      var minSize = 1.5 * Math.pow(2, 31 - tile.coords.z)
      var z = tile.coords.z
      tile._keyplace = tileAt(tile, time)
        // base layer has objects of all sizes, but we can only be answerable for ones visible at our scale
        .filter(function(placement) {
          if (placement.floor) return true
          if (z >= 27) return true
          var size = objectSize[placement.id]
          var tooSmall = !size || size <= minSize
          if (tooSmall) return false
          return true
        })
        .filter(function(placement) {
          return highlightObjects.indexOf(placement.id) != -1
        })
      this.drawTile(tile, tile.coords)
    },
    updateTiles: function(ms) {
      L.Util.setOptions(this, {time: ms})
      for (var key in this._tiles) {
        var tile = this._tiles[key]
        if (tile.el._maplog) {
          this.tileAt(tile.el, ms)
        }
      }
    },
  })

  L.GridLayer.KeySearchPoint = L.GridLayer.ObjectSwatchOverlay.extend({
    options: Object.assign({
      className: 'object-index-overlay-static',
      minZoom: 2,
      maxZoom: 23,
      alsoShowImagePoints: false,
    }, objectGenerationOptions),
    initialize: function(cache, options) {
      this._cache = cache;
      options = L.Util.setOptions(this, options);
    },
    createTile: function (coords, done) {
      var layer = this
      var options = layer.options
      var highlightObjects
      if (options.alsoShowImagePoints) {
        highlightObjects = options.highlightObjectSwatches.concat(options.highlightObjectImages)
      } else {
        highlightObjects = options.highlightObjectSwatches
      }
      var tile = document.createElement('canvas');
      if (!highlightObjects) {
        if (done) done(null, tile)
        return tile
      }
      var tileSize = layer.getTileSize();
      tile.setAttribute('width', tileSize.x);
      tile.setAttribute('height', tileSize.y);
      //console.log(coords)


      //console.log('data tile ' + JSON.stringify(coords), options.dataTime, mapServer)
      layer._cache.getTile(coords, {time: options.dataTime, server: mapServer, highlightObjects: highlightObjects}, options).then(function(keyplace) {
        //console.log(keyplace)
        tile._keyplace = keyplace.filter(function(placement) {
          return highlightObjects.indexOf(placement.id) != -1
        })
        //console.log(tile._keyplace)

        //console.timeEnd('data processing ' + JSON.stringify(coords))
        layer.drawTile(tile, coords)
        done(null, tile)
      })

      return tile
    },
  })

  var moveIfOutOfView = function(data, map) {
    if (data.length < 1) return
    var bounds = map.getBounds()
    for (var i in data) {
      var point = data[i]
      if (bounds.contains([point.birth_y, point.birth_x])) {
        return
      }
    }
    var bounds = data.filter(function(point) {
      return (-4000000 < point.birth_x && point.birth_x < 4000000)
    }).map(function(point) {
      return [point.birth_y, point.birth_x]
    })
    map.fitBounds(bounds)
  }

  var specialOverlay = L.layerGroup([], {className: 'special-overlay'})
  specialOverlay.name = 'special overlay'
  overlays["Special Placements"] = specialOverlay;

  var baseLayerByTime = function(map, ms, reason) {
    //console.log(ms, reason)
    var targetWorld
    var targetSpan
    worlds.forEach(function(world) {
      //console.log(world.msStart, ms, world.msEnd, world.name)
      if (world.msStart < ms && (ms <= world.msEnd || world.msEnd == undefined)) {
        //console.log('pick', world)
        //console.log('pick', world.name)
        targetWorld = world
        world.spans.forEach(function(span) {
          if (span.msStart < ms && ms <= span.msEnd) {
            targetSpan = span
            //console.log('pick span')
          }
        })
        if (!targetSpan) targetSpan = world.spans[world.spans.length - 1]
      }
    })
    var changes = 0
    var layers = []
    var objectOverlayLayers = []
    var activityOverlayLayers = []
    if (targetWorld) {
      if (objectBounds.length > 0) {
        chooseRandPlacements(targetWorld)
        createWorldLayers(targetWorld)
      }
      //console.log(targetWorld.generation.placements)
      //console.log(targetWorld.generation.gridPlacements)
      //console.log(targetWorld.generation.biomes)
      if (targetWorld.generation.placements) {
        updatePlacementLayer(specialOverlay, targetWorld.generation.placements)
      }

      layers = [
        biomeLayerVisible && targetWorld.biomeLayer,
        !targetSpan && targetWorld.objectLayer,
        !dataAnimated && targetSpan && targetSpan.keyPlacementLayer,
        dataAnimated && targetSpan && targetSpan.maplogLayer,
      ].filter(function(x) {return !!x})

      objectOverlayLayers = [
        !dataAnimated && targetSpan && targetSpan.keyPlacementPoint,
        !dataAnimated && targetSpan && targetSpan.keySearchPoint,
        !dataAnimated && targetSpan && targetSpan.keySearchImage,
        dataAnimated && targetSpan && targetSpan.maplogPoint,
        dataAnimated && targetSpan && targetSpan.maplogSearchPoint,
      ].filter(function(x) {return !!x})

      activityOverlayLayers = [
        targetSpan && targetSpan.actmap,
      ].filter(function(x) {return !!x})
    }
    oholBase.eachLayer(function(layer) {
      if (layers.indexOf(layer) == -1) {
        //console.log('remove', layer && layer.name)
        oholBase.removeLayer(layer)
        changes++
      }
    })
    layers.forEach(function(layer) {
      if (!oholBase.hasLayer(layer)) {
        //console.log('add', layer.name)
        oholBase.addLayer(layer)
        changes++
      }
    })
    objectOverlay.eachLayer(function(layer) {
      if (objectOverlayLayers.indexOf(layer) == -1) {
        //console.log('remove', layer && layer.name)
        objectOverlay.removeLayer(layer)
        changes++
      }
    })
    objectOverlayLayers.forEach(function(layer) {
      if (!objectOverlay.hasLayer(layer)) {
        //console.log('add', layer.name)
        objectOverlay.addLayer(layer)
        changes++
      }
    })
    activityOverlay.eachLayer(function(layer) {
      if (activityOverlayLayers.indexOf(layer) == -1) {
        //console.log('remove', layer && layer.name)
        activityOverlay.removeLayer(layer)
      }
    })
    activityOverlayLayers.forEach(function(layer) {
      if (!activityOverlay.hasLayer(layer)) {
        //console.log('add', layer.name)
        activityOverlay.addLayer(layer)
      }
    })
    if (changes > 0) {
      setTimeout(function() {
        toggleAnimationControls(map)
      },0)
    }
  }

  var riftLayerByTime = function(ms, zoom) {
    var targetLayer = null
    barrierRadius = null
    for (var i = riftHistory.length - 1;i >= 0;i--) {
      if (ms > riftHistory[i].ms) {
        if (!zoom || zoom < objectLayerOptions.showNaturalObjectsAboveZoom) {
          targetLayer = riftHistory[i].layer
        }
        barrierRadius = riftHistory[i].radius
        break
      }
    }
    riftHistory.forEach(function(rh) {
      if (riftOverlay.hasLayer(rh.layer)) {
        if (rh.layer != targetLayer) {
          riftOverlay.removeLayer(rh.layer)
        }
      } else {
        if (rh.layer == targetLayer) {
          riftOverlay.addLayer(rh.layer)
        }
      }
    })
  }

  var bandLayerByTime = function(ms) {
    var targetLayer = null
    for (var i = bandHistory.length - 1;i >= 0;i--) {
      if (ms > bandHistory[i].ms) {
        targetLayer = bandHistory[i].layer
        break
      }
    }
    bandHistory.forEach(function(bh) {
      if (bandsOverlay.hasLayer(bh.layer)) {
        if (bh.layer != targetLayer) {
          bandsOverlay.removeLayer(bh.layer)
        }
      } else {
        if (bh.layer == targetLayer) {
          bandsOverlay.addLayer(bh.layer)
        }
      }
    })
  }

  var mapUpdateTask = null
  var setMapTime = function(map, ms, reason) {
    mapTime = ms
    if (mapUpdateTask) {
      clearTimeout(mapUpdateTask)
    }
    mapUpdateTask = setTimeout(function() {
      if (map) baseLayerByTime(map, ms, reason)
      if (monumentOverlay) monumentsByTime(monumentOverlay, ms, reason)
      riftLayerByTime(ms, map && map.getZoom())
      bandLayerByTime(ms)
      lifeAnimOverlay.updateTiles(ms)
      arcUpdateTiles(ms)
      L.Util.setOptions(legendControl, {time: ms})
      legendControl.redraw()
    }, 10)
  }

  var setMapServer = function(map, server, reason) {
    mapServer = server
    if (server == 3) {
      if (badlandsAge.hasLayer(badlandsBaseBiome)) {
        badlandsAge.addLayer(server3)
        badlandsAge.removeLayer(badlandsBaseBiome)
      }
    } else {
      if (badlandsAge.hasLayer(server3)) {
        badlandsAge.addLayer(badlandsBaseBiome)
        badlandsAge.removeLayer(server3)
      }
    }
    setMapTime(map, mapTime, reason)
  }

  var setPointColor = function(color) {
    L.Util.setOptions(lifeAnimOverlay, {
      color: color,
    })
    lifeAnimOverlay.redraw()
    L.Util.setOptions(lifePointOverlay, {
      color: color,
    })
    lifePointOverlay.redraw()
    L.Util.setOptions(legendControl, {
      color: color,
    })
    legendControl.redraw()
  }

  var setPointLocation = function(location) {
    L.Util.setOptions(lifeAnimOverlay, {
      location: location,
    })
    lifeAnimOverlay.redraw()
    L.Util.setOptions(lifePointOverlay, {
      location: location,
    })
    lifePointOverlay.redraw()
  }

  lifeAnimOverlay.on('add', function(ev) {
    ev.target.addInteractiveTarget(ev.target._container)
  })
  lifeAnimOverlay.on('click', lifeAnimOverlay.selectPoints)

  lifePointOverlay.on('add', function(ev) {
    ev.target.addInteractiveTarget(ev.target._container)
  })
  lifePointOverlay.on('click', lifePointOverlay.selectPoints)

  resultPoints.on('add', function(ev) {
    ev.target.setZIndex(450)
  })

  var options = {
    showOriginLabel: false,
    redraw: 'move',
    attribution: '<a href="https://github.com/ablakey/Leaflet.SimpleGraticule">ablakey/SimpleGraticule</a>',
    zoomIntervals: [
      {start: 0,  end: 3,  interval: 1000000000},
      {start: 4,  end: 6,  interval: 100000000},
      {start: 7,  end: 9,  interval: 10000000},
      {start: 10, end: 13, interval: 1000000},
      {start: 14, end: 16, interval: 100000},
      {start: 17, end: 19, interval: 10000},
      {start: 20, end: 21, interval: 1000},
      {start: 22, end: 23, interval: 200},
      {start: 24, end: 29, interval: 40},
    ]
  };

  var graticule = L.simpleGraticule(options)
  overlays['graticule'] = graticule

  var layersControl = L.control.layers(base, overlays, {autoZIndex: false})

  L.Control.Scale.include({
    _updateMetric: function (maxMeters) {
      var meters = this._getRoundNum(maxMeters);
      var label = meters;
      if (meters < 1000) {
        label = meters;
      } else if (meters < 1000000) {
        label = (meters / 1000) + 'K';
      } else if (meters < 1000000000) {
        label = (meters / 1000000) + 'M';
      } else {
        label = (meters / 1000000000) + 'G';
      }

      label = label + ' z' + this._map.getZoom();

      this._updateScale(this._mScale, label, meters / maxMeters);
    }
  })

  L.Control.MapButton = L.Control.extend({
      options: {
        title: 'Button',
        icon: 'filter',
        message: 'mapButton',
      },
      onAdd: function(map) {
        return this._initLayout()
      },
      onRemove: function(map) {
        // Nothing to do here
      },
      _initLayout: function () {
        var className = 'leaflet-control-map-button'
        var container = this._container = L.DomUtil.create('div', className)

        var link = L.DomUtil.create('a', className + '-body', container);
        this.redraw(this.options.icon)

        L.DomEvent.on(link, 'click', this.toggle, this);

        return container;
      },
      toggle: function(e) {
        app.ports.leafletEvent.send({
          kind: this.options.message,
        })
        L.DomEvent.preventDefault(e)
      },
      setChecked: function(checked){
        if (checked) this.redraw('cancel-circle')
        else this.redraw(this.options.icon)
      },
      redraw: function(icon) {
        var link = this._container.firstChild
        link.href = '#';
        link.title = this.options.title
        link.innerHTML = '<svg class="icon icon-' + icon + '"><use xlink:href="symbol-defs.svg#icon-' + icon + '"></use></svg>'
      },
  });

  L.control.mapButton = function(opts) {
    return new L.Control.MapButton(opts);
  }

  L.Control.MapToggle = L.Control.extend({
      options: {
        off: {
          title: 'Button',
          icon: 'filter',
        },
        off: {
          title: 'Button',
          icon: 'cancle-circle',
        },
        message: 'mapButton',
      },
      onAdd: function(map) {
        return this._initLayout()
      },
      onRemove: function(map) {
        // Nothing to do here
      },
      _initLayout: function () {
        var className = 'leaflet-control-map-toggle'
        var container = this._container = L.DomUtil.create('div', className)

        var off = L.DomUtil.create('a', className + '-off', container);
        var on = L.DomUtil.create('a', className + '-on', container);
        this.redraw(this.options.off.icon)

        L.DomEvent.on(off, 'click', this.toggle, this);
        L.DomEvent.on(on, 'click', this.toggle, this);

        return container;
      },
      toggle: function(e) {
        app.ports.leafletEvent.send({
          kind: this.options.message,
        })
        L.DomEvent.preventDefault(e)
      },
      setChecked: function(checked){
        this.redraw(checked)
      },
      redraw: function(checked) {
        this.redrawIcon(this._container.firstChild, this.options.off, !checked)
        this.redrawIcon(this._container.lastChild, this.options.on, checked)
      },
      redrawIcon: function(node, opt, highlight) {
        node.href = '#';
        node.title = opt.title
        node.innerHTML = '<svg class="icon icon-' + opt.icon + (highlight ? ' leaflet-control-map-toggle-highlight' : ' ') + '"><use xlink:href="symbol-defs.svg#icon-' + opt.icon + '"></use></svg>'
      },
  });

  L.control.mapToggle = function(opts) {
    return new L.Control.MapToggle(opts);
  }


  var sidebarToggle = L.control.mapButton({
    title: 'Search and Options',
    icon: 'filter',
    message: 'sidebarToggle',
    position: 'bottomright'
  })
  var timelineToggle = L.control.mapButton({
    title: 'Detailed Time Selection',
    icon: 'time',
    message: 'timelineToggle',
    position: 'bottomleft'
  })
  var animToggle = L.control.mapToggle({
    off: {
      title: 'Snapshot',
      icon: 'image',
    },
    on: {
      title: 'Animated',
      icon: 'film',
    },
    message: 'animToggle',
    position: 'bottomleft'
  })

  L.Control.Legend = L.Control.extend({
    options: {
      color: 'lineageColor',
      theme: 'dark',
      dataAnimated: false,
      time: mapTime,
    },
    onAdd: function(map) {
      return this._initLayout()
    },
    onRemove: function(map) {
      // Nothing to do here
    },
    _initLayout: function () {
      var className = 'leaflet-control-color-scale'
      var container = this._container = L.DomUtil.create('div', className)
      this.redraw()

      return container;
    },
    redraw: function() {
      var container = this._container
      if (!container) return
      if (!this._map || !this._map.hasLayer(dataOverlay)) return
      var options = this.options
      if (options.color != 'lineageColor' && this._color == options.color && this._theme == options.theme) return
      this._color = options.color
      this._theme = options.theme
      while (container.firstChild) {
        container.removeChild(container.firstChild);
      }
      switch (options.color) {
        case 'lineageColor':
          this.updateLineages()
          var maxHeight = this._map.getSize().y - 256
          var height = 0
          var lineages = Object.values(options.lineages || {})
            .sort(function(a, b) {
            return b.chain - a.chain
            })
          for (var i in lineages) {
            var life = lineages[i]
            var swatch = L.DomUtil.create('a', 'swatch leaflet-interactive', container)
            if (height > maxHeight) {
              swatch.style = 'color: black'
              swatch.innerHTML = "...."
              break
            }
            var foreground = 'color: ' + (options.theme == 'light' ? 'white' : 'black')
            var background = 'background-color: ' + colorlineage(life.lineage, options.theme)
            swatch.style = foreground + ';' + background
            if (life.name) {
              var words = life.name.split(' ')
              swatch.innerHTML = (words[1] || 'unnamed')
            }
            height += 22
            L.DomEvent.on(swatch, 'click', focusLineage(life.lineage, this._map), this);
          }
          break;
        case 'birthTimeColor':
          var swatch = L.DomUtil.create('div', 'swatch', container)
          swatch.style = 'background-color: ' + colorlinear(options.min, options.min, options.max)
          swatch.innerHTML = 'older';

          swatch = L.DomUtil.create('div', 'swatch', container)
          swatch.style = 'background-color: ' + colorlinear(options.max, options.min, options.max) + '; color: black;'
          swatch.innerHTML = 'newer';
          break;
        case 'chainColor':
          var swatch = L.DomUtil.create('div', 'swatch', container)
          swatch.style = 'background-color: ' + colorlinear(options.minChain, options.minChain, options.maxChain)
          swatch.innerHTML = options.minChain;

          swatch = L.DomUtil.create('div', 'swatch', container)
          swatch.style = 'background-color: ' + colorlinear(options.maxChain, options.minChain, options.maxChain) + '; color: black;'
          swatch.innerHTML = options.maxChain;
          break;
        case 'causeOfDeathColor':
          [
            'killer',
            'hunger',
            'oldAge',
            'disconnect',
            'unknown',
          ].forEach(function(cause) {
            var swatch = L.DomUtil.create('div', 'swatch', container)
            swatch.style = 'background-color: ' + colorcause(cause, options.theme)
            swatch.innerHTML = cause;
          })
          break;
        case 'ageColor':
          [
            0,
            14,
            20,
            30,
            40,
            50,
            60,
          ].forEach(function(age) {
            var swatch = L.DomUtil.create('div', 'swatch', container)
            swatch.style = 'background-color: ' + colorage(age)
            swatch.innerHTML = age.toString();
          })
          break;
      }
    },
    updateLineages: function() {
      var dataChange = this._data != this.options.data
      if (!dataChange && this._time == this.options.time && this._dataAnimated == this.options.dataAnimated) return
      if (dataChange) {
        this._defaultLineages = calculateLineages(false, 0, {}, this.options.data)
      }
      L.Util.setOptions(this, {lineages: calculateLineages(this.options.dataAnimated, this.options.time, this._defaultLineages, this.options.data)})
      this._time = this.options.time
      this._dataAnimated = this.options.dataAnimated
      this._data = this.options.data
    },
    getLineages: function() {
      this.updateLineages()
      return this.options.lineages || {}
    }
  });

  L.control.legend = function(opts) {
    return new L.Control.Legend(opts);
  }

  var legendControl = L.control.legend({ position: 'topleft' })

  var calculateLineages = function(dataAnimated, ms, defaults, data) {
    var time = ms/1000
    var lineages = {}
    for (var i in data) {
      var point = data[i]
      if (dataAnimated) {
        var death_time = (point.death_time || (point.birth_time + 3600))
        if (time < point.birth_time || death_time < time) {
          continue;
        }
      }

      var key = (point.lineage || 0).toString()
      var prior = lineages[key]
      if (prior && prior.name && !point.name) continue;
      if (!prior || !prior.name || point.chain > prior.chain) {
        lineages[key] = point
      }
    }

    for (var key in lineages) {
      if (!defaults[key]) continue
      var prior = lineages[key]
      if (!prior || !prior.name) {
        lineages[key] = defaults[key]
      }
    }

    return lineages
  }

  var focusLineage = function(lineageId, map) {
    return function(e) {
      app.ports.leafletEvent.send({
        kind: 'selectLineage',
        lineage: lineageId,
      })
    }
  }

  L.Control.PointLegend = L.Control.extend({
    options: {
    },
    onAdd: function(map) {
      return this._initLayout()
    },
    onRemove: function(map) {
      // Nothing to do here
    },
    _initLayout: function () {
      var className = 'leaflet-control-point-legend'
      var container = this._container = L.DomUtil.create('div', className)
      this.redraw()

      return container;
    },
    redraw: function() {
      var container = this._container
      if (!container) return
      if (!this._map || !this._map.hasLayer(dataOverlay)) return
      while (container.firstChild) {
        container.removeChild(container.firstChild);
      }
      [
        ['stop2', 'Male'],
        ['circle', 'Female'],
        ['radio-checked2', 'Fertile'],
      ].forEach(function(pair) {
        var line = L.DomUtil.create('div', null, container)
        line.innerHTML = '<svg class="icon icon-'+pair[0]+'"><use xlink:href="symbol-defs.svg#icon-'+pair[0]+'"></use></svg> ' + pair[1]
      })
    },
  });

  L.control.pointLegend = function(opts) {
    return new L.Control.PointLegend(opts);
  }

  var pointLegendControl = L.control.pointLegend({ position: 'topleft' })


      // object blocking
      //console.log('-2,1', getMapObjectRaw(-2, 1, objectGenerationOptions))
      // off biome moving object
      //console.log('19,15', getBaseMap(19, 15, objectGenerationOptions))
      // object bounds height transparent padding
      //console.log('80,8', getMapObjectRaw(80, 8, objectGenerationOptions))

  var toggleAnimated = function(parent, status) {
    parent.eachLayer(function(layer) {
      if (status) {
        if (layer.options.alternateAnim) {
          parent.addLayer(layer.options.alternateAnim)
          parent.removeLayer(layer)
        }
      } else {
        if (layer.options.alternateStatic) {
          parent.addLayer(layer.options.alternateStatic)
          parent.removeLayer(layer)
        }
      }
    })
  }

  var addControl = function(map, control) {
    if (control._map != map) {
      map.addControl(control)
    }
  }

  var removeControl = function(map, control) {
    if (control._map == map) {
      map.removeControl(control)
    }
  }

  var toggleAnimationControls = function(map) {
    var animated = 0
    var stat = 0
    map.eachLayer(function(layer) {
      if (layer.options.alternateAnim) {
        stat++
      }
      if (layer.options.alternateStatic) {
        animated++
      }
    })
    if (animated > 0) {
      addControl(map, animToggle)
      animToggle.setChecked(true)
      app.ports.leafletEvent.send({
        kind: 'dataAnimated',
        animated: 'animated',
      })
    } else if (stat > 0) {
      addControl(map, animToggle)
      animToggle.setChecked(false)
      app.ports.leafletEvent.send({
        kind: 'dataAnimated',
        animated: 'static',
      })
    } else {
      removeControl(map, animToggle)
      app.ports.leafletEvent.send({
        kind: 'dataAnimated',
        animated: 'inert',
      })
    }
  }

  var toggleSidebarControls = function(map, status) {
    sidebarToggle.setChecked(status == "life")
  }

  var inhabit = function inhabit(id) {
    var map = L.map(id, {
      crs: crs,
      maxBounds: [[-2147483648, -2147483648], [2147483647, 2147483647]],
      minZoom: 2,
      maxZoom: 31,
    })
    map.createPane('baseimagePane');
    map.createPane('objectPane');
    map.createPane('activityPane');
    map.createPane('landmarkPane');

    map.on('zoomend', function(ev) {
      riftLayerByTime(mapTime, map.getZoom())
    })
    map.on('baselayerchange', function(ev) {
      toggleAnimationControls(map)
    })
    map.on('overlayadd', function(ev) {
      toggleAnimationControls(map)
    })
    map.on('overlayremove', function(ev) {
      toggleAnimationControls(map)
    })
    map.on('resize', function(ev) {
      legendControl.redraw()
    })

    var idle = false
    var setIdle = function() {
      L.DomUtil.addClass(map._controlContainer, 'idle')
      idle = true
    }

    var setActive = function() {
      if (idle) {
        L.DomUtil.removeClass(map._controlContainer, 'idle')
      }
      idle = false
      if (idleTimer) {
        clearTimeout(idleTimer)
      }
      idleTimer = setTimeout(setIdle, 1*60*5000)
    }

    var idleTimer = setTimeout(setIdle, 1*60*5000)
    L.DomEvent.on(map, 'mousemove', setActive, map);

    var t = Date.now()
    setMapTime(map, t, 'inhabit')
    oholBase.addTo(map)
    overlays['Activity Map'].addTo(map)
    overlays['Biomes'].addTo(map)
    //base['Topographic Test'].addTo(map)
    overlays['Rift'].addTo(map)
    //overlays['Bands'].addTo(map)
    overlays['Notable'].addTo(map)
    //overlays['Checker'].addTo(map)
    //overlays['graticule'].addTo(map)
    //base['Fractal'].addTo(map)
    //base['Biome'].addTo(map)
    clusterOverlay.addTo(map)

    //map.addControl(animToggle)

    layersControl.addTo(map)
    L.control.scale({imperial: false}).addTo(map)
    sidebarToggle.addTo(map)
    timelineToggle.addTo(map)
    //map.setView([0,0], 24)

    // --------------- Leaflet --------------------
    if (app.ports.leafletEvent) {
      map.on('moveend', function(ev) {
        var center = ev.target.getCenter()
        app.ports.leafletEvent.send({
          kind: 'moveend',
          x: Math.round(center.lng),
          y: Math.round(center.lat),
          z: ev.target.getZoom()
        })
      })
      map.on('zoomend', function(ev) {
        var center = ev.target.getCenter()
        app.ports.leafletEvent.send({
          kind: 'zoomend',
          z: ev.target.getZoom()
        })
      })
      map.on('overlayadd', function(ev) {
        app.ports.leafletEvent.send({
          kind: 'overlayadd',
          name: ev.name,
          server_id: ev.layer.options.server_id,
        })
      })
      map.on('overlayremove', function(ev) {
        app.ports.leafletEvent.send({
          kind: 'overlayremove',
          name: ev.name,
        })
      })
    }

    var positionSet = false

    var command = function(message) {
      try {
        //console.log(message)
        switch (message.kind) {
          case 'setView':
            if (positionSet) {
              var center = map.getCenter()
              if (center.lng != message.x || center.lat != message.y || map.getZoom() != message.z) {
                map.setView([message.y, message.x], message.z)
              }
            } else {
              map.setView([message.y, message.x], message.z)
              positionSet = true
            }
            break
          case 'currentTime':
            var time = message.time * 1000
            setMapTime(map, time, 'currentTime')
            break;
          case 'currentServer':
            setMapServer(map, message.serverId, 'currentServer')
            break;
          case 'worldList':
            updateWorlds(message.worlds.data)
            baseLayerByTime(map, mapTime, 'worldList')
            break;
          case 'objectBounds':
            objectBounds = new Array(message.ids.length)
            for (var i = 0;i < message.ids.length;i++) {
              var id = parseInt(message.ids[i])
              var bounds = message.bounds[i]
              objectBounds[id] = bounds
              objectSize[id] = Math.min(
                bounds[2] - bounds[0] - 30,
                bounds[3] - bounds[1] - 30)
            }
            baseLayerByTime(map, mapTime, 'objectbounds')
            toggleAnimationControls(map)
            //objectOverlayPixel.addTo(map)
            //objectOverlaySprite.addTo(map)
            break;
          case 'monumentList':
            updateMonumentLayer(monumentOverlay, message.monuments.data)
            monumentsByTime(monumentOverlay, mapTime, 'monumentList')
            monumentOverlay.addTo(map)
            break;
          case 'notableObjects':
            updateNotableLayer(notableOverlay, message.locations)
            break;
          case 'dataLayer':
            setDataLayers(message.lives.data, message.sendRange)
            moveIfOutOfView(message.lives.data, map)
            if (message.lives.data
             && message.lives.data.length > 0
             && !map.hasLayer(dataOverlay)) {
              map.addLayer(dataOverlay)
            }
            break;
          case 'displayResults':
            var data = message.lives.data
            L.Util.setOptions(resultPoints, {
              data: data,
            })
            resultPoints.redraw()
            break;
          case 'displayClusters':
            updateClusterLayer(clusterOverlay, message.clusters, map.getZoom())
            //clusterOverlay.addTo(map)
            break;
          case 'focusLife':
            var life = message.life;
            if (focusMarker) {
              focusMarker.remove();
            }
            focusMarker = L.marker([life.birth_y, life.birth_x])
              .bindPopup(life.name)
              .addTo(lifeSearchOverlay)
              .openPopup()
            map.setView([life.birth_y, life.birth_x])
            var ms = life.birth_time*1000
            setMapTime(map, ms, 'focus life')
            break
          case 'focusPoint':
            if (focusMarker) {
              focusMarker.remove();
            }
            focusMarker = L.marker([message.y, message.x])
              .addTo(objectOverlay)
            map.setView([message.y, message.x])
            break
          case 'focusPlacement':
            if (focusMarker) {
              focusMarker.remove();
            }
            focusMarker = L.marker([message.y, message.x])
              .addTo(objectOverlay)
            map.setView([message.y, message.x])
            setMapTime(map, message.t, 'focus placement')
            break
          case 'focusNone':
            if (focusMarker) {
              focusMarker.remove();
            }
            break
          case 'lifeSearchOverlay':
            if (message.status) {
              lifeSearchOverlay.addTo(map)
            } else {
              lifeSearchOverlay.remove()
            }
            break
          case 'objectSearchOverlay':
            if (message.status) {
              objectOverlay.addTo(map)
            } else {
              objectOverlay.remove()
            }
            break
          case 'highlightObjects':
            setObjectHighlightOptions({highlightObjectSwatches: message.swatches, highlightObjectImages: message.images})
            break
          case 'animOverlay':
            dataAnimated = message.status
            // base layer does this for the object image layer, which has no alterate to toggle
            //  toggleAnimated(objectOverlay, message.status)
            //  toggleAnimated(oholBase, message.status)
            // life layer doesn't transition without these, but base layer seems like it should take care of it.
            toggleAnimated(dataOverlay, message.status)
            baseLayerByTime(map, mapTime, 'animOverlay')
            L.Util.setOptions(legendControl, {dataAnimated: message.status})
            legendControl.redraw()
            toggleAnimationControls(map)
            break
          case 'timelineVisible':
            timelineVisible = message.status
            timelineToggle.setChecked(message.status)
            break
          case 'sidebar':
            toggleSidebarControls(map, message.sidebar)
            break
          case 'overlayVisible':
            for (var name in overlays) {
              if (name == message.layer) {
                if (message.status) {
                  if (!map.hasLayer(overlays[name])) map.addLayer(overlays[name])
                } else {
                  if (map.hasLayer(overlays[name])) map.removeLayer(overlays[name])
                }
              }
            }
            break;
          case 'pointColor':
            setPointColor(message.color)
            break;
          case 'pointLocation':
            setPointLocation(message.location)
            break;
          case 'changeTheme':
            setTheme(message.theme)
            break;
          case 'showOnlyCurrentMonuments':
            L.Util.setOptions(monumentOverlay, {showOnlyCurrentMonuments: message.status})
            monumentsByTime(monumentOverlay, mapTime, 'showOnlyCurrentMonuments')
            break
          case 'fadeTallObjects':
            setObjectLayerOptions({fadeTallObjects: message.status})
            break
          case 'showNaturalObjectsAboveZoom':
            setObjectLayerOptions({showNaturalObjectsAboveZoom: message.zoom})
            riftLayerByTime(mapTime, map.getZoom())
            break
          case 'activityMapSampleSize':
            actmapSampleSize = message.sampleSize
            setActivityMapOptions(actmapLayerOptions(actmapSampleSize))
            break
          case 'showActivityMapBelowZoom':
            actmapMaxZoom = message.zoom;
            setActivityMapOptions({maxZoom: actmapMaxZoom})
            break
          default:
            console.log('unknown message', message)
            break;
        }
      } catch (e) {
        console.error(e);
      }
    }

    if (app.ports.leafletCommand) {
      app.ports.leafletCommand.subscribe(command)
    }
  }

  inhabit('map')

  // --------------- LocalStorage --------------------
  var key = 'ohol-map'

  app.ports.localStorageSave.subscribe(function(value) {
    localStorage.setItem(key, value)
  })

  var loaded = localStorage.getItem(key)
  app.ports.localStorageLoaded.send(loaded)

  // --------------- Log --------------------
  if (app.ports.logCommand) {
    app.ports.logCommand.subscribe(function(message) {
      switch(message.kind) {
        case 'debug':
          console.debug(message.note, message.value)
          break
        case 'info':
          console.info(message.note, message.value)
          break
        case 'warn':
          console.warn(message.note, message.value)
          break
        case 'error':
          console.error(message.note, message.value)
          break
        default:
          console.log('unknown message', message)
          break;
      }
    })
  }

  /*
  var testSource = new CustomRandomSource(124567)
  for (var i = 0;i< 3;i++) {
    console.log('sample', i, testSource.getRandomBoundedInt(-352,352))
  }
  */
})()
