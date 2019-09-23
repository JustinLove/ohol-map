"use strict";

;(function() {
  var app = Elm.MapUI.init({flags: oholMapConfig})

  var cachedApiUrl = oholMapConfig.cachedApiUrl
  var apiUrl = oholMapConfig.apiUrl

  var msStartOfArcticAge = Date.parse("2018-03-08")
  var msStartOfDesertAge = Date.parse("2018-03-31")
  var msStartOfJungleAge = Date.parse("2018-11-19")
  var msStartOfRandomAge = Date.parse("Jul 27 2019 21:00:00 GMT-0000")
  var msStartOfTopographicAge = Date.parse("Jul 31 2019 01:25:24 GMT-0000")
  var msStartOfSpecialAge = Date.parse("Aug 1 2019 02:08:47 GMT-0000")

  var arcs = []

  var CELL_D = 128
  var objectBoundsPadding = 15

  var scale = Math.pow(2, 24)
  var crs = L.extend({}, L.CRS.Simple, {
    transformation: new L.transformation(1/scale, 0.5/scale, -1/scale, -0.5/scale)
  })

  var mapTime = Date.now()
  var dataAnimated = false

  var base = {};

  var attribution = '<a href="https://onehouronelife.com">Jason Rohrer</a> wondible ' +
    '<a href="https://twitter.com/wondible" title="@wondible"><svg class="icon icon-twitter"><use xlink:href="symbol-defs.svg#icon-twitter"></use></svg></a>' +
    '<a href="https://github.com/JustinLove/ohol-map" title="Frontend: ohol-map"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>' +
    '<a href="https://github.com/JustinLove/ohol-data-server" title="Backend: ohol-data-server"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>' +
    '<a href="https://github.com/JustinLove/OneLife/tree/mapping" title="Tile generation: OneLife/mapping"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>' +
    '<a href="https://github.com/JustinLove/onetech/tree/mapping" title="Object images based on Onetech"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>'
  var baseAttributionLayer = L.layerGroup([], {attribution: attribution})

  var biomeImageLayer = L.tileLayer(oholMapConfig.mainTiles, {
    className: 'crisp',
    errorTileUrl: 'ground_U.png',
    minZoom: 2,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
  })

  var screenshotImageLayer = L.tileLayer(oholMapConfig.mainTiles, {
    //errorTileUrl: 'ground_U.png',
    minZoom: 2,
    minZoom: 25,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 29,
    attribution: attribution,
    bounds: [[-512, -512], [511, 511]],
  })

  base['Badlands Age'] = null
  base['Arctic Age'] = null
  base['Desert Age'] = null
  base['Jungle Age'] = L.layerGroup([biomeImageLayer, screenshotImageLayer])
  base['Arc Age'] = L.layerGroup([])
  base['Uncertainty'] = L.layerGroup([])

  base['Crucible'] = L.tileLayer(oholMapConfig.crucibleTiles, {
    className: 'crisp',
    errorTileUrl: 'ground_U.png',
    minZoom: 2,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 25,
    attribution: attribution,
  });

  var dataOverlay = L.layerGroup([])
  dataOverlay.on('add', function(ev) {
    var map = ev.target._map
    legendControl.redraw()
    map.addControl(legendControl)
    map.addControl(pointLegendControl)
    setTimeout(function() {
      toggleAnimationControls(map)
      map.addLayer(baseFade)
    }, 0)
  })
  dataOverlay.on('remove', function(ev) {
    var map = ev.target._map
    map.removeControl(legendControl)
    map.removeControl(pointLegendControl)
    setTimeout(function() {
      toggleAnimationControls(map)
      map.removeLayer(baseFade)
    })
  })

  var baseFade = L.layerGroup([])
  baseFade.on('add', function(ev) {
    var map = ev.target._map
    L.DomUtil.setOpacity(map.getPane('tilePane'), 0.3)
    // moving the map at 28+ (varies with blur radius?) causes the current tile to blank out with blur
    if (map.getZoom() < 28) {
      L.DomUtil.addClass(map.getPane('tilePane'), 'blur')
    }
  })
  baseFade.on('remove', function(ev) {
    var map = ev.target._map
    L.DomUtil.setOpacity(map.getPane('tilePane'), 1.0)
    L.DomUtil.removeClass(map.getPane('tilePane'), 'blur')
  })

  var monumentOverlay = L.layerGroup([])

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
  ]

  var overlays = {
    graticule: null,
    "Rift": riftOverlay,
    "Life Data": dataOverlay,
    "Monuments": monumentOverlay,
    "Fade": baseFade,
  }

  var searchOverlay = L.layerGroup([])
  var focusMarker = null;

  var updateMonumentLayer = function(layer, data) {
    var now = new Date()
    layer.clearLayers()
    L.Util.setOptions(layer, {data: data})
    data.forEach(function(point) {
      var date = new Date(point.date*1000)
      var age = (now - date) / (24 * 60 * 60 * 1000)
      L.marker([point.y, point.x], {
          opacity: Math.max(0.4, Math.min(1.0, 1.0 - (age / (age+30))))
        })
        .bindPopup(date.toString())
        .addTo(layer)
      //L.circle([point.y, point.x], {radius: 21000, fill: false}).addTo(layer)
    })
  }

  L.GridLayer.CheckerLayer = L.GridLayer.extend({
    options: {
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

  overlays['Checker'] = new L.GridLayer.CheckerLayer()


  // fractal generation copying https://github.com/jasonrohrer/OneLife/blob/master/commonSource/fractalNoise.cpp
  // which cites https://bitbucket.org/runevision/random-numbers-testing/

  var XX_PRIME32_1 = 2654435761
  var XX_PRIME32_2 = 2246822519
  var XX_PRIME32_3 = 3266489917
  //var XX_PRIME32_4 = 668265263
  var XX_PRIME32_5 = 374761393

  var xxSeed = 0;

  var hex = function(x) {
    if (x < 0) {
      return (-x>>16 ^ 0xffff).toString(16).padStart(4, '0') + (x&0xffff).toString(16).padStart(4, '0') + ' ' + x
    } else {
      return x.toString(16).padStart(8, '0') + ' ' + x
    }
  }

  var xxTweakedHash2D = function(inX, inY) {
    var h32 = xxSeed + inX + XX_PRIME32_5
    h32 += Math.imul(inY, XX_PRIME32_3)
    h32 = Math.imul(h32, XX_PRIME32_2)
    h32 ^= h32 >>> 13
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

  var biomes = []
  var objects = []
  var objectBounds = []
  var objectSize = []
  var gridPlacements = []

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

      xxSeed = biome * options.biomeSeedScale + options.biomeSeedOffset
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

  var topographicMapBiomeIndex = function(inX, inY, options, secondPlace) {
    var numBiomes = options.biomeMap.length
    var regularBiomesLimit = numBiomes - options.numSpecialBiomes
    var scale = options.biomeOffset + options.biomeScale * numBiomes
    var roughness = options.biomeFractalRoughness
    var weights = options.biomeCumuWeights

    xxSeed = options.biomeSeedOffset
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
      pickedBiome = -1
      scale = options.biomeSpecialOffset + options.biomeSpecialScale * options.numSpecialBiomes
      roughness = options.biomeSpecialRoughness

      var maxValue = -10
      var secondMaxValue = -10
      var secondPlaceBiome = -1

      for (var i = regularBiomesLimit;i < numBiomes;i++) {
        var biome = options.biomeMap[i]

        xxSeed = biome * options.biomeSeedScale + options.biomeSeedOffset + options.biomeSeedSpecialOffset

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
    } else {
      secondPlaceBiome = pickedBiome - 1
      if (secondPlaceBiome < 0) {
        secondPlaceBiome = pickedBiome + 1
      }
    }

    if (secondPlace) {
      secondPlace.biome = secondPlaceBiome
      secondPlace.gap = 0.1
    }

    return pickedBiome
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
    for (var i = 0;i < gridPlacements.length;i++) {
      var gp = gridPlacements[i]
      if (inX % gp.spacing == 0 && inY % gp.spacing == 0) {
        pickedBiome = options.computeMapBiomeIndex(inX, inY, options, secondPlace)
        if (pickedBiome == -1) {
          return 0;
        }

        if (gp.permittedBiomes.indexOf(pickedBiome) != -1) {
          if (grid) grid.grid = true
          return gp.id
        }
      }
    }

    xxSeed = options.densitySeed
    var density = getXYFractal(inX, inY, options.densityRoughness, options.densityScale);
    density = sigmoid(density, options.densitySmoothness)
    density *= options.density

    xxSeed = options.presentSeed
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
    xxSeed = options.secondPlaceSeed
    var firstPlaceChance = options.secondPlaceOffset + options.secondPlaceScale * secondPlace.gap
    if (getXYRandom(inX, inY) > firstPlaceChance) {
      pickedBiome = secondPlace.biome
    }

    var biome = biomes[pickedBiome]
    var biomeObjects = biome.objects
    var numObjects = biomeObjects.length

    // jackpot chance
    var specialObjectIndex = -1
    var maxValue = -Number.MAX_VALUE

    var roughness = options.jackpotRoughness
    var scale = options.jackpotOffset + numObjects * options.jackpotScale

    for (var i = 0;i < numObjects;i++) {
      xxSeed = options.jackpotSeedOffset + i * options.jackpotSeedScale
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
    xxSeed = options.objectSeed
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
    if (pickedBiome == secondPlace.biome) {
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
        xxSeed == options.barrierSeed
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
    } else if (false /* wide */) {
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
  base['Fractal'] = new L.GridLayer.FractalLayer({
    minZoom: 2,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
  })

  base['Density'] = new L.GridLayer.FractalLayer({
    minZoom: 2,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
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

  var numSpecialBiomes = 3
  var regularBiomesLimit = specialBiomeMap.length - numSpecialBiomes

  var greenColor = hsvToRgb(89/360, 0.49, 0.67)
  var swampColor = hsvToRgb(253/360, 0.17, 0.65)
  var plainsColor = hsvToRgb(36/360, 0.75, 0.90)
  var badlandsColor = hsvToRgb(40/360, 0.16, 0.36)
  var arcticColor = hsvToRgb(0/360, 0.00, 1.0)
  var desertColor = hsvToRgb(37/360, 0.65, 0.62)
  var jungleColor = hsvToRgb(90/360, 0.87, 0.48)

  var biomeGenerationOptions = {
    computeMapBiomeIndex: competeMapBiomeIndex,
    biomeOffset: 0.83332,
    biomeScale: 0.08333,
    biomeFractalRoughness: 0.55,
    biomeSeedOffset: 723,
    biomeSeedScale: 263,
    biomeMap: jungleBiomeMap,
    numSpecialBiomes: 0,
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
      className: 'crisp',
      biomeColors: [
        greenColor,
        swampColor,
        plainsColor,
        badlandsColor,
        arcticColor,
        desertColor,
        jungleColor,
      ],
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
  }, biomeGenerationOptions)

  L.GridLayer.SpriteLayer = L.GridLayer.extend({
    options: {
      offset: 0,
      supersample: 1,
      fadeTallobjects: false,
      attribution: '<a href="https://onetech.info" title="Object generated by OneTech">OneTech</a> ' +
    '<a href="https://github.com/Kazetsukai/onetech" title="OneTech"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>',
    },
    loadImages(placements, done) {
      var layer = this
      placements.forEach(function(placement) {
        if (!objectImages[placement.id]) {
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
          if (!objectImages[placement.id].complete) {
            setTimeout(checkLoaded,100)
            return false
          }
        }
        //console.log('images done')
        setTimeout(done,0)
        return true
      }
      return checkLoaded()
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
          var color = hsvToRgb(placement.id * 3769 % 359 / 360, 1, 1)
          ctx.fillStyle = 'rgb(' + color[0] + ',' + color[1] + ',' + color[2] + ')'

          var iw = objectBounds[placement.id][2]/CELL_D - ox
          var ih = objectBounds[placement.id][1]/CELL_D + oy
          ctx.globalAlpha = 0.5
          ctx.fillRect(placement.x + ox, placement.y + oy, iw, ih)
          //ctx.fillRect(placement.x*cellSize, placement.y*cellSize, cellSize, cellSize)
        }
      })
      ctx.restore()

      if (done) done(null, tile)
    },
  })

  L.GridLayer.ObjectLayerPixel = L.GridLayer.extend({
    options: Object.assign({
      pane: 'overlayPane',
      className: 'crisp',
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
          var color = hsvToRgb(v * 3769 % 359 / 360, 1, 1)
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

  var objectOverlayPixel = new L.GridLayer.ObjectLayerPixel({
    minZoom: 24,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
    opacity: 0.5,
  })
  overlays['Object Pixel'] = objectOverlayPixel

  L.GridLayer.ObjectLayerSprite = L.GridLayer.SpriteLayer.extend({
    options: Object.assign({
      pane: 'overlayPane',
    }, objectGenerationOptions),
    createTile: function (coords, done) {
      var layer = this
      var options = layer.options
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
      var cellSize = Math.pow(2, coords.z - 24)
      var cellRight = tileSize.x/cellSize + paddingX
      var cellBottom = tileSize.y/cellSize + paddingDown
      //console.log(cellSize, cellRight, cellBottom)
      var minSize = 1.5 * Math.pow(2, 31 - coords.z)
      var kp = tile._keyplace = []
      for (var y = -paddingUp;y < cellBottom;y++) {
        for (var x = -paddingX;x < cellRight;x++) {
          var wx = startX + x
          var wy = startY - y
          var v = getMapObjectRaw(wx, wy, options)
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
      layer.loadImages(tile._keyplace, function() {
        layer.drawTile(tile, coords, done)
      })
      return tile
    },
  })

  var objectOverlaySprite = new L.GridLayer.ObjectLayerSprite({
    //opacity: 0.2,
    //offset: 0.2,
    minZoom: 24,
    maxZoom: 31,
    //minNativeZoom: 24,
    //maxNativeZoom: 24,
    attribution: attribution,
  })
  overlays['Object Sprite'] = objectOverlaySprite

  var updateArcs = function(arcData) {
    arcs = arcData.map(function(arc, i) {
      var biomeLayer = createArcBiomeLayer(arc.start * 1000, arc.seed)
      return {
        msStart: arc.start * 1000,
        msEnd: arc.end * 1000,
        seed: arc.seed,
        name: 'Arc '+(i+1),
        layer: L.layerGroup([biomeLayer]),
        biomeLayer: biomeLayer,
        keyPlacementLayer: null,
        maplogLayer: null,
      }
    })
    if (objectSize.length > 0) {
      addArcPlacements()
    }
    /*
    arcs.forEach(function(arc, i) {
      console.log(arc.name)
      console.log('Fr:', new Date(arc.msStart).toString())
      console.log('To:', new Date(arc.msEnd).toString())
    })
    */
  }

  var addArcPlacements = function() {
    arcs.forEach(function(arc) {
      var keyPlacementLayer = createArcKeyPlacementLayer(arc.msEnd/1000)
      keyPlacementLayer.name = "key placement"
      arc.keyPlacementLayer = keyPlacementLayer
      var maplogLayer = createArcMaplogLayer(arc.msStart, arc.msEnd/1000)
      maplogLayer.name = "maplog"
      arc.maplogLayer = maplogLayer
      L.Util.setOptions(keyPlacementLayer, {alternateAnim: maplogLayer})
      L.Util.setOptions(maplogLayer, {alternateStatic: keyPlacementLayer})
      if (dataAnimated) {
        arc.layer.addLayer(maplogLayer)
      } else {
        arc.layer.addLayer(keyPlacementLayer)
      }
    })
  }

  var createArcBiomeLayer = function(msStart, seed) {
    if (msStart > msStartOfSpecialAge) {
      return new L.GridLayer.BiomeLayer({
        computeMapBiomeIndex: topographicMapBiomeIndex,
        biomeTotalWeight: specialBiomeTotalWeight,
        biomeCumuWeights: specialBiomeCumuWeights,
        biomeSeedOffset: seed,
        biomeMap: specialBiomeMap,
        numSpecialBiomes: 3,
        minZoom: 2,
        maxZoom: 31,
        //minNativeZoom: 24,
        maxNativeZoom: 24,
        attribution: attribution,
      })
    } else if (msStart > msStartOfTopographicAge) {
      return new L.GridLayer.BiomeLayer({
        computeMapBiomeIndex: topographicMapBiomeIndex,
        biomeTotalWeight: topographicBiomeTotalWeight,
        biomeCumuWeights: topographicBiomeCumuWeights,
        biomeSeedOffset: seed,
        biomeMap: topographicBiomeMap,
        minZoom: 2,
        maxZoom: 31,
        //minNativeZoom: 24,
        maxNativeZoom: 24,
        attribution: attribution,
      })
    } else {
      return new L.GridLayer.BiomeLayer({
        biomeSeedOffset: seed,
        biomeMap: jungleBiomeMap,
        minZoom: 2,
        maxZoom: 31,
        //minNativeZoom: 24,
        maxNativeZoom: 24,
        attribution: attribution,
      })
    }
  }

  base['Desert Age'] = new L.GridLayer.BiomeLayer({
    biomeMap: desertBiomeMap,
    minZoom: 2,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
  })

  base['Arctic Age'] = new L.GridLayer.BiomeLayer({
    biomeMap: arcticBiomeMap,
    minZoom: 2,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
  })

  var badlandsAge = new L.GridLayer.BiomeLayer({
    biomeMap: badlandsBiomeMap,
    minZoom: 2,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
  })

  var greenColor3 = [50, 200, 50]
  var swampColor3 = [150, 70, 150]
  var plainsColor3 = [250, 250, 30]
  var badlandsColor3 = [150, 150, 150]

  var server3Biome = new L.GridLayer.BiomeLayer({
    biomeMap: badlandsBiomeMap,
    minZoom: 2,
    maxZoom: 31,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
    biomeColors: [
      greenColor3,
      swampColor3,
      plainsColor3,
      badlandsColor3,
    ]
  })

  var server3Map = L.imageOverlay('overlays/server3.png',
    [[-1170.5, -695.5], [-401.5, 1252.5 - 695]], {
      pane: 'tilePane',
      attribution: '<a href="https://onehouronelife.com/forums/viewtopic.php?id=236">rosden</a>',
    })

  var server3 = new L.layerGroup([server3Biome, server3Map])

  base['Badlands Age'] = new L.layerGroup([badlandsAge])

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
    expire: function() {
      while (this._list.length > 100) {
        var record = this._list.shift()
        delete this._index[record.url]
      }
    },
    loadTile: function(coords, data) {
      var dataZoom = this.dataZoom(coords)
      var cellSize = Math.pow(2, coords.z - dataZoom)
      var datacoords = {
        x: Math.floor(coords.x/cellSize),
        y: Math.floor(coords.y/cellSize),
        z: dataZoom,
      }
      var layer = this
      //console.log(datacoords)
      var url = this.getDataTileUrl(datacoords, data)
      if (this._index[url]) {
        var record = this._index[url]
        this._list.splice(this._list.indexOf(record),1)
        this._list.push(record)
        return record.promise
      } else {
        var record = {
          url: url,
          promise: fetch(url).then(function(response) {
            if (response.status % 100 == 4) {
              return Promise.resolve('')
            }
            return response.text()
          }),
        }
        this._list.push(record)
        this._index[url] = record
        this.expire()
        //console.log(this._list.length)
        return record.promise
      }
    }
  })

  var objectImages = []

  L.GridLayer.KeyPlacementSprite = L.GridLayer.SpriteLayer.extend({
    initialize: function(cache, options) {
      this._cache = cache;
      options = L.Util.setOptions(this, options);
    },
    createTile: function (coords, done) {
      var tile = document.createElement('canvas');
      var tileSize = this.getTileSize();
      var superscale = Math.pow(2, this.options.supersample)
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
      var dataZoom = this._cache.dataZoom(coords)
      var cellSize = Math.pow(2, coords.z - dataZoom)
      var cellWidth = tileSize.x/cellSize + paddingX
      var cellHeight = tileSize.y/cellSize + paddingDown
      var minSize = 1.5 * Math.pow(2, 31 - coords.z)

      //console.log('cellsize', cellSize, 'cellWidth', cellWidth)
      var layer = this
      //console.log(datacoords)
      //console.log('data tile ' + JSON.stringify(coords))
      //console.time('data tile ' + JSON.stringify(coords))
      layer._cache.loadTile(coords, {time: layer.options.dataTime}).then(function(text) {
        //console.timeEnd('data tile ' + JSON.stringify(coords))
        //console.time('data processing ' + JSON.stringify(coords))
        tile._keyplace = text.split("\n").filter(function(line) {
          return line != "";
        }).map(function(line) {
          var parts = line.split(" ")
          try {
          var out = {
            x: parseInt(parts[0],10) - startX,
            y: -(parseInt(parts[1],10) - startY),
            id: parseInt(parts[2].replace('f', ''),10),
            floor: parts[2][0] == 'f',
          }
          } catch (e) {
            console.log(e, parts, line)
          }
          return out
        }).filter(function(placement) {

          var isValid = !isNaN(placement.id) && placement.id < 5000
          if (!isValid) return false
          var inFrame =
            (-paddingX <= placement.x && placement.x < cellWidth) &&
            (-paddingUp <= placement.y && placement.y < cellHeight)
          if (!inFrame) return false
          var size = objectSize[placement.id]
          var tooSmall = !size || size <= minSize
          if (tooSmall) return false
          return true
        }).sort(sortTypeAndDrawOrder)

        var special = specialMapPlacements.filter(function(place) {
          return (place.msStart/1000 < layer.options.dataTime
                  && startX <= place.x && place.x <= endX
                  && endY <= place.y && place.y <= startY)
        }).map(function(place) {
          return {
            x: place.x - startX,
            y: -(place.y - startY),
            id: place.id
          }
        })
        if (special.length > 0) {
          tile._keyplace = special.concat(tile._keyplace)
        }

        //console.timeEnd('data processing ' + JSON.stringify(coords))
        //console.time('load images' + JSON.stringify(coords))
        //console.time('load images call' + JSON.stringify(coords))
        if (layer.loadImages(tile._keyplace, function() {
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

  L.GridLayer.MaplogSprite = L.GridLayer.SpriteLayer.extend({
    options: {
      time: 0,
    },
    initialize: function(cache, options) {
      this._cache = cache;
      options = L.Util.setOptions(this, options);
    },
    createTile: function (coords, done) {
      var tile = document.createElement('canvas');
      var tileSize = this.getTileSize();
      var superscale = Math.pow(2, this.options.supersample)
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
      var dataZoom = this._cache.dataZoom(coords)
      var cellSize = Math.pow(2, coords.z - dataZoom)
      var cellWidth = tileSize.x/cellSize + paddingX
      var cellHeight = tileSize.y/cellSize + paddingDown
      //console.log('cellsize', cellSize, 'cellWidth', cellWidth)
      var layer = this
      //console.log(datacoords)
      layer._cache.loadTile(coords, {time: layer.options.dataTime}).then(function(text) {
        var t = 0
        var x = 0
        var y = 0
        tile._maplog = text.split("\n").filter(function(line) {
          return line != "";
        }).map(function(line) {
          var parts = line.split(" ")
          try {
          t = t + (parseInt(parts[0],10)*10)
          x = x + parseInt(parts[1],10)
          y = y + parseInt(parts[2],10)
          var out = {
            t: t,
            x: x - startX,
            y: -(y - startY),
            id: parseInt(parts[3].replace('f', ''),10),
            floor: parts[3][0] == 'f',
          }
          out.key = [out.x, out.y].join(' ')
          } catch (e) {
            console.log(e, parts, line)
          }
          return out
        }).filter(function(placement) {

          var isValid = !isNaN(placement.id) && placement.id < 5000
          if (!isValid) return false
          var inFrame =
            (-paddingX <= placement.x && placement.x < cellWidth) &&
            (-paddingUp <= placement.y && placement.y < cellHeight)
          if (!inFrame) return false
          return true
        }).sort(function(a, b) {
          return a.t - b.t
        })

        var special = specialMapPlacements.filter(function(place) {
          return (place.msStart/1000 < layer.options.dataTime
                  && startX <= place.x && place.x <= endX
                  && endY <= place.y && place.y <= startY)
        }).map(function(place) {
          return {
            t: 1,
            x: place.x - startX,
            y: -(place.y - startY),
            id: place.id
          }
        })
        if (special.length > 0) {
          tile._maplog = special.concat(tile._maplog)
        }

        var time = layer.options.time
        tile.coords = coords
        layer.tileAt(tile, time)
        done()
      })

      return tile
    },
    tileAt: function(tile, time) {
      var maplog = tile._maplog
      if (!maplog) return;
      var lastTime = tile._time || 0
      tile._time = time
      var objects = tile._objects = tile._objects || {}
      var floors = tile._floors = tile._floors || {}
      var minSize = 1.5 * Math.pow(2, 31 - tile.coords.z)
      if (time < lastTime) {
        lastTime = 0
        objects = tile._objects = {}
        floors = tile._floors = {}
      }
      maplog.forEach(function(placement) {
        if (lastTime < placement.t && placement.t < time) {
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
        .filter(placementsLargerThan(minSize))
        .sort(sortDrawOrder)
      tile._keyplace = floorPlacements.concat(objectPlacements)
      var layer = this
      if (layer.loadImages(tile._keyplace, function() {
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
          this.drawTile(tile.el, tile.coords)
        }
      }
    },
  })

  var arcUpdateTiles = function(ms) {
    base['Arc Age'].eachLayer(function(group) {
      group.eachLayer(function(sub) {
        if (sub.eachLayer) {
          sub.eachLayer(function(layer) {
            if (layer.updateTiles) {
              layer.updateTiles(ms)
            }
          })
        }
      })
    })
  }

  var keyPlacementCache = new TileDataCache(oholMapConfig.keyPlacements, {
    dataminzoom: 24,
    datamaxzoom: 24,
  })

  var createArcKeyPlacementLayer = function(end) {
    if (end*1000 > msStartOfRandomAge) {
      return new L.layerGroup([
        baseAttributionLayer,
        new L.GridLayer.KeyPlacementSprite(keyPlacementCache, {
          dataTime: end.toString(),
          //dataTime: '1564439085',
          //dataTime: '1564457929',
          //dataTime: '1564571257',
          //dataTime: '1564625380',
          //dataTime: '1564632744',
          minZoom: 24,
          maxZoom: 31,
          //minNativeZoom: 24,
          maxNativeZoom: 31,
        })
      ])
    } else {
      return L.layerGroup([])
    }
  }

  var maplogCache = new TileDataCache(oholMapConfig.maplog, {
    dataminzoom: 24,
    datamaxzoom: 27,
  })

  var createArcMaplogLayer = function(msStart, sEnd) {
    if (sEnd*1000 > msStartOfRandomAge) {
      var ms = sEnd*1000
      if (msStart < mapTime && mapTime < sEnd*1000) {
        ms = mapTime
      }
      return new L.layerGroup([
        baseAttributionLayer,
        new L.GridLayer.MaplogSprite(maplogCache, {
          dataTime: sEnd.toString(),
          time: ms,
          minZoom: 24,
          maxZoom: 31,
          //minNativeZoom: 24,
          maxNativeZoom: 31,
        })
      ])
    } else {
      return L.layerGroup([])
    }
  }

  var setFadeTallObjects = function(status) {
    arcs.forEach(function(arc) {
      arc.keyPlacementLayer.eachLayer(function(layer) {
        L.Util.setOptions(layer, {fadeTallObjects: status})
        layer.redraw()
      })
      arc.maplogLayer.eachLayer(function(layer) {
        L.Util.setOptions(layer, {fadeTallObjects: status})
        layer.redraw()
      })
    })
  }

  var colormap = function(id) {
    return '#' + (((id * 49157) % 12582917).toString(16))
  }

  var colorlineage = function(id) {
    var hue = (id * 49157) % 359
    var sat = (id * 24593) % 67 + 33
    var light = (id * 12289) % 53 + 20
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

  var colorcause = function(cause) {
    if (cause && cause.match('killer')) {
      return "#ff0000";
    }
    switch(cause) {
      case 'hunger': return "hsl(60, 50%, 40%)"
      case 'oldAge': return "hsl(0, 0%, 20%)"
      case 'disconnect': return "hsla(180, 100%, 40%)"
      case 'unknown': return "hsl(290, 100%, 50%)"
      default: return "hsla(90, 100%, 50%, 0)"
    }
  }

  L.GridLayer.PointOverlay = L.GridLayer.extend({
    options: {
      pane: 'overlayPane',
      alternateAnim: null,
      alternateStatic: null,
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

      var color = options.color || 'lineageColor'
      var location = options.location || 'birth'
      var fadeTime = 60*60
      options.data.forEach(function(point) {
        //console.log(point)

        var t = 1
        var r = 1;
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
            r = Math.pow(10 * t, -6 * t)
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
          } else {
            ctx.fillStyle = ctx.strokeStyle = point[color]
          }

          if (point.gender == 'M') {
            ctx.beginPath();
            ctx.fillRect(p.x - 3, p.y - 3, 6, 6)
            ctx.fill();
          } else if (14 <= age && age <= 40) {
            ctx.beginPath();
            ctx.arc(p.x, p.y, 3, 0, 2*Math.PI, false);
            ctx.lineWidth = 3;
            ctx.stroke();
          } else {
            ctx.beginPath();
            ctx.arc(p.x, p.y, 3, 0, 2*Math.PI, false);
            ctx.fill();
          }

          if (point.chain == 1) {
            ctx.beginPath();
            ctx.arc(p.x, p.y, 3 + 7 * r, 0, 2*Math.PI, false);
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
      var map = layer._map
      var zoom = map.getZoom()
      var llnw = map.layerPointToLatLng(pnw, zoom)
      var llse = map.layerPointToLatLng(pse, zoom)
      //console.log(center, llnw, llse)
      var location = this.options.location || 'birth'

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

  var pointOverlay = new L.GridLayer.PointOverlay({
    className: 'interactive',
  })
  pointOverlay.name = 'point overlay'

  var animOverlay = new L.GridLayer.PointOverlay({
    className: 'interactive',
    time: 0,
    alternateStatic: pointOverlay,
  })
  animOverlay.name = 'anim overlay'
  L.Util.setOptions(pointOverlay, {alternateAnim: animOverlay})

  if (dataAnimated) {
    animOverlay.addTo(dataOverlay)
  } else {
    pointOverlay.addTo(dataOverlay)
  }

  var resultPoints = new L.GridLayer.PointOverlay().addTo(searchOverlay)

  var setDataLayers = function(data) {
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
      point.lineageColor = colorlineage(point.lineage)
      if (point.hash) {
        point.hashColor = colorhash(point.hash)
      }
      point.birthTimeColor = colorlinear(point.birth_time, min, max)
      point.chainColor = colorlinear(point.chain, minChain, maxChain)
      point.causeOfDeathColor = colorcause(point.cause)
      point.ageColor = colorage(point.age)
    })
    L.Util.setOptions(animOverlay, {
      data: data,
      min: min,
      max: max,
    })
    animOverlay.redraw()
    L.Util.setOptions(pointOverlay, {
      data: data,
      min: min,
      max: max,
    })
    pointOverlay.redraw()
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
    app.ports.leafletEvent.send({
      kind: 'dataRange',
      min: min,
      max: max,
    })
  }

  var moveIfOutOfView = function(data, map) {
    var bounds = map.getBounds()
    for (var i in data) {
      var point = data[i]
      if (bounds.contains([point.birth_y, point.birth_x])) {
        return
      }
    }
    var bounds = L.latLngBounds(data.filter(function(point) {
      return (-40000 < point.birth_x && point.birth_x < 40000)
    }).map(function(point) {
      return [point.birth_y, point.birth_x]
    }))
  }

  var baseLayerByTime = function(map, ms, reason) {
    //console.log(ms, reason)
    var targetLayer
    arcs.forEach(function(arc) {
      if (ms > arc.msStart && ms <= arc.msEnd) {
        targetLayer = 'Arc Age'
        //console.log(arc.msStart, ms, arc.msEnd)
        base['Arc Age'].addLayer(arc.layer)
      } else {
        base['Arc Age'].removeLayer(arc.layer)
      }
    })
    if (targetLayer) {
      //skip
    } else if (ms > msStartOfRandomAge) {
      targetLayer = 'Uncertainty'
    } else if (ms > msStartOfJungleAge) {
      targetLayer = 'Jungle Age'
    } else if (ms > msStartOfDesertAge) {
      targetLayer = 'Desert Age'
    } else if (ms > msStartOfArcticAge) {
      targetLayer = 'Arctic Age'
    } else {
      targetLayer = 'Badlands Age'
    }
    var changes = 0
    Object.keys(base).forEach(function(key) {
      if (map.hasLayer(base[key])) {
        if (key != targetLayer) {
          map.removeLayer(base[key])
          changes++
        }
      } else {
        if (key == targetLayer) {
          map.addLayer(base[key])
          changes++
        }
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
        if (!zoom || zoom < 25) {
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

  var setMapTime = function(map, ms, reason) {
    mapTime = ms
    if (map) baseLayerByTime(map, ms, reason)
    riftLayerByTime(ms, map && map.getZoom())
    animOverlay.updateTiles(ms)
    arcUpdateTiles(ms)
    L.Util.setOptions(legendControl, {time: ms})
    legendControl.redraw()
  }

  var setPointColor = function(color) {
    L.Util.setOptions(animOverlay, {
      color: color,
    })
    animOverlay.redraw()
    L.Util.setOptions(pointOverlay, {
      color: color,
    })
    pointOverlay.redraw()
    L.Util.setOptions(legendControl, {
      color: color,
    })
    legendControl.redraw()
  }

  var setPointLocation = function(location) {
    L.Util.setOptions(animOverlay, {
      location: location,
    })
    animOverlay.redraw()
    L.Util.setOptions(pointOverlay, {
      location: location,
    })
    pointOverlay.redraw()
  }

  animOverlay.on('add', function(ev) {
    ev.target.addInteractiveTarget(ev.target._container)
  })
  animOverlay.on('click', animOverlay.selectPoints)

  pointOverlay.on('add', function(ev) {
    ev.target.addInteractiveTarget(ev.target._container)
  })
  pointOverlay.on('click', pointOverlay.selectPoints)

  resultPoints.on('add', function(ev) {
    ev.target.setZIndex(450)
  })

  var options = {
    showOriginLabel: false,
    redraw: 'move',
    attribution: '<a href="https://github.com/ablakey/Leaflet.SimpleGraticule">ablakey/SimpleGraticle</a>',
    zoomIntervals: [
      {start: 0,  end: 3,  interval: 1000000000},
      {start: 4,  end: 6,  interval: 100000000},
      {start: 7,  end: 9,  interval: 10000000},
      {start: 10, end: 13, interval: 1000000},
      {start: 14, end: 16, interval: 100000},
      {start: 17, end: 19, interval: 10000},
      {start: 20, end: 23, interval: 1000},
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

        var link = L.DomUtil.create('a', className + '-toggle', container);
        link.href = '#';
        link.title = this.options.title
        link.innerHTML = '<svg class="icon icon-' + this.options.icon + '"><use xlink:href="symbol-defs.svg#icon-' + this.options.icon + '"></use></svg>'

        L.DomEvent.on(link, 'click', this.toggle, this);

        return container;
      },
      toggle: function(e) {
        app.ports.leafletEvent.send({
          kind: this.options.message,
        })
        L.DomEvent.preventDefault(e)
      },
  });

  L.control.mapButton = function(opts) {
    return new L.Control.MapButton(opts);
  }

  var sidebarToggle = L.control.mapButton({
    title: 'Data',
    icon: 'filter',
    message: 'sidebarToggle',
    position: 'bottomright'
  })
  var animToggleAnimated = L.control.mapButton({
    title: 'Time',
    icon: 'cancel-circle',
    message: 'animToggle',
    position: 'bottomleft'
  })
  var animToggleStatic = L.control.mapButton({
    title: 'Time',
    icon: 'time',
    message: 'animToggle',
    position: 'bottomleft'
  })

  L.Control.Legend = L.Control.extend({
    options: {
      color: 'lineageColor',
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
      if (this.options.color != 'lineageColor' && this._color == this.options.color) return
      this._color = this.options.color
      while (container.firstChild) {
        container.removeChild(container.firstChild);
      }
      switch (this.options.color) {
        case 'lineageColor':
          this.updateLineages()
          var maxHeight = this._map.getSize().y - 256
          var height = 0
          var lineages = Object.values(this.options.lineages || {})
            .sort(function(a, b) {
            return b.chain - a.chain
            })
          for (var i in lineages) {
            var life = lineages[i]
            var swatch = L.DomUtil.create('div', 'swatch', container)
            if (height > maxHeight) {
              swatch.style = 'color: black'
              swatch.innerHTML = "...."
              break
            }

            swatch.style = 'background-color: ' + colorlineage(life.lineage)
            if (life.name) {
              var words = life.name.split(' ')
              swatch.innerHTML = (words[1] || words[0])
            }
            height += 22
          }
          break;
        case 'birthTimeColor':
          var swatch = L.DomUtil.create('div', 'swatch', container)
          swatch.style = 'background-color: ' + colorlinear(this.options.min, this.options.min, this.options.max)
          swatch.innerHTML = 'older';

          swatch = L.DomUtil.create('div', 'swatch', container)
          swatch.style = 'background-color: ' + colorlinear(this.options.max, this.options.min, this.options.max) + '; color: black;'
          swatch.innerHTML = 'newer';
          break;
        case 'chainColor':
          var swatch = L.DomUtil.create('div', 'swatch', container)
          swatch.style = 'background-color: ' + colorlinear(this.options.minChain, this.options.minChain, this.options.maxChain)
          swatch.innerHTML = this.options.minChain;

          swatch = L.DomUtil.create('div', 'swatch', container)
          swatch.style = 'background-color: ' + colorlinear(this.options.maxChain, this.options.minChain, this.options.maxChain) + '; color: black;'
          swatch.innerHTML = this.options.maxChain;
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
            swatch.style = 'background-color: ' + colorcause(cause)
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
      if (this._time == this.options.time && this._dataAnimated == this.options.dataAnimated) return
      var time = this.options.time/1000
      var lineages = {}
      var data = this.options.data
      var dataAnimated = this.options.dataAnimated
      for (var i in data) {
        var point = data[i]
        if (dataAnimated) {
          var death_time = (point.death_time || (point.birth_time + 3600))
          if (time < point.birth_time || death_time < time) {
            continue;
          }
        }

        var key = point.lineage.toString()
        var prior = lineages[key]
        if (prior && prior.name && !point.name) break;
        if (!prior || !prior.name || point.chain > prior.chain) {
          lineages[key] = point
        }
      }
      L.Util.setOptions(this, {lineages: lineages})
    },
  });

  L.control.legend = function(opts) {
    return new L.Control.Legend(opts);
  }

  var legendControl = L.control.legend({ position: 'topleft' })

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

  var objectLoad = function(map) {
    var objectMaster = fetch('static/objects.json').then(function(response) {
      return response.json()
    })

    var size = objectMaster.then(function(wrapper) {
      objectBounds = new Array(wrapper.ids.length)
      for (var i = 0;i < wrapper.ids.length;i++) {
        var id = parseInt(wrapper.ids[i])
        var bounds = wrapper.bounds[i]
        objectBounds[id] = bounds
        objectSize[id] = Math.min(
          bounds[2] - bounds[0] - 30,
          bounds[3] - bounds[1] - 30)
      }
      addArcPlacements()
      toggleAnimationControls(map)
    }).catch(function(err) {
      console.log(err)
    })

    //var mapGen = Promise.resolve()
    var mapGen = objectMaster.then(function(wrapper) {
      objects = new Array(wrapper.ids.length)
      for (var i = 0;i < wrapper.ids.length;i++) {
        if (wrapper.names[i].match('gridPlacement')) {
          gridPlacements.push({
            name: wrapper.names[i],
            id: parseInt(wrapper.ids[i]),
            spacing: parseInt(wrapper.names[i].match(/gridPlacement(\d+)/)[1], 10),
            permittedBiomes: [],
          })
        }
      }
      wrapper.biomes.forEach(function(biome) {
        biome.id = parseInt(biome.id, 10)
        var bi = jungleBiomeMap.indexOf(biome.id)
        biomes[bi] = biome
        biome.objects.forEach(function(object) {
          object.id = parseInt(object.id, 10)
        })
        biome.totalChanceWeight = 0
        biome.objects = biome.objects.filter(function(spawnable) {
          for (var i = 0;i < gridPlacements.length;i++) {
            if (gridPlacements[i].id == spawnable.id) {
              gridPlacements[i].permittedBiomes.push(jungleBiomeMap.indexOf(biome.id))
              return false
            }
          }
          biome.totalChanceWeight += spawnable.mapChance
          //console.log('biome', biome.id, spawnable.id, spawnable.mapChance)
          return true
        })
      })
      // object blocking
      //console.log('-2,1', getMapObjectRaw(-2, 1, objectGenerationOptions))
      // off biome moving object
      //console.log('19,15', getBaseMap(19, 15, objectGenerationOptions))
      // object bounds height transparent padding
      //console.log('80,8', getMapObjectRaw(80, 8, objectGenerationOptions))
    }).catch(function(err) {
      console.log(err)
    })
    return Promise.all([size, mapGen])
  }

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
      addControl(map, animToggleAnimated)
      removeControl(map, animToggleStatic)
    } else if (stat > 0) {
      addControl(map, animToggleStatic)
      removeControl(map, animToggleAnimated)
    } else {
      removeControl(map, animToggleStatic)
      removeControl(map, animToggleAnimated)
    }
  }

  var inhabit = function inhabit(id) {
    var map = L.map(id, {
      crs: crs,
      maxBounds: [[-2147483648, -2147483648], [2147483647, 2147483647]],
      minZoom: 2,
      maxZoom: 31,
    })

    map.on('zoomend', function(ev) {
      // moving the map at 28+ (varies with blur radius?) causes the current tile to blank out with blur
      if (map.getZoom() > 27) {
        L.DomUtil.removeClass(map.getPane('tilePane'), 'blur')
      } else if (map.hasLayer(baseFade)) {
        L.DomUtil.addClass(map.getPane('tilePane'), 'blur')
      }

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
      idleTimer = setTimeout(setIdle, 1*60*1000)
    }

    var idleTimer = setTimeout(setIdle, 1*60*1000)
    L.DomEvent.on(map, 'mousemove', setActive, map);

    var t = Date.now()
    setMapTime(map, t, 'inhabit')
    //base['Topographic Test'].addTo(map)
    overlays['Rift'].addTo(map)
    //overlays['Checker'].addTo(map)
    //base['Fractal'].addTo(map)
    //base['Biome'].addTo(map)
    //map.addControl(animToggle)

    layersControl.addTo(map)
    L.control.scale({imperial: false}).addTo(map)
    sidebarToggle.addTo(map)
    //map.setView([0,0], 24)

    objectLoad(map).then(function() {
      objectOverlayPixel.addTo(map)
      objectOverlaySprite.addTo(map)
    })

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
          var targetLayer
          if (message.server.id == 3) {
            if (base['Badlands Age'].hasLayer(badlandsAge)) {
              base['Badlands Age'].addLayer(server3)
              base['Badlands Age'].removeLayer(badlandsAge)
            }
          } else {
            if (base['Badlands Age'].hasLayer(server3)) {
              base['Badlands Age'].addLayer(badlandsAge)
              base['Badlands Age'].removeLayer(server3)
            }
          }
          break;
        case 'arcList':
          updateArcs(message.arcs.data)
          var ms = message.time * 1000
          setMapTime(map, ms, 'arcList')
          break;
        case 'monumentList':
          updateMonumentLayer(monumentOverlay, message.monuments.data)
          //monumentOverlay.addTo(map)
          break;
        case 'dataLayer':
          setDataLayers(message.lives.data)
          moveIfOutOfView(message.lives.data, map)
          if(!map.hasLayer(dataOverlay)) {
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
        case 'focus':
          var life = message.life;
          if (focusMarker) {
            focusMarker.remove();
          }
          focusMarker = L.marker([life.birth_y, life.birth_x])
            .bindPopup(life.name)
            .addTo(searchOverlay)
            .openPopup()
          map.setView([life.birth_y, life.birth_x])
          var ms = life.birth_time*1000
          setMapTime(map, ms, 'focus')
          break
        case 'searchOverlay':
          if (message.status) {
            searchOverlay.addTo(map)
          } else {
            searchOverlay.remove()
          }
          break
        case 'animOverlay':
          dataAnimated = message.status
          toggleAnimated(dataOverlay, message.status)
          arcs.forEach(function(arc) {
            toggleAnimated(arc.layer, message.status)
          })
          L.Util.setOptions(legendControl, {dataAnimated: message.status})
          legendControl.redraw()
          toggleAnimationControls(map)
          break
        case 'baseLayer':
          for (var name in base) {
            if (name == message.layer) {
              map.addLayer(base[name])
            } else {
              map.removeLayer(base[name])
            }
          }
          break;
        case 'pointColor':
          setPointColor(message.color)
          break;
        case 'pointLocation':
          setPointLocation(message.location)
          break;
        case 'fadeTallObjects':
          setFadeTallObjects(message.status)
          break
        default:
          console.log('unknown message', message)
          break;
      }
    }

    if (app.ports.leafletCommand) {
      app.ports.leafletCommand.subscribe(command)
    }
  }

  inhabit('map')
})()
