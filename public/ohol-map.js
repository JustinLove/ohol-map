;(function() {
  var app = Elm.MapUI.init({flags: oholMapConfig})

  var cachedApiUrl = oholMapConfig.cachedApiUrl
  var apiUrl = oholMapConfig.apiUrl

  var scale = Math.pow(2, 24)
  var crs = L.extend({}, L.CRS.Simple, {
    transformation: new L.transformation(1/scale, 0.5/scale, -1/scale, -0.5/scale)
  })

  var base = {};

  var attribution = '<a href="https://onehouronelife.com">Jason Rohrer</a> wondible' +
    '<a href="https://twitter.com/wondible" title="@wondible"><svg class="icon icon-twitter"><use xlink:href="symbol-defs.svg#icon-twitter"></use></svg></a>' +
    '<a href="https://github.com/JustinLove/ohol-map" title="Frontend: ohol-map"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>' +
    '<a href="https://github.com/JustinLove/ohol-data-server" title="Backend: ohol-data-server"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>' +
    '<a href="https://github.com/JustinLove/OneLife/tree/mapping" title="Tile generation: OneLife/mapping"><svg class="icon icon-github"><use xlink:href="symbol-defs.svg#icon-github"></use></svg></a>'

  base['Default'] = L.tileLayer(oholMapConfig.mainTiles, {
    errorTileUrl: 'ground_U.png',
    minZoom: 2,
    maxZoom: 27,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
  })

  base['Faded'] = L.tileLayer(oholMapConfig.mainTiles, {
    errorTileUrl: 'ground_U.png',
    minZoom: 2,
    maxZoom: 27,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    opacity: 0.2,
    attribution: attribution,
  })

  base['Crucible'] = L.tileLayer(oholMapConfig.crucibleTiles, {
    errorTileUrl: 'ground_U.png',
    minZoom: 2,
    maxZoom: 27,
    //minNativeZoom: 24,
    maxNativeZoom: 25,
    attribution: attribution,
  });

  var dataOverlay = L.layerGroup([])
  dataOverlay.on('add', function(ev) {
    var map = ev.target._map
    map.addControl(colorScaleControl)
    setTimeout(function() {
      map.removeLayer(base['Default'])
      map.addLayer(base['Faded'])
    },0)
  })
  dataOverlay.on('remove', function(ev) {
    var map = ev.target._map
    map.removeControl(colorScaleControl)
    setTimeout(function() {
      map.removeLayer(base['Faded'])
      map.addLayer(base['Default'])
    },0)
  })

  var monumentOverlay = L.layerGroup([])

  var overlays = {
    graticule: null,
    "Life Data": dataOverlay,
    "Monuments": monumentOverlay,
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
    h32 = xxSeed + inX + XX_PRIME32_5
    h32 += Math.imul(inY, XX_PRIME32_3)
    h32 = Math.imul(h32, XX_PRIME32_2)
    h32 ^= h32 >>> 13
    h32 = Math.imul(h32, XX_PRIME32_3)
    h32 ^= h32 >>> 16
    h32 >>>= 0
    return h32
  }

  var getXYRandomBN = function(inX, inY) {
    var floorX = Math.floor(inX)
    var ceilX = floorX + 1
    var floorY = Math.floor(inY)
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

  var biomeMap = [
    0,
    3,
    4,
    5,
    2,
    1,
    6,
  ]

  var computeMapBiomeIndex = function(inX, inY, options) {
    var maxValue = -Number.MAX_VALUE
    var pickedBiome = -1
    var scale = options.biomeOffset + options.biomeScale * options.numBiomes
    var roughness = options.biomeFractalRoughness
    for (var i = 0;i < options.numBiomes;i++) {
      var biome = biomeMap[i]

      xxSeed = biome * options.biomeSeedScale + options.biomeSeedOffset
      randVal = getXYFractal(inX, inY, roughness, scale)

      if (randVal > maxValue) {
        maxValue = randVal
        pickedBiome = i
      }
    }

    return pickedBiome
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
      //console.log(tileSize)
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
      //console.log(coords, pnw)
      llnw = crs.pointToLatLng(pnw, coords.z)
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

  base['Fractal'] = new L.GridLayer.FractalLayer({
    minZoom: 2,
    maxZoom: 27,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
  })

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

  var greenColor = hsvToRgb(89/360, 0.49, 0.67)
  var swampColor = hsvToRgb(253/360, 0.17, 0.65)
  var plainsColor = hsvToRgb(36/360, 0.75, 0.90)
  var badlandsColor = hsvToRgb(40/360, 0.16, 0.36)
  var arcticColor = hsvToRgb(0/360, 0.00, 1.0)
  var desertColor = hsvToRgb(37/360, 0.65, 0.62)
  var jungleColor = hsvToRgb(90/360, 0.87, 0.48)

  L.GridLayer.BiomeLayer = L.GridLayer.extend({
    options: {
      biomeOffset: 0.83332,
      biomeScale: 0.08333,
      biomeFractalRoughness: 0.55,
      numBiomes: 7,
      biomeSeedOffset: 723,
      biomeSeedScale: 263,
      biomeColors: [
        greenColor,
        swampColor,
        plainsColor,
        badlandsColor,
        arcticColor,
        desertColor,
        jungleColor,
      ]
    },
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
      llnw = crs.pointToLatLng(pnw, coords.z)
      //console.log(coords, llnw)

      var stride = Math.pow(2, 24 - coords.z)
      var w = tile.width
      var h = tile.height
      var startX = llnw.lng + 0.5
      var startY = llnw.lat - 0.5

      //console.log(coords, startX, startY)

      var scale = this.options.biomeOffset + this.options.biomeScale * 7
      var roughness = this.options.biomeFractalRoughness

      var colors = this.options.biomeColors
      var imageData = ctx.createImageData(tile.width, tile.height)
      var d = imageData.data

      for (var y = 0;y < h;y++) {
        for (var x = 0;x < w;x++) {
          var i = (y * w + x) * 4
          var wx = startX + x*stride
          var wy = startY - (y*stride)
          var v = biomeMap[computeMapBiomeIndex(wx, wy, this.options)]
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

  base['Biome'] = new L.GridLayer.BiomeLayer({
    minZoom: 2,
    maxZoom: 27,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: attribution,
  })

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
      if (!this.options.data) {
        return
      }
      if (this.options.timeDimension) {
        time = time || this.options.timeDimension.getCurrentTime()/1000
      }
      var tileSize = this.getTileSize();

      var ctx = tile.getContext('2d');
      ctx.clearRect(0, 0, tile.width, tile.height)
      var padding = 10

      var origin = L.point(coords.x * tileSize.x, coords.y * tileSize.y)
      var pnw = L.point(coords.x * tileSize.x - padding, coords.y * tileSize.y - padding)
      var pse = L.point(pnw.x + tileSize.x + padding*2, pnw.y + tileSize.y + padding*2)
      //console.log(coords, pnw, pse)
      llnw = crs.pointToLatLng(pnw, coords.z)
      llse = crs.pointToLatLng(pse, coords.z)
      //console.log(coords, llnw, llse)

      var color = this.options.color || 'lineageColor'
      var location = this.options.location || 'birth'
      var fadeTime = 60*60
      this.options.data.forEach(function(point) {
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
            var a = 1 - t
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
          ctx.fillStyle = ctx.strokeStyle = point[color]
          ctx.beginPath();
          ctx.arc(p.x, p.y, 3, 0, 2*Math.PI, false);
          ctx.fill();

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
    updateTiles: function(ev) {
      var time = ev.time/1000
      for (key in this._tiles) {
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
      llnw = map.layerPointToLatLng(pnw, zoom)
      llse = map.layerPointToLatLng(pse, zoom)
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
      if (this.options.timeDimension) {
        var time = this.options.timeDimension.getCurrentTime()/1000
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


  var timeDimension = new L.TimeDimension({
    //timeInterval: start + "/" + end,
    //period: "PT1M",
  });

  var player = new L.TimeDimension.Player({
    transitionTime: 100, 
    loop: false,
    startOver:true
  }, timeDimension);

  var timeDimensionControlOptions = {
    player: player,
    timeDimension: timeDimension,
    position: 'bottomleft',
    autoPlay: false,
    minSpeed: 1,
    speedStep: 1,
    maxSpeed: 30,
    timeSteps: 60,
    timeSliderDragUpdate: true
  };

  L.Control.ClosableTimeDimension = L.Control.TimeDimension.extend({
    onAdd: function(map) {
      var container = L.Control.TimeDimension.prototype.onAdd(map)
      this._buttonClose = this._createButton('Close', container);
      return container
    },
    _buttonCloseClicked: function() {
      app.ports.leafletEvent.send({
        kind: 'animToggle',
      })
    },
  });

  var timeDimensionControl = new L.Control.ClosableTimeDimension(timeDimensionControlOptions);

  var pointOverlay = new L.GridLayer.PointOverlay({className: 'interactive'}).addTo(dataOverlay)

  var animOverlay = new L.GridLayer.PointOverlay({
    attribution: '<a href="https://github.com/socib/Leaflet.TimeDimension">socib/Leaflet.TimeDimension</a>',
    className: 'interactive',
    timeDimension: timeDimension,
  })
  timeDimension.on("timeload", animOverlay.updateTiles, animOverlay)

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
      if (max == null || point.birth_time > max) {
        max = point.birth_time
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
    })
    var times = []
    for (var t = min;t < max;t += 1) {
      times.push(t * 1000)
    }
    timeDimension.setAvailableTimes(times, 'replace')
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
    L.Util.setOptions(colorScaleControl, {
      min: min,
      max: max,
      minChain: minChain,
      maxChain: maxChain,
    })
    pointOverlay.redraw()
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
    L.Util.setOptions(colorScaleControl, {
      color: color,
    })
    colorScaleControl.redraw()
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
    ev.target._map.addControl(timeDimensionControl)
    ev.target.addInteractiveTarget(ev.target._container)
  })
  animOverlay.on('remove', function(ev) {
    ev.target._map.removeControl(timeDimensionControl)
  })
  animOverlay.on('click', animOverlay.selectPoints)

  pointOverlay.on('add', function(ev) {
    ev.target.addInteractiveTarget(ev.target._container)
    ev.target._map.addControl(animToggle)
  })
  pointOverlay.on('remove', function(ev) {
    ev.target._map.removeControl(animToggle)
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

  layersControl = L.control.layers(base, overlays, {autoZIndex: false})

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
      toggle: function() {
        app.ports.leafletEvent.send({
          kind: this.options.message,
        })
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
  var animToggle = L.control.mapButton({
    title: 'Time',
    icon: 'time',
    message: 'animToggle',
    position: 'bottomleft'
  })

  L.Control.ColorScale = L.Control.extend({
      options: {
        color: 'lineageColor'
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
        while (container.firstChild) {
          container.removeChild(container.firstChild);
        }
        switch (this.options.color) {
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
        }
      }
  });

  L.control.colorScale = function(opts) {
    return new L.Control.ColorScale(opts);
  }

  var colorScaleControl = L.control.colorScale({ position: 'topleft' })

  var inhabit = function inhabit(id) {
    var map = L.map(id, {
      crs: crs,
      maxBounds: [[-2147483648, -2147483648], [2147483647, 2147483647]],
      minZoom: 2,
      maxZoom: 27,
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

    //base['Default'].addTo(map)
    //base['Faded'].addTo(map)
    //base['Fractal'].addTo(map)
    base['Biome'].addTo(map)

    // helper to share the timeDimension object between all layers
    map.timeDimension = timeDimension; 
    layersControl.addTo(map)
    L.control.scale({imperial: false}).addTo(map)
    sidebarToggle.addTo(map)
    map.setView([0,0], 17)

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

    var command = function(message) {
      switch (message.kind) {
        case 'setView':
          var center = map.getCenter()
          if (center.lng != message.x || center.lat != message.y || map.getZoom() != message.z) {
            map.setView([message.y, message.x], message.z)
          }
          break
        case 'monumentList':
          updateMonumentLayer(monumentOverlay, message.monuments.data)
          monumentOverlay.addTo(map)
          break;
        case 'dataLayer':
          setDataLayers(message.lives.data)
          if(!map.hasLayer(dataOverlay)) {
            map.addLayer(dataOverlay)
          }
          break;
        case 'beginPlayback':
          timeDimension.setCurrentTime(message.start_time)
          player.setTransitionTime(1000 / (message.frame_rate || 1))
          player.stop()
          player.start(message.game_seconds_per_frame)
          break;
        case 'playbackScale':
          player.stop()
          player.start(message.game_seconds_per_frame)
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
          map.setView([life.birth_y, life.birth_x], 24)
          break
        case 'searchOverlay':
          if (message.status) {
            searchOverlay.addTo(map)
          } else {
            searchOverlay.remove()
          }
          break
        case 'animOverlay':
          if (message.status) {
            dataOverlay.addLayer(animOverlay)
            dataOverlay.removeLayer(pointOverlay)
          } else {
            dataOverlay.removeLayer(animOverlay)
            dataOverlay.addLayer(pointOverlay)
          }
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
