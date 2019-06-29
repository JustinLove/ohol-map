;(function() {
  var app = Elm.MapUI.init({flags: oholMapConfig})

  var cachedApiUrl = oholMapConfig.cachedApiUrl
  var apiUrl = oholMapConfig.apiUrl

  var scale = Math.pow(2, 24)
  var crs = L.extend({}, L.CRS.Simple, {
    transformation: new L.transformation(1/scale, 0.5/scale, -1/scale, -0.5/scale)
  })

  var base = {};

  base['Default'] = L.tileLayer(oholMapConfig.mainTiles, {
    errorTileUrl: 'ground_U.png',
    minZoom: 2,
    maxZoom: 27,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    attribution: '<a href="https://onehouronelife.com">Jason Rohrer</a> wondible',
  })

  base['Faded'] = L.tileLayer(oholMapConfig.mainTiles, {
    errorTileUrl: 'ground_U.png',
    minZoom: 2,
    maxZoom: 27,
    //minNativeZoom: 24,
    maxNativeZoom: 24,
    opacity: 0.2,
    attribution: '<a href="https://onehouronelife.com">Jason Rohrer</a> wondible',
  })

  base['Crucible'] = L.tileLayer(oholMapConfig.crucibleTiles, {
    errorTileUrl: 'ground_U.png',
    minZoom: 2,
    maxZoom: 27,
    //minNativeZoom: 24,
    maxNativeZoom: 25,
    attribution: '<a href="https://onehouronelife.com">Jason Rohrer</a> wondible',
  });

  var overlays = {
    graticule: null,
    Search: L.layerGroup([]),
    "48h Births": null,
    "48h Births Anim": null,
  }

  var focusMarker = null;

  onMonumentLayerAdd = function(e) {
    var layer = e.target

    if (layer.options.data) return

    var server_id = layer.options.server_id

    //fetch("data/" + server + ".onehouronelife.com_monuments.json").then(function(response) {
    fetch(cachedApiUrl + "monuments?server_id=" + server_id).then(function(response) {
      response.json().then(function(wrapper){
        //console.log('monuments', wrapper)
        var now = new Date()
        var data = wrapper.data
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
        if (layer.options.startEnabled) {
          layer.addTo(layer._map);
          var last = data[data.length-1]
          //layer._map.setView([last.y, last.x], 17)
        }
      })
    })
  }


  var colormap = function(id) {
    return '#' + (((id * 49157) % 12582917).toString(16))
  }

  var colorhash = function(hash) {
    return '#' + hash.slice(0,6)
  }

  L.GridLayer.PointOverlay = L.GridLayer.extend({
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

      //var timebase = this.options.min
      //var timescale = this.options.max - timebase
      //var point = this.options.data[0]
      //var t = (point.birth_time - timebase) * 100 / timescale
      var fadeTime = 60*60
      this.options.data.forEach(function(point) {
        //console.log(point)

        var t = 1
        var r = 1;
        if ( llnw.lng < point.birth_x && point.birth_x < llse.lng
          && llse.lat < point.birth_y && point.birth_y < llnw.lat) {
          if (time) {
            if (point.birth_time < (time - fadeTime) || time < point.birth_time) {
              return
            }

            var a = 1 - (time - point.birth_time) / fadeTime
            ctx.globalAlpha = a
            t = (time - point.birth_time) / fadeTime
            r = Math.pow(10 * t, -6 * t)
          } else {
            t = 1
            ctx.globalAlpha = 0.5
          }
          var ll = L.latLng(point.birth_y, point.birth_x)
          var p = crs.latLngToPoint(ll, coords.z)
          //console.log(ll, p, pnw)
          p.x = p.x - origin.x
          p.y = p.y - origin.y
          //console.log(p)
          //var t = (point.birth_time - timebase) * 75 / timescale
          //ctx.fillStyle = "hsla(240, 100%, " + t + "%, " + a + ")"
          ctx.fillStyle = point.lineage
          ctx.strokeStyle = point.lineage
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
  })


  var timeDimension = new L.TimeDimension({
    //timeInterval: start + "/" + end,
    //period: "PT1M",
  });
  // helper to share the timeDimension object between all layers

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
    speedStep: 0.5,
    maxSpeed: 30,
    timeSliderDragUpdate: true
  };

  var timeDimensionControl = new L.Control.TimeDimension(timeDimensionControlOptions);

  var pointOverlay = new L.GridLayer.PointOverlay()
  overlays["48h Births"] = pointOverlay

  var animOverlay = new L.GridLayer.PointOverlay({
    attribution: '<a href="https://github.com/socib/Leaflet.TimeDimension">socib/Leaflet.TimeDimension</a>',
  })
  timeDimension.on("timeload", animOverlay.updateTiles, animOverlay)
  overlays["48h Births Anim"] = animOverlay

  var resultPoints = new L.GridLayer.PointOverlay().addTo(overlays['Search'])

  var requireRecentLives = function() {
    if (animOverlay.options.data) return

    //fetch("data/bigserver2_points_48.json").then(function(response) {
    //fetch("data/EVE COLIN.json").then(function(response) {
    //fetch("data/bigserver2_points_hash.json").then(function(response) {
    fetch(cachedApiUrl + "lives?server_id=17&period=P2D").then(function(response) {
      response.json().then(function(wrapper){
        //console.log('lives', wrapper)
        var min = null;
        var max = null;
        var data = wrapper.data;
        data.forEach(function(point) {
          if (min == null || point.birth_time < min) {
            min = point.birth_time
          }
          if (max == null || point.birth_time > max) {
            max = point.birth_time
          }
          point.lineage = colormap(point.lineage)
          if (point.hash) {
            point.hash = colorhash(point.hash)
          }
        })
        var times = []
        for (var t = min;t < max;t += 60) {
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
      })
    })
  }

  animOverlay.on('add', function(ev) {
    ev.target._map.addControl(timeDimensionControl)
    requireRecentLives()
  })
  animOverlay.on('remove', function(ev) {
    ev.targtet._map.removeControl(timeDimensionControl)
  })

  pointOverlay.on('add', requireRecentLives)

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

  layersControl = L.control.layers(base, overlays)

  var fetchMonuments = function fetchMonuments(map) {
    fetch(cachedApiUrl + "servers").then(function(response) {
      response.json().then(function(wrapper){
        //console.log('monuments', wrapper)
        var data = wrapper.data
        data.forEach(function(server) {
          var short = server.server_name.replace('.onehouronelife.com', '')
          title = short + ' Monuments'
          overlays[title] = L.layerGroup([], {
            server_id: server.id,
            startEnabled: short == 'bigserver2',
          });
          overlays[title].on('add', onMonumentLayerAdd)
          layersControl.addOverlay(overlays[title], title)
          if (short == 'bigserver2') {
            overlays[title].addTo(map);
          }
        })
      })
    })
  }

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

  var inhabit = function inhabit(id) {
    var map = L.map(id, {
      crs: crs,
      maxBounds: [[-2147483648, -2147483648], [2147483647, 2147483647]],
      minZoom: 2,
      maxZoom: 27,
    })

    base['Default'].addTo(map)
    overlays['Search'].addTo(map)

    map.timeDimension = timeDimension; 
    layersControl.addTo(map)
    L.control.scale({imperial: false}).addTo(map)
    map.setView([0,0], 17)
    fetchMonuments(map)

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
            .addTo(overlays['Search'])
            .openPopup()
          map.setView([life.birth_y, life.birth_x], 17)
          break
      }
    }

    if (app.ports.leafletCommand) {
      app.ports.leafletCommand.subscribe(command)
    }
  }


  inhabit('map')
})()
