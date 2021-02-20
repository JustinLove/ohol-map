#!/usr/bin/env bash
elm make src/MapUI.elm --output public/map-ui.js
s3cmd --add-header="Cache-Control:max-age=600" --exclude public/index.html sync public/* s3://wondible-com-twomap/
s3cmd --add-header="Cache-Control:no-cache" sync public/index.html s3://wondible-com-twomap/
