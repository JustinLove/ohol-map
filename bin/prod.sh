#!/usr/bin/env bash
elm make src/MapUI.elm --output public/map-ui.js
s3cmd sync public/* s3://wondible-com-onemap/
