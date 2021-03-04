#!/usr/bin/env bash
elm make src/MapUI.elm --optimize --output public/map-ui.js
s3cmd --add-header="Cache-Control:max-age=86400" --exclude public/index.html --exclude public/favicon.ico sync public/* s3://wondible-com-onemap/
s3cmd --add-header="Cache-Control:max-age=31536000" sync public/favicon.ico s3://wondible-com-onemap/
s3cmd --add-header="Cache-Control:no-cache" sync public/index.html s3://wondible-com-onemap/
