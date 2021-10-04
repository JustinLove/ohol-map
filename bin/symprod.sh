#!/usr/bin/env bash
s3cmd --add-header="Cache-Control:max-age=86400" sync public/symbol-defs.svg s3://wondible-com-onemap/
