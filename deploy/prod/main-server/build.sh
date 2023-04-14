#!/bin/bash

noderun="docker run -it --rm -v $PWD:/usr/src/app -w /usr/src/app node"

$noderun npm ci
mkdir -p public
$noderun npm run build:prod
$noderun npm run minify
cp elm-web.js public/elm-web.js
cp index.html public/index.html
cp ports.js public/ports.js
cp cloud_configs.js public/cloud_configs.js
cp config.js public/config.js
cp exosphere.webmanifest public
cp -R assets public
cp -R fonts public
cp service-worker.js public