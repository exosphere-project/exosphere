#!/usr/bin/env sh

IMAGE_NAME="${1:-exosphere}"
CONTAINER_NAME="${2:-exosphere}"

docker run --rm \
       --mount type=bind,source="${PWD}"/elm.json,destination=/usr/src/app/elm.json \
       --mount type=bind,source="${PWD}"/elm-tooling.json,destination=/usr/src/app/elm-tooling.json \
       --mount type=bind,source="${PWD}"/elm-git.json,destination=/usr/src/app/elm-git.json \
       --mount type=bind,source="${PWD}"/index.html,destination=/usr/src/app/index.html \
       --mount type=bind,source="${PWD}"/service-worker.js,destination=/usr/src/app/service-worker.js \
       --mount type=bind,source="${PWD}"/config.js,destination=/usr/src/app/config.js \
       --mount type=bind,source="${PWD}"/ports.js,destination=/usr/src/app/ports.js \
       --mount type=bind,source="${PWD}"/cloud_configs.js,destination=/usr/src/app/cloud_configs.js \
       --mount type=bind,source="${PWD}"/exosphere.webmanifest,destination=/usr/src/app/exosphere.webmanifest \
       --mount type=bind,source="${PWD}"/src,destination=/usr/src/app/src \
       --mount type=bind,source="${PWD}"/assets,destination=/usr/src/app/assets \
       --mount type=bind,source="${PWD}"/fonts,destination=/usr/src/app/fonts \
       -it \
       --name $CONTAINER_NAME \
       -p 127.0.0.1:8000:8000 \
       $IMAGE_NAME
