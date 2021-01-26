#!/usr/bin/env bash

cd /usr/src/app || exit
npx elm-live src/Exosphere.elm \
  --proxy-prefix '/proxy' --proxy-host 'https://try-dev.exosphere.app/proxy' \
  --host 0.0.0.0 --start-page index.html \
  -- --output=elm-web.js
