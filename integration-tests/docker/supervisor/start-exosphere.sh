#!/usr/bin/env bash

cd /usr/src/app
npx elm-live src/Exosphere.elm \
  --proxy-prefix '/proxy' --proxy-host 'https://try-dev.exosphere.app/proxy' \
  --host 0.0.0.0 --pushstate true --verbose --start-page index.html --hot true \
  -- --output=elm-web.js
