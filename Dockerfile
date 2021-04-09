FROM node:current-buster

# Note: this docker build is intended for local development only
# docker build -t exosphere .

# Create app directory
WORKDIR /usr/src/app

RUN apt-get update && \
    apt-get install -y \
       --no-install-recommends \
           curl \
           gzip && \
           rm -rf /var/lib/apt/lists/*

# Install and cache dependencies
COPY package*.json ./
COPY elm.json ./
COPY elm-tooling.json ./
RUN npm install \
    && npm install --no-save elm-live

# Add remainder of files
COPY index.html .
COPY service-worker.js .
COPY environment-configs/docker-config.js ./config.js
COPY ports.js .
COPY exosphere.webmanifest .
COPY src ./src
COPY assets ./assets
COPY fonts ./fonts
RUN npx elm make src/Exosphere.elm --output elm-web.js

EXPOSE 8000

CMD npx elm-live src/Exosphere.elm --pushstate true --proxy-prefix '/proxy' --proxy-host 'https://try-dev.exosphere.app/proxy' --host 0.0.0.0 --verbose --start-page index.html --hot true -- --output=elm-web.js
