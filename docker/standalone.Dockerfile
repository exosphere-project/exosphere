FROM node:current-buster AS compile

# Note: This docker build is intended for building a standalone container,
# including a local CCP (Cloud CORS Proxy).

# docker build -t exosphere -f ./docker/standalone.Dockerfile .

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
RUN npm install

# Copy Elm source
COPY src ./src

# Compile app
RUN npm run build:prod

# Minify app
RUN npm run minify

FROM nginx:alpine

COPY docker/nginx/standalone.conf /etc/nginx/conf.d/default.conf
COPY docker/nginx/compression.conf /etc/nginx/conf.d/compression.conf

WORKDIR /usr/share/nginx/html

COPY --from=compile /usr/src/app/elm-web.js ./

# Add remainder of files to Nginx image
COPY index.html .
COPY service-worker.js .
COPY environment-configs/docker-config.js ./config.js
COPY ports.js .
COPY cloud_configs.js .
COPY exosphere.webmanifest .
COPY assets ./assets
COPY fonts ./fonts

EXPOSE 8000
