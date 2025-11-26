FROM node:current-bookworm

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
COPY elm-git.json ./
COPY elm-watch.json ./
COPY elm.sideload.json ./
COPY postprocess.js ./
RUN npm install

EXPOSE 8000

CMD ELM_WATCH_HOST=0.0.0.0 npx elm-watch hot
