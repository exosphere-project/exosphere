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
RUN npm install

# Add remainder of files
COPY . .
RUN npx elm make src/Exosphere.elm --output public/elm-web.js

EXPOSE 8000

#ENTRYPOINT ["npm", "run live"]
