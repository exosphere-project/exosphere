FROM node:10

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
RUN npm install && \
    npm install -g http-server && \
    curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz && \
    gunzip elm.gz && \
    chmod +x elm && \
    mv elm /usr/local/bin/

# Add remainder of files
COPY . .

RUN git submodule sync --recursive && \
    git submodule update --init --recursive

RUN elm make src/Exosphere.elm --output elm.js
EXPOSE 8080

ENTRYPOINT ["http-server"]
