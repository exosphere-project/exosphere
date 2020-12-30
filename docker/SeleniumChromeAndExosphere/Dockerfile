FROM selenium/standalone-chrome:4

USER root

# Install node and npm
RUN curl -sL https://deb.nodesource.com/setup_15.x | bash - \
    && apt install --yes nodejs \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /usr/src/app

# Install and cache dependencies
COPY package*.json ./
RUN chown -R seluser . \
    && su - seluser -c "cd /usr/src/app; npm install" \
    && chown -R seluser .

# Add remainder of files
COPY . .
RUN chown seluser .

USER seluser
RUN npx elm make src/Exosphere.elm --output public/elm-web.js

COPY docker/supervisor/exosphere.conf /etc/supervisor/conf.d/
COPY docker/supervisor/start-exosphere.sh /opt/bin/
COPY environment-configs/elm-live.exosphere.app-config.js config.js

EXPOSE 8000
