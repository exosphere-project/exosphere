FROM selenium/standalone-chrome:4

USER root

# Install node and npm
RUN curl -sL https://deb.nodesource.com/setup_15.x | bash - \
    && apt install --yes nodejs \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /usr/src/app

COPY . .
RUN chown -R seluser .

USER seluser

RUN if [ -d home/.elm ]; then echo "Restoring ~/.elm"; mv home/.elm ~/; fi
RUN if [ -d home/.npm ]; then echo "Restoring ~/.npm"; mv home/.npm ~/; fi

RUN npm install elm@0.19.1-3 http-server
RUN npx elm make src/Exosphere.elm --output elm-web.js

COPY integration-tests/docker/supervisor/exosphere.conf /etc/supervisor/conf.d/
COPY integration-tests/docker/supervisor/selenium.conf /etc/supervisor/conf.d/
COPY integration-tests/docker/supervisor/add-exosphere-localhost.sh /opt/bin/
COPY integration-tests/docker/supervisor/start-exosphere.sh /opt/bin/
COPY environment-configs/integration-tests-service.exosphere.app-config.js config.js

USER root

EXPOSE 8000
