FROM selenium/standalone-chrome:4 as standalone-chrome-nodejs

USER root

# Install node and npm
RUN curl -sL https://deb.nodesource.com/setup_18.x | bash - \
    && apt install --yes nodejs \
    && rm -rf /var/lib/apt/lists/* \
    && npm install -g http-server

FROM standalone-chrome-nodejs

# Create app directory
WORKDIR /usr/src/app

COPY . .
RUN chown -R seluser .

USER seluser

COPY integration-tests/docker/supervisor/exosphere.conf /etc/supervisor/conf.d/
COPY integration-tests/docker/supervisor/selenium.conf /etc/supervisor/conf.d/
COPY integration-tests/docker/supervisor/add-exosphere-localhost.sh /opt/bin/
COPY integration-tests/docker/supervisor/start-exosphere.sh /opt/bin/

USER root

EXPOSE 8000
