#!/usr/bin/env bash

grep app.exosphere.localhost /etc/hosts || echo '127.0.0.1       app.exosphere.localhost' >> /etc/hosts
