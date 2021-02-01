#!/usr/bin/env sh

nohup cloudflared tunnel --no-autoupdate --url tcp://e2e.exosphere.service:5900 &
