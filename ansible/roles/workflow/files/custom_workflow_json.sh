#!/usr/bin/env bash

set -e -o pipefail

/usr/bin/docker exec custom-workflow.service jupyter notebook list \
  | grep -m 1 -Po '(token=)\K[a-f0-9]+' \
  | xargs -I% echo '{"exoWorkflowToken":"%"}' \
  > $1
