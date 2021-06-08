#!/usr/bin/env bash

set -e

workflow_token=`docker exec workflow jupyter notebook list | grep -m 1 -Po '(token=)\K[a-f0-9]+'`

echo "{\"exoWorkflowToken\":\"$workflow_token\"}"
