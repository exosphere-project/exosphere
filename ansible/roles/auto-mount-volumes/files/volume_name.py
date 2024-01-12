#!/usr/bin/env python3

"""
Get the name of a volume from the openstack instance metadata

Usage:
    python volume-name.py e2f5a828-2cd2-4834-bf90-e6efff2c35bb

When attaching a volume, Exosphere will add metadata per volume to the instance
    Key: exoVolumes::e2f5a828-2cd2-4834-bf90-e6efff2c35bb
    Value: {"e2f5a828-2cd2-4834-bf90-e6efff2c35bb":"volume-name"}
"""

import json
import urllib.request
import time
import sys


OPENSTACK_METADATA_URL = "http://169.254.169.254/openstack/latest/meta_data.json"
RETRIES = 3
DELAY = 1


def get_volume_names():
    with urllib.request.urlopen(
        "http://169.254.169.254/openstack/latest/meta_data.json"
    ) as response:
        openstack_meta_data = json.load(response)

    volume_names = {}
    for key, value in openstack_meta_data["meta"].items():
        if key.startswith("exoVolumes::"):
            volume_names.update(json.loads(value))

    return volume_names


def get_volume_name(uuid, retries=RETRIES, delay=DELAY):
    for remaining_retries in range(retries, 0, -1):
        volume_names = get_volume_names()

        if uuid in volume_names:
            return volume_names[uuid]

        if remaining_retries:
            print(
                "Did not find volume {} in the instance metadata, retrying {} more times".format(
                    uuid, remaining_retries
                ),
                file=sys.stderr,
            )
            time.sleep(delay)

    return uuid


if __name__ == "__main__":
    volume_uuid = sys.argv[1]

    print(get_volume_name(volume_uuid))
