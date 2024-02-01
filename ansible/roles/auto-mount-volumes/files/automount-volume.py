#!/usr/bin/env python3
import urllib.request
import json
import sys
import subprocess
import argparse
import pathlib
import typing
import time
import re


MOUNT_PATH = pathlib.Path("/media/volume")


def sanitize(label: str) -> str:
    return re.sub(r"\W+", "-", label)


def udevadm_info(device: pathlib.Path) -> dict[str, str]:
    return dict(
        [
            val.split("=", maxsplit=1)
            for val in subprocess.check_output(
                ["udevadm", "info", "--query=property", str(device)]
            )
            .decode("utf-8")
            .splitlines()
        ]
    )


def get_disk_uuid(device: pathlib.Path) -> typing.Optional[str]:
    device_info = udevadm_info(device)
    return device_info.get("ID_SERIAL_SHORT")


def get_openstack_metadata():
    with urllib.request.urlopen(
        "http://169.254.169.254/openstack/latest/meta_data.json"
    ) as response:
        return json.load(response)


def get_all_volume_metadata():
    metadata = get_openstack_metadata()
    volumes = {}
    for k, v in metadata["meta"].items():
        if k.startswith("exoVolumes::"):
            uuid = k.split("::", maxsplit=1)[1]
            volumes[uuid] = json.loads(v)
    return volumes


def get_volume_name(uuid, *, retries=0, default=None):
    while retries >= 0:
        names = get_all_volume_metadata()
        if uuid in names:
            return names[uuid]["name"]

        if retries:
            time.sleep(1)
        retries -= 1

    return default


def get_mounts(device: pathlib.Path):
    with open("/proc/mounts", "r", encoding="utf-8") as f:
        for line in f:
            mount = line.strip().split()
            if mount[0] == str(device):
                yield mount


def log_exec(cmd, *args, _ignore_errors=False, **kwargs):
    print("Calling", cmd, "(", *args, kwargs, ")")
    try:
        return subprocess.check_output(cmd, *args, **kwargs)
    except subprocess.CalledProcessError:
        if not _ignore_errors:
            raise


def do_mount(device: pathlib.Path):
    print(f"do_mount({device=})", file=sys.stderr)
    disk_info = udevadm_info(device)

    uuid = disk_info.get("ID_SERIAL_SHORT")
    volume_name = get_volume_name(uuid, default=device.name, retries=3)
    mountpoint: pathlib.Path = MOUNT_PATH / sanitize(volume_name)

    if disk_info.get("ID_FS_USAGE", None) != "filesystem":
        print(f"formatting {device} to ext4")
        # Options borrowed from https://github.com/systemd/systemd/blob/0e2f18eedd/src/shared/mkfs-util.c#L418-L426
        log_exec(
            (
                "mkfs.ext4",
                "-L",  # Volume label
                volume_name[:16],
                "-U",  # Volume UUID, lets match OpenStack!
                uuid,
                "-I",  # Inode size
                "256",
                "-m",  # Reserved blocks percentage
                "0",
                "-E",  # faster formatting, copied from systemd-makefs
                "nodiscard,lazy_itable_init=1",
                "-b",  # Block size
                "4096",
                str(device),
            )
        )

    elif disk_info.get("ID_FS_LABEL", "") != volume_name:
        print("Fixing volume label")
        log_exec(("e2label", str(device), volume_name))

    print(f"mounting {device} to /media/volumes/{volume_name}")
    mountpoint.mkdir(mode=0o755, parents=True, exist_ok=True)
    log_exec(
        (
            "systemd-mount",
            "--options",
            "user,exec,rw,auto,nofail,X-mount.owner=exouser,X-mount.group=exouser,x-systemd.device-timeout=1s",
            "--collect",
            str(device),
            str(mountpoint),
        )
    )
    log_exec(("/usr/bin/chown", "exouser:exouser", str(mountpoint)))


def do_unmount(device: pathlib.Path):
    print(f"do_unmount({device=})", file=sys.stderr)

    for _, mountpoint, *_ in get_mounts(device):
        log_exec(("/usr/bin/env", "systemd-mount", "--unmount", mountpoint))
        log_exec(("/usr/bin/rmdir", mountpoint))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    subparsers = parser.add_subparsers(required=True)
    mount_parser = subparsers.add_parser("mount")
    unmount_parser = subparsers.add_parser("unmount")

    mount_parser.add_argument("device", type=pathlib.Path)
    mount_parser.set_defaults(action=do_mount)

    unmount_parser.add_argument("device", type=pathlib.Path)
    unmount_parser.set_defaults(action=do_unmount)

    args = parser.parse_args()
    args.action(args.device)
