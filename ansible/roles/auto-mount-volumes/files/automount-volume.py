#!/usr/bin/env python3
import urllib.request
import json
import sys
import subprocess
import argparse
import pathlib
import typing

MOUNT_PATH = pathlib.Path("/media/volume")


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


def get_volume_names():
    metadata = get_openstack_metadata()
    volumes = {}
    for k, v in metadata["meta"].items():
        if k.startswith("exoVolumes::"):
            volumes.update(**json.loads(v))
    return volumes


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
    uuid = get_disk_uuid(device)
    volume_name = get_volume_names().get(uuid)
    mountpoint: pathlib.Path = MOUNT_PATH / volume_name

    print(f"mounting {device} to /media/volumes/{volume_name}")

    mountpoint.mkdir(mode=0o755, parents=True, exist_ok=True)
    log_exec(("/lib/systemd/systemd-makefs", "ext4", str(device)), _ignore_errors=True)
    log_exec(
        (
            # "/usr/bin/env",
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
