#!/usr/bin/env python3

import argparse
import typing as t
import subprocess
import pathlib
import logging


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


CEPH_CONFIG_PATH = pathlib.Path("/etc/ceph/")
SYSTEMD_PATH = pathlib.Path("/etc/systemd/system/")
MOUNT_PATH = pathlib.Path("/media/share/")
FSTAB_PATH = pathlib.Path("/etc/fstab")


def systemd_escape_path(val: str) -> str:
    return subprocess.check_output(
        [
            "systemd-escape",
            "--path",
            val,
        ],
        text=True,
    ).strip()


DEFAULT_MOUNT_OPTIONS = ["noatime", "rw", "_netdev", "auto", "nofail"]
MOUNT_TEMPLATE = """
[Unit]
Description=Share {share_name}
Before={service_name}

[Install]
WantedBy=remote-fs.target
Requires={service_name}

[Mount]
What={share_path}
Where=/media/share/{share_name}
DirectoryMode=0755
Type=ceph
Options={options}
"""

SERVICE_TEMPLATE = """
[Unit]
Description="Ensure share permissions are set for {share_name}"
BindsTo={mount_name}
After={mount_name}

[Install]
RequiredBy={mount_name}

[Service]
Type=OneShot
ExecStart=/usr/bin/env chown exouser:exouser "/media/share/{share_name}"
"""


def do_mount(
    share_name: str,
    share_path: str,
    access_rule_name: t.Optional[str] = None,
    access_rule_key: t.Optional[str] = None,
):
    mount_point = MOUNT_PATH / share_name
    mount_options = list(DEFAULT_MOUNT_OPTIONS)
    escaped_name = systemd_escape_path(str(mount_point))

    if access_rule_name:
        mount_options.append(f"name={access_rule_name}")

    if access_rule_key:
        mount_options.append(f"secret={access_rule_key}")

    systemd_mount_path = (SYSTEMD_PATH / escaped_name).with_suffix(".mount")
    systemd_service_path = (SYSTEMD_PATH / escaped_name).with_suffix(".service")

    systemd_mount_path.write_text(
        MOUNT_TEMPLATE.format(
            share_path=share_path,
            share_name=share_name,
            access_rule_name=access_rule_name,
            service_name=systemd_service_path.name,
            options=",".join(mount_options),
        )
    )

    systemd_service_path.write_text(
        SERVICE_TEMPLATE.format(
            share_name=share_name,
            mount_name=systemd_mount_path.name,
        )
    )

    # Enable and start
    subprocess.check_call(
        (
            "systemctl",
            "enable",
            "--now",
            systemd_mount_path.name,
            systemd_service_path.name,
        )
    )


def do_unmount(share_name: str):
    mount_point = MOUNT_PATH / share_name
    escaped_name = systemd_escape_path(str(mount_point))

    systemd_mount_path = (SYSTEMD_PATH / escaped_name).with_suffix(".mount")
    systemd_service_path = (SYSTEMD_PATH / escaped_name).with_suffix(".service")

    subprocess.check_call(
        (
            "systemctl",
            "disable",
            "--now",
            systemd_mount_path.name,
            systemd_service_path.name,
        )
    )
    systemd_mount_path.unlink()
    systemd_service_path.unlink()

    subprocess.check_call(
        (
            "systemctl",
            "daemon-reload",
        )
    )


parser = argparse.ArgumentParser()
parser.set_defaults(action=parser.print_help)

subparsers = parser.add_subparsers()
mount_parser = subparsers.add_parser(
    "mount",
    help="Tool for managing ceph keyrings and mounts",
)
mount_parser.add_argument("--access-rule-name")
mount_parser.add_argument("--access-rule-key")
mount_parser.add_argument("--share-name", required=True)
mount_parser.add_argument("--share-path", required=True)
mount_parser.set_defaults(action=do_mount)

unmount_parser = subparsers.add_parser(
    "unmount",
    help="Tool for unmounting and cleaning up ceph keyrings and mounts",
)
unmount_parser.add_argument("--share-name", required=True)
unmount_parser.set_defaults(action=do_unmount)

if __name__ == "__main__":
    args = dict(parser.parse_args().__dict__)
    action = args.pop("action")
    action(**args)
