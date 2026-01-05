#!/usr/bin/env python3

import textwrap
import argparse
import logging
import pathlib
import subprocess
import sys
import typing as t

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
logger = logging.getLogger(sys.argv[0])

#
# Constants and templates
#

SYSTEMD_PATH = pathlib.Path("/etc/systemd/system/")
MOUNT_PATH = pathlib.Path("/media/share/")

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

#
# Helper types and functions
#


def systemd_escape_path(val: str) -> str:
    r"""
    Uses systemd-escape to escape path names as systemd mount file names

    Example:
        systemd_escape_path("/media/share/test-share") == "media-share-test\x2dshare"
    """

    return subprocess.check_output(
        [
            "systemd-escape",
            "--path",
            val,
        ],
        text=True,
    ).strip()


class OpenFile(t.NamedTuple):
    """Represents an open file as determined by lsof"""

    pid: int
    command: str
    fd: t.Optional[str]
    name: t.Optional[str]


def split_iterable(
    predicate: t.Callable[[t.T], bool],
    iterable: t.Iterable[t.T],
    *,
    before_predicate: bool = True,
) -> t.Iterable[t.List[t.T]]:
    """
    Split an iterable into chunks, separating on a predicate

    Empty chunks (e.g, where the first element matches the predicate) are not yielded
    """

    chunk = []
    for element in iterable:
        if predicate(element):
            if not before_predicate:
                chunk.append(element)

            if chunk:
                yield chunk
                chunk.clear()

            if before_predicate:
                chunk.append(element)

        else:
            chunk.append(element)

    if chunk:
        yield chunk


def iter_open_files(path: pathlib.Path) -> t.Iterable[OpenFile]:
    """
    Parses the output of `lsof -F pfcn` to find any open files within a path

    `-F` is "Output for processing by other programs"
    `pfcn` selects the pid, command, fd, and filename for output

    lsof outputs the following for each open file
        p[pid]
        c[command]
        f[fd]
        n[file_path]

    Note: f/n may not be included in some cases
    """

    lsof = subprocess.run(
        ("lsof", "-F", "pcfn", str(path)),
        capture_output=True,
        check=False,
    )

    output_lines = lsof.stdout.decode().splitlines()

    for open_file_lines in split_iterable(lambda x: x.startswith("p"), output_lines):
        open_file_data = {l[0]: l[1:] for l in open_file_lines}

        yield OpenFile(
            pid=int(open_file_data.get("p", "-1")),
            command=open_file_data.get("c", "[unknown]"),
            fd=open_file_data.get("f", None),
            name=open_file_data.get("n", None),
        )


#
# Command functions
#


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

    systemd_mount = MOUNT_TEMPLATE.format(
        share_path=share_path,
        share_name=share_name,
        access_rule_name=access_rule_name,
        service_name=systemd_service_path.name,
        options=",".join(mount_options),
    )
    systemd_service = SERVICE_TEMPLATE.format(
        share_name=share_name,
        mount_name=systemd_mount_path.name,
    )

    logger.info("Creating %s", systemd_mount_path, extra={"content": systemd_mount})
    systemd_mount_path.write_text(systemd_mount)

    logger.info("Creating %s", systemd_service_path, extra={"content": systemd_service})
    systemd_service_path.write_text(systemd_service)

    # Enable and start
    try:
        subprocess.run(
            (
                "systemctl",
                "enable",
                "--now",
                systemd_mount_path.name,
                systemd_service_path.name,
            ),
            check=True,
            capture_output=True,
        )

    except subprocess.CalledProcessError as e:
        logger.error(
            "Failed to start mount scripts:\n%s",
            textwrap.indent((e.stderr or e.stdout).decode(), "  "),
        )

    else:
        print(f"Successfully mounted at {mount_point}")


def do_unmount(share_name: str):
    mount_point = MOUNT_PATH / share_name
    escaped_name = systemd_escape_path(str(mount_point))

    systemd_mount_path = (SYSTEMD_PATH / escaped_name).with_suffix(".mount")
    systemd_service_path = (SYSTEMD_PATH / escaped_name).with_suffix(".service")

    if systemd_mount_path.exists() or systemd_service_path.exists():
        try:
            logger.debug(
                "Disabling services %s and %s",
                systemd_mount_path.name,
                systemd_service_path.name,
            )
            subprocess.run(
                (
                    "systemctl",
                    "disable",
                    "--now",
                    systemd_mount_path.name,
                    systemd_service_path.name,
                ),
                check=True,
                capture_output=True,
            )

        except subprocess.CalledProcessError as e:
            open_files = list(iter_open_files(mount_point))
            max_file_name_length = max(len(f.name) for f in open_files)

            logger.error(
                "Failed to stop mount:\n%s\n\nOpen files:\n%s",
                textwrap.indent((e.stderr or e.stdout).decode(), "  "),
                textwrap.indent(
                    "\n".join(
                        f"{f.name.ljust(max_file_name_length)}  (Open in {f.pid}: {f.command})"
                        for f in iter_open_files(mount_point)
                    ),
                    "  ",
                ),
            )

            sys.exit(1)

        else:
            logger.debug(
                "Deleting %s and %s",
                systemd_mount_path.name,
                systemd_service_path.name,
            )
            systemd_mount_path.unlink(missing_ok=True)
            systemd_service_path.unlink(missing_ok=True)

            subprocess.run(
                (
                    "systemctl",
                    "daemon-reload",
                ),
                check=False,
                capture_output=True,
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
