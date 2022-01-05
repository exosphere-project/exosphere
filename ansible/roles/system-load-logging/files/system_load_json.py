#!/usr/bin/env python3

# Outputs system resource usage on one line as JSON.
# Uses only the standard library shipped with CentOS 8 and Ubuntu 20 cloud images.
# Requires "top", "free", and "df" installed.

import json
import re
import subprocess
import sys
import time


def cmd_stdout_lines(cmd_list):
    return subprocess.run(cmd_list, stdout=subprocess.PIPE) \
        .stdout \
        .decode() \
        .split("\n")


def cpu():
    # Returns percent CPU non-idle
    top_output_lines = cmd_stdout_lines(["top", "-b", "-n", "1"])
    cpu_line = \
        next(
            filter(
                (lambda l: l[:4] == "%Cpu"),
                top_output_lines)
        )
    cpu_idle_pct = float(cpu_line.split(",")[3][:5])
    cpu_used_pct = round(100 - cpu_idle_pct)
    return cpu_used_pct


def mem():
    # Returns percent of total memory not available
    # https://www.linuxatemyram.com/
    free_output_lines = cmd_stdout_lines(["free", "-t", "-m"])
    total_line = \
        next(
            filter(
                (lambda l: l[:4] == "Mem:"),
                free_output_lines)
        )
    mem_avail_mb = int(total_line.split()[6])
    mem_total_mb = int(total_line.split()[1])
    mem_unavail_pct = round(100 - mem_avail_mb / mem_total_mb * 100)
    return mem_unavail_pct


def disk():
    # Returns percent of root filesystem used
    df_output_lines = cmd_stdout_lines(["df", "/"])
    rootfs_line = \
        next(
            filter(
                (lambda l: l.split()[-1] == "/"),
                df_output_lines
            )
        )
    rootfs_used_pct = rootfs_line.split()[-2][:-1]
    return int(rootfs_used_pct)


def gpu():
    try:
        cmd = ["nvidia-smi", "--query", "--display=UTILIZATION"]
        output = cmd_stdout_lines(cmd)
        output_stripped_whitespace = [line.strip() for line in output]
        in_utilization_section = False
        for line in output_stripped_whitespace:
            if in_utilization_section:
                if line.startswith("Gpu"):
                    match = re.search(":\s*(\d+)\s*%", line)
                    if match:
                        gpu_used_percent = int(match.group(1))
                        return gpu_used_percent

            elif line.startswith("Utilization"):
                in_utilization_section = True
            else:
               pass
    except FileNotFoundError:
        return None
    return None


json_dict = {
    "epoch": round(time.time()),
    "cpuPctUsed": cpu(),
    "memPctUsed": mem(),
    "rootfsPctUsed": disk(),
    "gpuPctUsed": gpu()
}
output_line = json.dumps(json_dict) + "\n"
sys.stdout.buffer.write(output_line.encode())
