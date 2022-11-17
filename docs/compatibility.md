# Exosphere Compatibility

## To use with an OpenStack Cloud

- Exosphere works with OpenStack Queens version (released February 2018) or later.
- Exosphere works best with clouds that have [automatic allocation of network topology](https://docs.openstack.org/neutron/latest/admin/config-auto-allocation.html) enabled.

## Supported Instance Operating Systems

Exosphere works best with instances launched from images based on **Ubuntu 22.04 and 20.04, AlmaLinux, and Rocky Linux**. Ubuntu 18.04 and CentOS 7 are also supported, but they receive less attention when testing new features. Exosphere can launch instances that run other operating systems, but some features and integrations are likely to not work.

For example: the one-click graphical desktop feature, only works with Ubuntu 20.04 and newer, AlmaLinux, and Rocky Linux.

If your community relies on an operating system that we don't currently support, please [create an issue](https://gitlab.com/exosphere/exosphere/-/issues) explaining your need! It's probably not hard to add support for Linux distros that use systemd and an APT/YUM/DNF package system.
