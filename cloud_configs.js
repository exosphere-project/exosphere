"use strict";

/** @type {Exosphere.CloudConfigs} */
var cloud_configs = {
  clouds: [
    {
      keystoneHostname: "js2.jetstream-cloud.org",
      friendlyName: "Jetstream2",
      userAppProxy: [
        {
          region: "ASU",
          hostname: "proxy-js2-asu.exosphere.app",
        },
        {
          region: "Cornell",
          hostname: "proxy-js2-cornell.exosphere.app",
        },
        {
          region: "IU",
          hostname: "proxy-js2-iu.exosphere.app",
        },
        {
          region: "TACC",
          hostname: "proxy-js2-tacc.exosphere.app",
        },
        {
          region: "UH",
          hostname: "proxy-js2-uh.exosphere.app",
        },
        {
          region: null,
          hostname: "proxy-js2-iu.exosphere.app",
        },
      ],
      imageExcludeFilter: null,
      featuredImageNamePrefix: "Featured-",
      instanceTypes: [
        {
          friendlyName: "Ubuntu",
          description:
            "- Wide compatibility with community software packages\n\n- Good choice for new users",
          logo: "assets/img/ubuntu.svg",
          versions: [
            {
              friendlyName: "24.04 (no software collection)",
              isPrimary: false,
              imageFilters: {
                name: "Preview-Ubuntu24",
                visibility: "public",
              },
              restrictFlavorIds: null,
            },
            {
              friendlyName: "24.04 (latest)",
              isPrimary: true,
              imageFilters: {
                name: "Featured-Ubuntu24",
                visibility: "public",
              },
              restrictFlavorIds: null,
            },
            {
              friendlyName: "22.04",
              isPrimary: true,
              imageFilters: {
                name: "Featured-Ubuntu22",
                visibility: "public",
              },
              restrictFlavorIds: null,
            },
            {
              friendlyName: "20.04 (no GPU support)",
              isPrimary: false,
              imageFilters: {
                name: "Featured-Ubuntu20",
                visibility: "public",
              },
              restrictFlavorIds: [
                "1",
                "14",
                "15",
                "2",
                "3",
                "3001",
                "3002",
                "3003",
                "3004",
                "3005",
                "4",
                "5",
                "7",
                "8",
                "9",
              ],
            },
          ],
        },
        {
          friendlyName: "Red Hat-like",
          description:
            "- Based on Red Hat Enterprise Linux (RHEL)\n\n- Compatible with RPM-based software",
          logo: "assets/img/hat-fedora.svg",
          versions: [
            {
              friendlyName: "Rocky Linux 9",
              isPrimary: true,
              imageFilters: {
                name: "Featured-RockyLinux9",
                visibility: "public",
              },
              restrictFlavorIds: null,
            },
            {
              friendlyName: "Rocky Linux 8",
              isPrimary: false,
              imageFilters: {
                name: "Featured-RockyLinux8",
                visibility: "public",
              },
              restrictFlavorIds: null,
            },
          ],
        },
      ],
      flavorGroups: [
        {
          matchOn: "m3..*",
          title: "General-purpose",
          description: null,
          disallowedActions: [],
        },
        {
          matchOn: "r3..*",
          title: "Large-memory",
          description: "These have lots of RAM.",
          disallowedActions: ["Suspend"],
        },
        {
          matchOn: "g3..*",
          title: "GPU",
          description: "These have a graphics processing unit.",
          disallowedActions: ["Suspend"],
        },
        {
          matchOn: "p3..*",
          title: "Private",
          description: "Special-purpose private flavors.",
          disallowedActions: [],
        },
      ],
      desktopMessage: "",
      securityGroups: {
        TACC: {
          name: "tacc-default",
          description: "allow ICMP, SSH, and Guacamole",
          rules: [
            {
              description: null,
              direction: "egress",
              ethertype: "IPv4",
              port_range_max: null,
              port_range_min: null,
              protocol: null,
              remote_group_id: null,
              remote_ip_prefix: null,
            },
            {
              description: null,
              direction: "egress",
              ethertype: "IPv6",
              port_range_max: null,
              port_range_min: null,
              protocol: null,
              remote_group_id: null,
              remote_ip_prefix: null,
            },
            {
              description: "Ping",
              direction: "ingress",
              ethertype: "IPv4",
              port_range_max: null,
              port_range_min: null,
              protocol: "icmp",
              remote_group_id: null,
              remote_ip_prefix: null,
            },
            {
              description: "SSH",
              direction: "ingress",
              ethertype: "IPv4",
              port_range_max: 22,
              port_range_min: 22,
              protocol: "tcp",
              remote_group_id: null,
              remote_ip_prefix: null,
            },
            {
              description: "Guacamole",
              direction: "ingress",
              ethertype: "IPv4",
              port_range_max: 49528,
              port_range_min: 49528,
              protocol: "tcp",
              remote_group_id: null,
              remote_ip_prefix: null,
            },
          ],
        },
      },
    },
    {
      keystoneHostname: "keystone.rc.nectar.org.au",
      friendlyName: "Nectar Cloud",
      userAppProxy: null,
      imageExcludeFilter: null,
      featuredImageNamePrefix: null,
      instanceTypes: [],
      flavorGroups: [],
      desktopMessage: null,
    },
    {
      keystoneHostname: "rci.uits.iu.edu",
      friendlyName: "Rescloud",
      userAppProxy: [
        {
          region: "RegionOne",
          hostname: "proxy-rescloud.exosphere.app",
        },
      ],
      imageExcludeFilter: null,
      featuredImageNamePrefix: null,
      instanceTypes: [],
      flavorGroups: [],
      desktopMessage: null,
    },
  ],
};
