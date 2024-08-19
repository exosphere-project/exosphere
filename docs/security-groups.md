# Configuring Default Security Groups

When an instance is created, a default security group is attached to configure its network connection rules.

Exosphere's default security group is fairly permissive (e.g. it allows all outgoing TCP connections).

This might not be an appropriate default for all clouds or regions, so Exosphere enables cloud administrators to configure their own default security groups.

## securityGroups

The `securityGroups` JSON object maps a region key to a security group object.

The region key applies the security group default to the specified region only. Use the key `noRegion` as a catch-all or fallback configuration.

Each security group object has the following members:

- `name` (string) identifies the security group.

- `description` (null or string) is optional help text such explain what the group does e.g. "Allow all traffic".

- `rules` (array) is a list of rule objects conforming to a subset of OpenStack's Networking API [security group rules definition](https://docs.openstack.org/api-ref/network/v2/index.html#create-security-group-rule).

  - `ethertype` (string) the internet protocol version, either "IPv4" or "IPv6".
  - `direction` (string) the network traffic direction, either "ingress" or "egress".
  - `protocol `(null or string) the communication protocol e.g. "icmp", "tcp", "udp", etc.
  - `port_range_min` (null or number) the starting port, if any.
  - `port_range_max` (null or number) the ending port, if any.
  - `remote_ip_prefix` (null or string) a CIDR to whitelist; null or "0.0.0.0/0" for any remote IP.
  - `remote_group_id` (null or string) another security group ID to allow its members access to this group (can reference itself for a shared private network).
  - `description` (null or string) a description for this rule's purpose e.g. "SSH".

  > **Note:** Exosphere will try to maintain consistency between this configuration & the actual rules of the security group. If rules are added or removed manually, the application will delete ones not in the config & restore ones which are.

## Example securityGroups

```javascript
var config = {
  ...
  clouds: [
    {
      keystoneHostname: "iu.jetstream-cloud.org",
      ...  
      securityGroups: {
        noRegion: {
          name: "permissive",
          description: "Allow all traffic",
          rules: [
            {
              ethertype: "IPv4",
              direction: "ingress",
              protocol: "udp",
              port_range_min: 60000,
              port_range_max: 61000,
              remote_ip_prefix: "0.0.0.0/0",
              remote_group_id: null,
              description: "Mosh",
            },
            {
              ethertype: "IPv4",
              direction: "ingress",
              protocol: "tcp",
              port_range_min: 22,
              port_range_max: 22,
              remote_ip_prefix: null,
              remote_group_id: null,
              description: "SSH",
            },
            {
              ethertype: "IPv4",
              direction: "egress",
              protocol: null,
              port_range_min: null,
              port_range_max: null,
              remote_ip_prefix: null,
              remote_group_id: null,
              description: null,
            },
            {
              ethertype: "IPv6",
              direction: "egress",
              protocol: null,
              port_range_min: null,
              port_range_max: null,
              remote_ip_prefix: null,
              remote_group_id: null,
              description: null,
            },
            {
              ethertype: "IPv4",
              direction: "ingress",
              protocol: "icmp",
              port_range_min: null,
              port_range_max: null,
              remote_ip_prefix: null,
              remote_group_id: null,
              description: "Ping",
            },
            {
              ethertype: "IPv4",
              direction: "ingress",
              protocol: "tcp",
              port_range_min: null,
              port_range_max: null,
              remote_ip_prefix: null,
              remote_group_id: null,
              description: "Expose all incoming ports",
            },
          ],
        },
        IU: {
          name: "restrictive",
          description: "Only allow SSH",
          rules: [
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
          ],
        },
      },
    },
  ],
};
```
