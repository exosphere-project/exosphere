# Direct Guacamole connections

## Overview

By default, the browser reaches Guacamole on an instance through a [User Application Proxy](user-app-proxy.md), which terminates TLS at the cloud and forwards to the instance. Direct mode skips the proxy: the instance serves Guacamole itself over HTTPS on port 443, using a Let's Encrypt certificate issued for its own IP address, and the browser connects to it.

Turn it on per cloud with the `directGuacamole` flag in `cloud_configs.js`:

```javascript
{
  "keystoneHostname": "openstack.example.cloud",
  "friendlyName": "My Example Cloud",
  "directGuacamole": true,
  ...
}
```

A cloud can have both `directGuacamole` and `userAppProxy` set. Direct mode is preferred where it applies, and the proxy stays as the fallback.

## Which address Exosphere uses

For each instance, in order:

1. The instance's floating IP address, if it has one.
2. Otherwise a globally routable IPv6 address from the instance's fixed IP addresses. Link-local (`fe80::/10`) and unique-local (`fc00::/7`) addresses do not count, because they are not reachable from a user's browser.
3. Otherwise the user application proxy, if the cloud has one.

Not every project has IPv6, and not every instance gets a floating IP, so the last step matters. An instance with no public address of either kind on a cloud with no proxy has no way to serve Guacamole, and Exosphere says so instead of offering a dead button.

## Instances launched before you turned it on

Serving TLS is set up at launch time, by the `caddy` Ansible role. Exosphere records whether that happened in the instance's `exoGuac` server metadata, so flipping `directGuacamole` on does not strand instances that were launched without it. Those instances keep using the user application proxy. Turning the flag off likewise leaves already-launched instances alone, though they will then need a proxy to stay reachable.

## What the instance runs

The `caddy` role installs Caddy and points it at Guacamole on `127.0.0.1:49528`. Caddy requests a certificate for the instance's own address using the ACME `shortlived` profile, which is the only profile under which Let's Encrypt issues certificates for IP addresses. Those certificates last about six days, so a long-running instance renews often; an instance that loses outbound network access to Let's Encrypt for a week will stop serving a valid certificate.

Caddy also answers the CORS preflight for Guacamole's token endpoint and adds the response header for it, naming the Exosphere origin that launched the instance. That origin is passed in at launch as the `exo_origin` Ansible variable, so a given deployment allows only itself.

## Security groups

Direct mode needs inbound TCP 443 on the instance, for both IPv4 and IPv6. Exosphere's default security group rules include it. Certificate issuance uses the TLS-ALPN-01 challenge, which runs over port 443, so port 80 does not need to be open.

See [security groups](security-groups.md) for how Exosphere reconciles default rules against a project's existing groups.
