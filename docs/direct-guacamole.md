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

A cloud can have both `directGuacamole` and `userAppProxy` set. Instances that can serve Guacamole themselves are reached directly; the proxy carries the rest.

## Which address Exosphere uses

For each instance, in order:

1. A globally routable IPv6 address from the instance's fixed IP addresses. Link-local (`fe80::/10`) and unique-local (`fc00::/7`) addresses do not count, because they are not reachable from a user's browser.
2. Otherwise the instance's floating IP address, if it has one.

IPv6 comes first because floating IP addresses are scarce. A user whose network speaks IPv6 can connect without spending one, which leaves more of them for the users who need them.

Direct mode never falls back to the user application proxy. The proxy still serves every instance on a cloud that has not turned direct mode on, and every instance launched before it was turned on, and those instances behave exactly as they did before. An instance in direct mode with no public address of its own has no way to serve Guacamole, and Exosphere says so instead of offering a dead button.

## Finding out whether the browser has IPv6

An instance can have a perfectly good IPv6 address that a particular user cannot reach, because their own network, office, or ISP is IPv4-only. Exosphere finds this out by making the request rather than by asking a third party.

The first Guacamole token request goes to the address chosen above. From there:

- The request succeeds over IPv6, so this browser has IPv6. Exosphere remembers that for the session.
- The request fails with a network error and the instance also has a floating IP address, so Exosphere immediately retries there. If that works, this browser has no IPv6, and from then on floating IP addresses come first for every instance in the session.
- The request fails with a network error and the instance has no floating IP address to fall back to. Exosphere keeps retrying on its usual cadence and counts the failures.

What the browser learns is not persisted. A user who is on an IPv4-only network today and an IPv6 one tomorrow gets the right answer each time.

Anything other than a network error, such as a 500 from Guacamole or a timeout, says nothing about addressing, so it never changes the address Exosphere uses.

## When the user needs a floating IP address

An instance whose only public address is IPv6, on a browser that cannot reach IPv6, cannot be connected to at all. After three failed attempts in a row, and only once the instance has finished its own setup, the Terminal and Desktop interactions show a warning rather than an error:

> Your network can't reach this instance over IPv6. Assign a floating IP address to connect over IPv4.

Next to it is a button that opens Exosphere's normal assignment flow for that instance. Once a floating IP address is assigned, Exosphere resolves to it on the next poll and the interactions become ready without the user doing anything else.

The wait for setup to finish matters: an instance that is still deploying is not serving Guacamole yet, and those failures must not be read as a browser without IPv6.

## Instances launched before you turned it on

Serving TLS is set up at launch time, by the `caddy` Ansible role. Exosphere records whether that happened in the instance's `exoGuac` server metadata, so flipping `directGuacamole` on does not strand instances that were launched without it. Those instances keep using the user application proxy. Turning the flag off likewise leaves already-launched instances alone, though they will then need a proxy to stay reachable.

## What the instance runs

The `caddy` role installs Caddy and points it at Guacamole on `127.0.0.1:49528`. Caddy requests a certificate for the instance's own address using the ACME `shortlived` profile, which is the only profile under which Let's Encrypt issues certificates for IP addresses. Those certificates last about six days, so a long-running instance renews often; an instance that loses outbound network access to Let's Encrypt for a week will stop serving a valid certificate.

Caddy also answers the CORS preflight for Guacamole's token endpoint and adds the response header for it, naming the Exosphere origin that launched the instance. That origin is passed in at launch as the `exo_origin` Ansible variable, so a given deployment allows only itself.

## Security groups

Direct mode needs inbound TCP 443 on the instance, for both IPv4 and IPv6. Exosphere's default security group rules include it. Certificate issuance uses the TLS-ALPN-01 challenge, which runs over port 443, so port 80 does not need to be open.

See [security groups](security-groups.md) for how Exosphere reconciles default rules against a project's existing groups.
