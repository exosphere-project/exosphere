# Direct Guacamole connections

## Overview

Exosphere deploys Guacamole onto an instance so that the user gets a shell and a desktop in the browser. The instance serves it itself over HTTPS on port 443, using a Let's Encrypt certificate issued for its own IP address, and the browser connects straight to it. There is nothing to configure and no per-cloud switch.

Instances launched by an older Exosphere were not set up to do that, and are still reached through a [User Application Proxy](user-app-proxy.md), which terminates TLS at the cloud and forwards to the instance. A cloud that carries such instances still needs its `userAppProxy` configuration.

## Which address Exosphere uses

For each instance, in order:

1. A globally routable IPv6 address from the instance's fixed IP addresses. Link-local (`fe80::/10`) and unique-local (`fc00::/7`) addresses do not count, because they are not reachable from a user's browser.
2. Otherwise the instance's floating IP address, if it has one.

IPv6 comes first because floating IP addresses are scarce. A user whose network speaks IPv6 can connect without spending one, which leaves more of them for the users who need them.

The `Automatic` floating IP option on the launch form follows the same reasoning: an instance that has a globally routable IPv6 address and is serving Guacamole itself does not get a floating IP address at all. A class of a hundred students can therefore share far fewer than a hundred of them.

Direct mode never falls back to the user application proxy. The proxy still serves every instance launched before Exosphere deployed Guacamole this way, and those instances behave exactly as they did before.

## Finding out whether the browser has IPv6

An instance can have a perfectly good IPv6 address that a particular user cannot reach, because their own network, office, or ISP is IPv4-only. Exosphere finds this out by making the request rather than by asking a third party.

The first Guacamole token request goes to the address chosen above. From there:

- The request succeeds over IPv6, so this browser has IPv6. Exosphere remembers that for the session and does not change its mind, including on an instance that fails later: at that point the instance is the likelier problem.
- The request fails with a network error and the instance also has a floating IP address, so Exosphere immediately retries there. If that works, the most likely explanation is that this browser has no IPv6, so Exosphere assumes that until something answers over IPv6.
- The request fails with a network error, the instance has no floating IP address to fall back to, and the instance has finished its own setup. Again the most likely explanation is a browser without IPv6, and Exosphere assumes that until something answers over IPv6.

Either of the last two settles it for the whole session at once, so the user is told about every IPv6-only instance rather than one at a time.

This is a guess, not a measurement. A browser reports a refused TLS handshake the same way it reports an address it cannot open, so an instance whose certificate has not been issued yet can look like a browser without IPv6. The first token that arrives over IPv6 corrects it for the rest of the session.

What the browser learns is not persisted. A user who is on an IPv4-only network today and an IPv6 one tomorrow gets the right answer each time.

Anything other than a network error, such as a 500 from Guacamole or a timeout, says nothing about addressing, so it never changes the address Exosphere uses.

## When the user needs a floating IP address

An instance whose only public address is IPv6, on a browser that cannot reach IPv6, cannot be connected to at all. The Terminal and Desktop interactions show a warning rather than an error:

> Your network can't reach this instance over IPv6. Assign a floating IP address to connect over IPv4.

Next to it is a button that opens Exosphere's normal assignment flow for that instance. The instance picks the new address up within a minute and starts serving on it, and Exosphere resolves to it on the next poll, so the interactions become ready without the user doing anything else.

The instance's own setup has to have finished before any of this. An instance that is still deploying is not serving Guacamole yet, and those failures must not be read as a browser without IPv6.

## What the instance runs

The `caddy` Ansible role installs Caddy and points it at Guacamole on `127.0.0.1:49528`. Caddy requests a certificate for the instance's own address using the ACME `shortlived` profile, which is the only profile under which Let's Encrypt issues certificates for IP addresses. Those certificates last about six days, so a long-running instance renews often; an instance that loses outbound network access to Let's Encrypt for a week will stop serving a valid certificate.

On Rocky Linux the role relabels the Caddy binary so that SELinux runs it unconfined, as it is on Ubuntu. The targeted policy would otherwise confine it as `httpd_t`, which denies it reading the connections it accepts and its own admin socket, so it would serve nothing and could not reload.

Which addresses the instance answers on is not fixed. A floating IP address can be attached or detached at any time, and it is not on any local interface, so it has to come from the OpenStack metadata service. The role installs `/usr/local/sbin/exosphere-caddy-addresses` and a systemd timer that runs it every minute. The script writes a Caddyfile with one site per address the instance currently has, validates it, and reloads Caddy only if the content changed. An instance with no public address yet gets a Caddyfile with no sites, and Caddy waits.

Caddy also answers the CORS preflight for Guacamole's token endpoint and adds the response header for it, naming the Exosphere origin that launched the instance. That origin is passed in at launch as the `exo_origin` Ansible variable, so a given deployment allows only itself.

Exosphere records in the instance's `exoGuac` server metadata that this happened, which is how instances launched before it are told apart and kept on the user application proxy.

## Security groups

Instances need inbound TCP 443, for both IPv4 and IPv6. Exosphere's default security group rules include it. Certificate issuance uses the TLS-ALPN-01 challenge, which runs over port 443, so port 80 does not need to be open.

See [security groups](security-groups.md) for how Exosphere reconciles default rules against a project's existing groups.
