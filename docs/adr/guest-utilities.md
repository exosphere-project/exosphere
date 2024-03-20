# ADR 6: Exosphere Guest Utilities service

## Status

Proposed

## Context

As Exosphere grows, a growing number of potential features are limited by the lack of a bi-directional communication channel with instances. Extracting data from the instance console log is functional for reading status back from instances, however we need ways to issue commands to running instances. 

Common operations like unmounting volumes and managing network filesystems are currently not an example of "user-friendliest" design.

More discussion can be found at [Issue 861 - Bi-directional communication between Exosphere and instances](https://gitlab.com/exosphere/exosphere/-/issues/861)

In many ways, we need a similar utility to that provided by virtualization guest utilities. As an example, the [VirtualBox Guest Additions](https://wiki.osdev.org/VirtualBox_Guest_Additions) allow Host-Guest communication through a specialized virtual PCI device. 

## Alternatives

* [Cockpit](https://cockpit-project.org/documentation.html)  
  Cockpit mainly exists to provide a web interface for server management. It has a websocket wire protocol, however this protocol is currently considered unstable. It is also very low level, and would require a lot of Elm and JavaScript to interface with. This code would also end up being Cockpit version dependent, and we would need to handle multiple Cockpit versions at once
* [rpcd](https://openwrt.org/docs/techref/rpcd)
  rpcd does provide a minimal HTTP interface, essentially acting like CGI-BIN. Plugins can be implemented as shell scripts or compiled `.so` libraries. We would need to fork `rpcd`, extend the `session` plugin to support authentication methods we need, and distribute binaries for all platforms we support. As well, we would maintain our collection of shell plugins which would provide methods such as `mount_share`
* [EnTrance](https://package.elm-lang.org/packages/ensoft/entrance/latest/)  
  An Elm+Python project from providing remote management of devices such as Cisco routers. This does provide some portions of what would be needed, however it is heavily focused on a single centralized management server that opens SSH, NetConf, or gRPC connections to multiple servers (routers) for remote management. The design notes also state "The application designs so far are suitable for apps that require no client authentication" and while authentication features are discussed in the design notes, they are prefaced with "the functionality in this section has not been implemented at the time of writing." (and have not since been implemented).
  Some of the Elm-side code could be useful inspiration for designing the client-side.
* Configuration Management Utilities (Ansible, Puppet, Salt, etc)  
  These services universally run on a client-server architecture, and we would need some way of deploying and maintaining the configuration server each instance would talk to. 

## Choices

The most straightforward implementation is an HTTP daemon running on every instance launched by Exosphere. This would allow us to provide Exosphere to Instance communication, and by adding a WebSocket we would upgrade to a full bi-directional communication channel. With the use of an RPC interface instead of a REST-ful design, we can effectively mirror the same API across HTTP and WebSockets.

[OpenRPC](https://open-rpc.org/) is a standard for designing and documenting [JSON-RPC 2.0](https://www.jsonrpc.org/specification) APIs.

### Development Language

Chosing an appropriate language is important. Existing developers on the Exosphere project will need familiarity to maintain and extend the codebase.

#### Python

Pros: 
* Already used within the Exosphere project
* Rapid development
* Easily deployed through `pip` or [`.pyz` (zip-apps)](https://docs.python.org/3/library/zipapp.html)

Cons:
* Memory usage. Python web servers commonly require well into the hundreds of megabytes of RAM
* Alma 8 currently ships Python 3.6.8, which has been deprecated by most modern Python frameworks

### Security

Adding an exposed service to instances is a security concern. To minimize this risk, the service will be installed and run as a non-root service user. `/opt/exoguest` will be created and ownership changed to the service user. `/opt/exoguest/data` will be used to store configuration files and generated systemd mount and services. 

[PolKit](https://en.wikipedia.org/wiki/Polkit) provides a secure method of privilege escalation. By adding a custom rule (written in JavaScript) to `/etc/polkit-1/rules.d/` we can control what the daemon is allowed to do. We currently expect to need permissions to create, start, and stop systemd services (we do not need write permissions to /etc/systemd for this, services can be linked from other paths). In the future this may be extended, for example granting access to the docker socket or talking to PackageKit for notifying about system package updates.

Authentication will be managed through [`pam`](https://en.wikipedia.org/wiki/Linux_PAM), using linux user credentials for access. Authentication with valid credentials will return a temporary access token ([JWT](https://jwt.io/)) which can be used to authorize further requests. In the future, SSH private key authentication or another form of private key authentication could be added.

### RPC Authentication

<details><summary>Authentication using HTTP POST</summary>

  ```
  POST /auth HTTP/1.1
  Authorization: Basic [base64 encoded "username:password"]

  HTTP/1.1 200 OK
  Content-Type: application/json
  {"username": "exouser", "token": "[jwt token]"}

  POST /rpc HTTP/1.1
  Authorization: Bearer [jwt token]
  Content-Type: application/json
  {"jsonrpc": "2.0", "id": 0, "method": "authenticate.me"}

  HTTP/1.1 200 OK
  Content-Type: application/json
  {"jsonrpc": "2.0", "id": 0, "method": "authenticate.me", "result": {
    "username": "exouser", 
    "schemes": {"apikey": ["rpc", "login"]
  }}}
  ```
</details>

<details><summary>Authentication using Batched RPC requests</summary>

Using JSON-RPC batched requests, we can issues a single HTTP POST to authenticate and execute methods that request authorization

#### Request
```json
[ { "jsonrpc": "2.0", "id": 0, "method": "authenticate.login", "params": {"username": "exouser", "password": "[passphrase]"} }
, { "jsonrpc": "2.0", "id": 1, "method": "mounts.list" }
]
```

#### Response
```json
[ { "jsonrpc": "2.0", "id": 0, "method": "authenticate.login", "result": {"username": "exouser", "schemes": ["rpc", "login"] } }
, { "jsonrpc": "2.0", "id": 1, "method": "mounts.list", "result": { ... } }
]
```
</details>

<details><summary>Authorization over RPC with an existing token</summary>

Using JSON-RPC batched requests, we can issues a single HTTP POST to authenticate and execute methods that request authorization

#### Request
```json
[ { "jsonrpc": "2.0", "id": 0, "method": "authenticate.token", "params": {"token": "[jwt token]"} }
, { "jsonrpc": "2.0", "id": 1, "method": "mounts.list" }
]
```

#### Response
```json
[ { "jsonrpc": "2.0", "id": 0, "method": "authenticate.token", "result": {"username": "exouser", "schemes": ["rpc", "login"] } }
, { "jsonrpc": "2.0", "id": 1, "method": "mounts.list", "result": { ... } }
]
```
</details>

### Commands

Commands to be used on the machine for basic control of the daemon. Assuming we use python

- `guest_utils install`  
  Create and enable systemd services to auto start the daemon

- `guest_utils uninstall`  
  Stop and remove installed systemd services

- `guest_utils [command]`  
  Local interface for interacting with the service

### RPC Methods

Note: this is a draft list, and final methods are to be determined. 

- `authenticate.login`  
  Using the passphrase, authenticate the current session returning a session token.  
  Further HTTP requests would use HTTP Bearer Authentication, websockets would remain authenticated for the current socket lifetime

- `authenticate.token`  
  Authenticate the current session using an existing session token.

- `authenticate.me`  
  Return the current session's authentication.

- `system_load.current`  
  Query the current system load metrics

- `system_load.historical`  
  Query historical system load metrics

Note: mounts and shares are currently separated, due to the different requirements for mounting and unmounting.

- `mounts.list`  
  List currently mounted devices

- `mounts.unmount`  
  Unmount a mounted device. This will let us safely detach volumes
  - Error when device is in use, return list of open inodes on the device
  - May need special casing behavior for shares vs volumes.

- `mounts.mount_device`  
  Mount a device to a mountpoint.

- `mounts.mount_cephfs`  
  Mount a CephFS share, creating and starting the relevant systemd.mount and systemd.service

`self` methods exist to support version upgrades

- `self.check_updates`  
  Check pypi (or other used distribution method) for available updates

- `self.upgrade`
  Download the newest version of the guest utilities, and restart the daemon

### RPC Streaming Responses (Notifications)

When using a Websocket connection, we are able to stream notifications back to the client. This allows for realtime system load monitoring, status updates for mounted devices, and more.

The following methods can be used to control notifications for a session

- `notifications.list`  
  List known notifications
 
- `notifications.subscribe`  
  Subscribe to named notifications

- `notifications.unsubscribe`  
  Unsubscribe to named notifications

All such notifications will start with `notify.` for clarity. Many notifications seem superfluous, but will allow multiple instances of Exosphere to stay consistent when used to monitor for state changes.

- `notify.system_load`
  Periodic notifications of the current system load

- `notify.mounts.mount_device`
  Notifications when a new device is mounted

- `notify.mounts.mount_cephfs`
  Notifications when a new cephfs share is mounted

- `notify.mounts.unmount`
  Notifications when a device is unmounted

## Decision and Consequences

- Create an `exosphere-guest-utilities` python package
  - Implement an HTTP / Websocket service, exposing a OpenRPC documented JSON-RPC 2.0 interface
    - [https://python-openrpc.burkard.cloud/](python-openrpc) is an easy toolkit for building the JSON-RPC router and exporting the schema
  - Extend the current `cloud-config` or `ansible` to deploy this to /opt/exosphere_guest_utils and enable the service

## Elm Client implementation

The proposed structure is split into four components

1. Types
    
    A single module will be implemented containing record type definitions for request methods, parameters, and notifications. As Open-RPC schemas are implemented using [JSON-Schema](https://json-schema.org/) for parameters and results, there are tools available for parsing and generating elm code from json-schema types. 

    Leveraging [elm-codegen](https://github.com/mdgriffith/elm-codegen) is a strong option, and code could be re-purporsed from `elm-open-api-cli` (licensed MIT, so with attribution) to initially only handle the component schemas defined in the Open-RPC specification.

    The current draft implementation uses hand-written types, referencing the Open-RPC schema.

2. Requests 

    Individual requests can be sent over HTTP or Websockets, while requests may also be batched over HTTP. To properly support batches notifications and subscriptions we will need a generic `jsonRpcDecoder` that dispatches based on method. 
    
    [dwayne/elm-json-rpc](https://package.elm-lang.org/packages/dwayne/elm-json-rpc/latest/) is a great starting point for HTTP-only JSON-RPC, however it hides many types within private modules. Use this code as an example, or vendor the codebase to extend where we need to expose `toJson` functions for the private request types, and remove parts we don't need.
     
3. WebSockets
    
    Ports are needed for websockets, given native support was removed in Elm 0.19. The draft implementation uses [kageurufu/elm-websockets](https://github.com/kageurufu/elm-websockets) for a simple and user friendly API. [https://github.com/billstclair/elm-websocket-client](billstclair/elm-websocket-client) is a more full-featured but less user-friendly option. 
