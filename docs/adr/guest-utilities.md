# ADR 6: Exosphere Guest Utilities service

## Status

Proposed

## Context

As Exosphere grows, a growing number of potential features are limited by the lack of a bi-directional communication channel with instances. Extracting data from the instance console log is functional for reading status back from instances, however we need ways to issue commands to running instances. 

Common operations like unmounting volumes and managing network filesystems are currently not an example of "user-friendliest" design.

More discussion can be found at [Issue 861 - Bi-directional communication between Exosphere and instances](https://gitlab.com/exosphere/exosphere/-/issues/861)

In many ways, we need a similar utility to that provided by virtualization guest utilities. As an example, the [VirtualBox Guest Additions](https://wiki.osdev.org/VirtualBox_Guest_Additions) allow Host-Guest communication through a specialized virtual PCI device. 

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

### Authentication

The exouser passphrase could be saved to a local configuration, hashed similar to guacamole and used for session authentication.

Alternately (or in addition), temporary tokens could be emitted to the console log, caught by Exosphere, and sent back to the guest utilities server. 

### Commands

Commands to be used on the machine for basic control of the daemon. Assuming we use python

- `guest_utils install`  
  Create and enable systemd services to auto start the daemon

- `guest_utils uninstall`  
  Stop and remove installed systemd services

- `guest_utils [command]`  
  Local interface for interacting with the service

### RPC Methods

- `authenticate`  
  Using the passphrase (or other derived keys), authenticate the current session returning a session token.  
  Further HTTP requests would use HTTP Bearer Authentication, websockets would remain authenticated for the current socket lifetime

- `system_load.current`  
  Query the current system load metrics

- `system_load.historical`  
  Query historical system load metrics

Note: mounts and shares are separated, due to differences in mounting.

- `mounts.list`  
  List currently mounted devices

- `mounts.unmount`  
  Unmount a mounted device
  - Error when device is in use, return list of open inodes on the device

- `mounts.mount`  
  Mount a device to a mountpoint

- `shares.list`  
  List currently mounted (managed by guest utilities) shares

- `shares.mount`  
  Create and start relevant systemd.mount service

- `shares.unmount`  
  Stops and removes systemd.mount service
  - Error when share is in use, return list of open inodes on the device

- `self.check_updates`  
  Check pypi (or other used distribution method) for available updates

- `self.upgrade`
  Download the newest version of the guest utilities, and restart the daemon

The following methods will only be available to WebSocket connections

- `notifications.subscribe`
  Subscribe to named notifications

- `notifications.unsubscribe`
  Unsubscribe to named notifications

### RPC Notifications

All notifications will start with `notify.` for clarity. Many notifications seem superfluous, but will allow multiple instances of Exosphere to stay consistent when used to monitor for state changes.

- `notify.system_load`
  Periodic notifications of the current system load

- `notify.mounts.mount`
  Notifications when a new device is mounted

- `notify.shares.mount`
  Notifications when a share is mounted

- `notify.mounts.unmount`
  Notifications when a share is unmounted

## Decision and Consequences

- Create an `exosphere-guest-utilities` python package
  - Implement an HTTP / Websocket service, exposing a OpenRPC documented JSON-RPC 2.0 interface
    - [https://python-openrpc.burkard.cloud/](python-openrpc) is an easy toolkit for building the JSON-RPC router and exporting the schema
  - Extend the current `cloud-config` or `ansible` to deploy this to /opt/exosphere_guest_utils and enable the service
- Include a WebSockets elm interface for live streaming, I suggest [kageurufu/elm-websockets](https://github.com/kageurufu/elm-websockets)
- Implement JSON-RPC in Elm, some packages exist as a starting point, but have limitations that make them difficult to work with. [dwayne/elm-json-rpc](https://package.elm-lang.org/packages/dwayne/elm-json-rpc/latest/) is a great starting point for HTTP-only JSON-RPC, however it hides many types within private modules. Vendor the codebase to extend where we need to expose `toJson` functions for the private request types, and remove parts we don't need.

