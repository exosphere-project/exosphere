# Exosphere: the User-Friendliest Interface for Non-proprietary Cloud Infrastructure

- Empowers researchers and other non-IT professionals to deploy their code and run services on [OpenStack](https://www.openstack.org)-based cloud infrastructure without advanced knowledge of virtualization or networking concepts
- Fills the gap between interfaces built for system administrators like OpenStack Horizon, and intuitive-but-proprietary services like DigitalOcean
- Enables cloud operators to deliver a user-friendly, powerful interface to their community with customized branding, nomenclature, and single sign-on integration

[![screenshot of Exosphere](docs/assets/screenshot-for-readme.png)](docs/assets/screenshot-for-readme.png)

## Video presentation and demo

[![Exosphere: A researcher-friendly GUI for OpenStack](https://img.youtube.com/vi/CTL-6icekYQ/0.jpg)](https://www.youtube.com/watch?v=CTL-6icekYQ)

## Quick Start

- **[try.exosphere.app](https://try.exosphere.app)** in your browser, if you have access to an existing OpenStack cloud with internet-facing APIs
- Use **[jetstream2.exosphere.app](https://jetstream2.exosphere.app)** if you have an allocation on [Jetstream2](https://jetstream-cloud.org/)

## Overview and Features

_Wait, what is OpenStack?_ OpenStack is the operating system and APIs that power public research clouds at [Jetstream](https://jetstream-cloud.org) and [CyVerse](https://cyverse.org),  private clouds at organizations like [Wikimedia](https://www.mediawiki.org/wiki/Wikimedia_Cloud_Services_team) and [CERN](https://clouddocs.web.cern.ch/), and public commercial clouds like [OVH](https://us.ovhcloud.com/public-cloud/), [Fuga](https://fuga.cloud/), and [Vexxhost](https://vexxhost.com/). You can also run OpenStack on your own hardware to provide cloud infrastructure-as-a-service for your organization!

_OK, what can I do with Exosphere?_

- Easily create instances to run your code, and volumes to manage your data
  - Works great for containers, intensive compute jobs, disposable experiments, and persistent web services
- Get **one-click, browser-based shell** access to cloud resources with Exosphere's [Apache Guacamole](http://guacamole.apache.org) integration
  - One-click **graphical desktop** as well! (for Ubuntu 20.04 and newer, Rocky Linux, and AlmaLinux)
- **Pretty graphs** show resource utilization of each instance at a glance
- If you're a cloud operator, deliver a customized interface with white-labeling, localized nomenclature, and single sign-on
- 100% self-hostable, 99% standalone client application
  - Two small proxy servers facilitate secure web browser connections to OpenStack APIs and services running on user-launched cloud instances
- On the roadmap:
  - First-class support for containers and data science workbenches
  - Cluster orchestration
  - Community-curated deployment automations for scientific workflows and custom services
- Fully open source and open development process -- come hack with us!
  - See Exosphere's [values and goals](values-goals.md)
  
Exosphere will be a primary user interface for [Jetstream 2](https://itnews.iu.edu/articles/2020/NSF-awards-IU-10M-to-build-Jetstream-2-cloud-computing-system-.php), an [NSF](https://www.nsf.gov)-funded science and engineering cloud. Jetstream 2 will be available to any US-based researcher starting late 2021.

## Collaborate With Us

To start a conversation or ask for help, talk to us in real-time on [Matrix / Element](https://riot.im/app/#/room/#exosphere:matrix.org). You can also [browse an archive](https://view.matrix.org/room/!qALrQaRCgWgkQcBoKG:matrix.org/) of the chat history.

We use GitLab to track issues and contributions. To request a new feature or report a bug, [create a new issue](https://gitlab.com/exosphere/exosphere/-/issues/new) on our GitLab project.


We discuss project progress and priorities on a **weekly video call** Mondays at 16:00 UTC. The whole community is invited! Join at <https://meet.jit.si/exosphere>, or dial in at +1.512.647.1431, PIN: 3037 7824 88#. Find the agenda and notes from previous meetings [here](https://c-mart.sandcats.io/shared/wfRsWBVmJZ3maUn7HMFqNj_MR_Bzy1vob9CzWu1n7QI).

## Documentation

### For Users and Anyone Else

- [Running Exosphere](docs/run-exosphere.md) yourself (instead of using one of the hosted sites)
- [Nomenclature Reference](docs/nomenclature-reference.md)
- [Values and Goals of the Exosphere Project](values-goals.md)
- [Cockpit Deprecation and Manual Installation](docs/cockpit.md)
- [Installing Exosphere Progressive Web Application](docs/pwa-install.md)
- [Exosphere Compatibility](docs/compatibility.md) (with clouds and instance operating systems)

### For Cloud Operators

- [Configuration Options](docs/config-options.md)
- [Instance Setup Code](docs/instance-setup.md)
- [User Application Proxy (UAP)](docs/user-app-proxy.md)
- [Solving the CORS Problem (Cloud CORS Proxy)](docs/solving-cors-problem.md)
- [Configuring Instance Types](docs/instance-types.md)
- [Message for desktop environment users](docs/desktop-message.md)
- [Federated Login Support](docs/federated-login.md)

### For Exosphere Contributors

- [Contributing to Exosphere](contributing.md)
- [Tour of Exosphere Codebase](docs/code-tour.md)
- [UI, Layout, Style & Design System](docs/style.md)
- [Architecture Decision Records](docs/adr/README.md)
- [Exosphere Tests](tests/README.md)
- [Browser Integration Tests](integration-tests/README.md)

### Legal Docs

- [Exosphere Project License](LICENSE)
- [Acceptable Use Policy for Exosphere Hosted Sites](docs/acceptable-use-policy.md)
- [Privacy Policy for Exosphere Hosted Sites](docs/privacy-policy.md)
