# Exosphere: the User-Friendliest Interface for Non-proprietary Cloud Infrastructure

- Empowers researchers and other non-IT professionals to deploy code and run services on [OpenStack](https://www.openstack.org)-based cloud systems, without advanced virtualization or networking knowledge
- Fills the gap between OpenStack interfaces built for system administrators (like [Horizon](https://docs.openstack.org/horizon/latest/)), and intuitive-but-proprietary services like [DigitalOcean](https://www.digitalocean.com/) and [Amazon Lightsail](https://aws.amazon.com/lightsail)
- Enables cloud operators to deliver a friendly, powerful interface to their community with customized branding, nomenclature, and single sign-on integration

[![screenshot of Exosphere](docs/assets/screenshot-for-readme.png)](docs/assets/screenshot-for-readme.png)

Watch a [video presentation and demo](https://www.youtube.com/watch?v=CTL-6icekYQ):

[![Exosphere: A researcher-friendly GUI for OpenStack](https://img.youtube.com/vi/CTL-6icekYQ/0.jpg)](https://www.youtube.com/watch?v=CTL-6icekYQ)

## Try Exosphere

- Visit **[try.exosphere.app](https://try.exosphere.app)** if you have access to an existing OpenStack cloud with internet-facing APIs
- Use **[jetstream2.exosphere.app](https://jetstream2.exosphere.app)** if you have an allocation on [Jetstream2](https://jetstream-cloud.org/)
- [Run Exosphere locally](docs/run-exosphere.md) if you don't want to use our hosted site

## Overview and Features

_Wait, what is OpenStack? [OpenStack](http://openstack.org) is the operating system and APIs that power public research clouds like [Jetstream2](https://jetstream-cloud.org) and [Chameleon](https://www.chameleoncloud.org), private clouds at organizations like [Wikimedia](https://www.mediawiki.org/wiki/Wikimedia_Cloud_Services_team) and [CERN](https://clouddocs.web.cern.ch/), and public commercial clouds like [OVH](https://us.ovhcloud.com/public-cloud/), [Fuga](https://fuga.cloud/), [Vexxhost](https://vexxhost.com/), and [Leafcloud](https://leaf.cloud). Any organization can run OpenStack on its own hardware to provide a cloud service for its own community, or for the world._

What can I do with Exosphere?

- Create instances to run your code, and volumes to manage your data
  - Works great for containers, intensive compute jobs, disposable experiments, and persistent web services
- Get **one-click, browser-based shell and graphical desktop** access to cloud instances (via [Apache Guacamole](http://guacamole.apache.org))
- **Pretty graphs** show an instance's resource usage at a glance
- If you're a cloud operator, deliver a customized interface with white-labeling, localization, and single sign-on
- 100% self-hostable, 99% standalone client application
  - Two small proxy servers secure web browser connections to OpenStack APIs, and interactive services running on cloud instances
- On the [roadmap](https://gitlab.com/exosphere/exosphere/-/issues?label_name[]=long-term+goal):
  - First-class support for [container](https://gitlab.com/exosphere/exosphere/-/issues/82) and [data science workbench](https://gitlab.com/exosphere/exosphere/-/issues/717) resources
  - [Cluster orchestration](https://gitlab.com/exosphere/exosphere/-/issues/317)
  - [Community-curated deployment automations](https://gitlab.com/exosphere/exosphere/-/issues/573) for scientific workflows and custom services
- Free, open source, and open development process -- come hack with us!
  - See Exosphere's [values and goals](docs/values-goals.md)

## Documentation Index

### For Users and Anyone Else

- [Running Exosphere](docs/run-exosphere.md) yourself (instead of using one of the hosted sites)
- [Exosphere Compatibility](docs/compatibility.md) (with clouds and instance operating systems)
- [Values and Goals of the Exosphere Project](docs/values-goals.md)
- [Nomenclature Reference](docs/nomenclature-reference.md)
- [Installing Exosphere Progressive Web Application](docs/pwa-install.md)
- [Cockpit Deprecation and Manual Installation](docs/cockpit.md)

### For Cloud Operators

- [Configuration Options](docs/config-options.md)
- [Instance Setup Code](docs/instance-setup.md)
- [Solving the CORS Problem (Cloud CORS Proxy)](docs/solving-cors-problem.md)
- [User Application Proxy (UAP)](docs/user-app-proxy.md)
- [Configuring Instance Types](docs/instance-types.md)
- [Message for desktop environment users](docs/desktop-message.md)
- [Federated Login Support](docs/federated-login.md)

### For Exosphere Contributors

- [Contributing to Exosphere](contributing.md) **(new contributors start here)**
- [Tour of Exosphere Codebase](docs/code-tour.md)
- [UI, Layout, Style & Design System](docs/style.md)
- [Architecture Decision Records](docs/adr/README.md)
- [Exosphere Tests](tests/README.md)
- [Browser Integration Tests](integration-tests/README.md)
- [MkDocs Site](docs/mkdocs-site.md)
- [Code of Conduct](docs/code-of-conduct.md)

### Legal Docs

- [Exosphere Project License](LICENSE)
- [Acceptable Use Policy for Exosphere Hosted Sites](docs/acceptable-use-policy.md)
- [Privacy Policy for Exosphere Hosted Sites](docs/privacy-policy.md)

## Collaborate With Us

Talk to us in real-time on [Matrix / Element](https://matrix.to/#/#exosphere:matrix.org). You can also [browse an archive](https://view.matrix.org/room/!qALrQaRCgWgkQcBoKG:matrix.org/) of the chat history.

We use GitLab to track issues and contributions. To request a new feature or report a bug, [create a new issue](https://gitlab.com/exosphere/exosphere/-/issues/new) on our GitLab project.

We have a **weekly community video call** Wednesdays at 15:30 UTC (**note the new day and time**). Join by clicking on the Jitsi widget in our [Element chat](https://matrix.to/#/#exosphere:matrix.org). ([agenda](https://c-mart.sandcats.io/shared/wfRsWBVmJZ3maUn7HMFqNj_MR_Bzy1vob9CzWu1n7QI), and [previous meeting notes](https://gitlab.com/exosphere/exosphere/-/wikis/Meetings/2023/Weekly-Community-Meetings-(2023)))

## Exosphere's Impact

- [Jetstream2](https://jetstream-cloud.org), a science and engineering research cloud, offers Exosphere as its primary user interface. Jetstream2 is available to any US-based researcher.
- Exosphere has [a $298k award](https://nsf.gov/awardsearch/showAward?AWD_ID=2229642) from the US National Science Foundation's [Pathways to Enable Open-Source Ecosystems](https://beta.nsf.gov/funding/opportunities/pathways-enable-open-source-ecosystems-pose). Read about it in [this news release](https://itnews.iu.edu/articles/2022/IU-wins-300K-NSF-award-to-build-an-open-source-ecosystem-around-heavily-used-cloud-tool.php).

---

This material is based upon work supported by the National Science Foundation under Grant No. [2229642](https://nsf.gov/awardsearch/showAward?AWD_ID=2229642). Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.
