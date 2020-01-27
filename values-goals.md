## Goals/Values of Exosphere Project

### What

- We do
  - Reduce incidental complexity (cognitive load) of managing cloud compute workloads
  - Support non-proprietary infrastructure ecosystems (e.g. OpenStack)
  - Give people useful tools, rather than exploit them or tie them to a platform
- We are NOT doing
  - Building a capital-P Platform
  - Adding core features which enable vendor lock-in


### Who we're doing it for

- Users
  - People who need a friendly way to manage:
    - Big/fast computers
    - Computers that are on and available 24/7
    - Disposable computers to try "this one thing"
    - Containers, functions, or data (independently of "computers")
  - In order to:
    - Store, analyze, visualize, curate, and share their data
    - Develop and deploy software and services
    - ??? Surprise us!
- Cloud Operators
- Exosphere Contributors
  - Exosphere Developers
  - Those in the community who interact with the project

"Markets" served:
- Researchers who have access to institutional cloud resources (e.g. [Jetstream Cloud](https://jetstream-cloud.org))
- Users and operators of OpenStack public/private cloud services


### How we're doing it

- User-, operator-, contributor-friendly
  - Build things that others find rewarding to consume, troubleshoot, repair, and extend
  - [D] Keeping the internal complexity of project as low as possible, thus making it easier for contributors to participate and reducing the scope for error
  - Developer tooling that is rewarding to use
- Open, community-driven approach
  - Engineer a great contributor experience (especially new contributors)
  - Open source, permissively licensed
  - Open development process (issues, MRs, roadmap)
  - Sustainable project designed to outlast founding developers' involvement
- Stable core yet extensible
  - Only stable code enabled in default build
  - Experiments belong in extensions or features enabled via configuration


## Functional scope of Exosphere Project

### What we're already doing

(benefits U for users, O for operators, C for Exosphere contributors)

- [U] Build an app that delivers the most user-friendly way to interact with non-proprietary cloud infrastructure
- [UO] Provide a consistent UX across infrastructures operated by different groups
- [UOC] Client is really easy to consume and offer
- [U] Build a client that can be pointed at infrastructure operated by anyone
  - (as opposed to building a platform or walled garden)
- [U] Empower people to use Exosphere along with other tools to manage the same resources, with minimal cost of switching
  - i.e. make it easy to move away from Exosphere to other tools and come back at will
- [UO] Supporting those who value security
  - No backdoor SSH key needed on instances
  - No need for app to have god-mode cloud admin access
  - Small, well-defined set of dependencies and "moving parts"
  - CI+CD scans app dependencies and alerts on known vulnerabilities
- [UOC] Stability
  - Reliable instance provisioning minimizes user frustration and operator support burden
  - No runtime exceptions in client
  - No custom backend (for core functionality) where things break between client and infrastructure
- [C] Velocity/pleasure of Development
  - Extremely short tweak-test cycle for entire app (a couple of seconds)
  - Continuous deployment
    - Allows rapid iteration on user feedback, quick bug fixes, and cheap experimentation
  - Browser dev tools expose all API calls to OpenStack
  - Only one (small) programming language to learn to become productive
    - Same language for both UI and app logic
  - Compiler eliminates several classes of bugs
    - Delivers level of assurance that would require extremely diligent code testing effort in most other languages
    - Allows fearless refactoring (and accepting of community contributions)


### Non-features (definitely not on the roadmap)

- Requiring custom (backend) services to support core features
- Re-inventing that which is both time-consuming and not uniquely valuable, e.g.
  - User auth (OpenStack handles this for us, but see "Institutional Single Sign-on" below)
  - User/resource pools (currently using OpenStack projects)


### What we will do (driven by community needs & funding)

(* means contingent on funding)

- Automated deployment of custom services/apps (e.g. scientific workloads) on user's cloud resources
  - One-click installation of common tools like Jupyter Notebook, RStudio, et al. at instance launch time
  - Automated deployment of parallel data processing workflows
- Support for modern / "cloud-native" workloads
  - Cluster orchestration, possibly using OpenStack Heat / Magnum
  - Support for container-based and notebook-based workloads as first-class citizens
    - In progress - proofs of concept exist
- Support for graphical workloads
  - "I want a GUI" checkbox deploys a GUI desktop at instance launch time
  - 3D-accelerated remote graphical session (perhaps TurboVNC, or Xpra)
- Support for high-security workloads
  - Deploy bastion hosts, servers with no public IP addresses
- Polish the UX
  - Smoother client install/upgrade process, signed binaries
  - Dashboard with overview of resources for all providers
- Empower users to share cloud resources with collaborators
- Extension points (for plugins and 'no-recompile' customization - e.g. white-label / skinning, internationalization)
- \* Institution-specific integrations to enhance the UX and cloud operator effectiveness. Examples:
    - \* Institutional Single Sign-on with OpenStack credential management/leases
    - \* Allocation service and tools
    - \* Reporting tools for operators and project PIs
    - \* Support/ticketing systems (e.g. Intercom, Zendesk)
    - \* On-premises reverse proxy server (OpenStack services behind firewall)
- \* Data management tools for users
- \* Sync user settings between devices
- \* Support for cloud providers other than OpenStack
    - \* Either other open cloud computing APIs or to help users control/limit their spending on commercial clouds
- \* On-demand import of images from Atmosphere side of Jetstream Cloud
