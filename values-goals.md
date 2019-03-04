## Goals/Values of Exosphere Project

### What

- We do
  - Reduce incidental complexity (cognitive load) of managing cloud compute workloads
  - Support open infrastructure ecosystems (e.g. OpenStack)
  - Give people useful tools, rather than exploit them or tie them to a platform
- We are NOT doing
  - Building a capital-P Platform
  - Adding core features which enable vendor lock-in


### Who we're doing it for

- Users (currently mostly researchers who need cloud computing)
- Cloud Operators
- Developers


### How we're doing it

- User-, operator-, developer-friendly
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

### What we're definitely doing

(benefits U for users, O for operators, D for developers)

- [U] Build an app that delivers the most user-friendly way to interact with non-proprietary cloud infrastructure
- [UO] Provide a consistent UX across infrastructures operated by different groups
- [UOD] Client is really easy to consume and offer
- [U] Build a client that can be pointed at infrastructure operated by anyone
  - (as opposed to building a platform or walled garden)
- [U] Empower people to use Exosphere along with other tools to manage the same resources, with minimal cost of switching
  - i.e. make it easy to move away from Exosphere to other tools and come back at will
- [UO] Supporting those who value security
  - No backdoor SSH key needed on instances
  - No need for app to have god-mode cloud admin access
  - Small, well-defined set of dependencies and "moving parts"
  - CI+CD scans app dependencies and alerts on known vulnerabilities
- [UOD] Stability
  - Reliable instance provisioning minimizes user frustration and operator support burden
  - No runtime exceptions in client
  - No custom backend (for core functionality) where things break between client and infrastructure
- [D] Velocity/pleasure of Development
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
  - User auth (OpenStack handles this for us)
  - User/resource pools (currently using OpenStack projects)


### What we might do

#### Driven by community needs

- [U] Automated deployment of custom services/apps (e.g. scientific workloads) on user's cloud resources
  - Automated deployment of parallel data processing workflows
- [U] Support for modern / "cloud-native" workloads
  - [U] Cluster orchestration
  - [U] Support for container-based and notebook-based workloads as first-class citizens
    - In progress - proofs of concept exist
- [U] Support for graphical workloads
  - "I want a GUI" checkbox deploys a GUI desktop at instance launch time
  - 3D-accelerated remote graphical session (perhaps TurboVNC)
- [U] Polish the UX
  - Make OpenStack's error messages more user-friendly
  - Dashboard with overview of resources for all providers
- [UO] Display provider quotas
- [UO] Extension points (for plugins and 'no-recompile' customization - e.g. themes, internationalization)


#### Contingent on financial support (or especially strong community demand)

- Optional modules designed to interact with institution-specific services which enhance the user experience for  users of cloud services provided by these institutions. Examples:
    - [OU] Institutional Single Sign-on with OpenStack credential management/leases
    - [OU] Allocation service
    - [O] Reporting tools
    - [O] Reverse proxy server 
- [U] Data management tools
- [U] Sync user settings between devices
- [UO] Support for cloud providers other than OpenStack
    - Either other open cloud computing APIs or to help users control/limit their spending on commercial clouds
