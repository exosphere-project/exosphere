# ADR 2: Configuration Management for Instance Provisioning

## Status

Proposed

## Context

When an Exosphere user launches an instance (a.k.a. virtual computer or server), Exosphere does the following provisioning steps on the new instance, heretofore implemented in [`src/ServerDeploy.elm`](https://gitlab.com/exosphere/exosphere/-/blob/c583d345ce2d9f65ec5fbf65e148ad93104197ed/src/ServerDeploy.elm):

- Create `exouser` local user with optional user-specified authorized SSH public keys
- Run operating system package updates
- Install haveged to prevent entropy starvation
- Generate a passphrase, set it for `root` and `exouser`, and POST it to OpenStack metadata service
- Configure automatic mounting for any attached volumes
- Set up Python script and cron task to report system load to instance console log
- Optionally, install a graphical desktop environment
- Optionally (but enabled by default), set up Guacamole stack for one-click in-browser remote shell
- Report status of provisioning (in progress, complete) to instance console log

This provisioning automation is implemented as a combination of [cloud-init](https://cloud-init.io) and shell (bash) -- whatever cannot be accomplished with native cloud-init is written in bash. Some provisioning code exists in a [separate repository](https://gitlab.com/exosphere/guacamole-config) which is downloaded and run at server deploy time. This code works fairly reliably, but it has been somewhat fragile to build, and it is not the most readable.

---

Merge request [!415](https://gitlab.com/exosphere/exosphere/-/merge_requests/415) (solving issue #398) introduces a one-click, in-browser remote graphical desktop, served by Guacamole. This requires significant expansion of our provisioning code:

1. Configure desktop environment with automatic login; disable screen lock timeout
2. Install a VNC server; configure it with several files with correct directories, permissions, and SELinux context; and set up a systemd service
3. When setting up Guacamole, template out an appropriate configuration depending on whether the instance has a desktop environment

Most of these steps must be implemented differently for each distro that Exosphere supports (Ubuntu or CentOS). Package and config file names differ between distros. The TigerVNC package for CentOS includes the skeleton of a systemd service, while on Ubuntu you need to build your own unit file with the `vncserver` wrapper command. Also, all of these steps need to be idempotent, i.e., they need to do the right thing on an instance launched from an image which previously had these provisioning steps applied.

So, it feels like our provisioning steps at server deploy time are becoming sophisticated and complex enough to reach for a more powerful configuration management tool. These tools are built to do things like manage software packages, template out files, modify existing files, and configure the init system. They are designed to support idempotent changes and distro-specificity.

## Choices

### More Shell (Bash)

It is possible to accomplish all of the provisioning steps for [!415](https://gitlab.com/exosphere/exosphere/-/merge_requests/415) using bash. For example:

```
while fuser /var/lib/dpkg/lock /var/lib/apt/lists/lock /var/cache/apt/archives/lock > /dev/null 2>&1; do
  sleep 5
done
```

```
if ! grep -qF "AutomaticLogin = exouser" $GDM_CUSTOM_CONF_LOCATION; then
  sed -i "s/\[daemon\]/[daemon]\nAutomaticLoginEnable = true\nAutomaticLogin = exouser\n/" $GDM_CUSTOM_CONF_LOCATION
fi
```

- Good, because bash is baked into every instance, and no other tools need to be installed
- Good, because there are libraries such as [bash-booster](http://www.bashbooster.net/) which help solve common tasks like installing system packages and templating out a file with some variables. 
  - It is still not as feature-complete as Ansible, though.
- Bad, because bash can be fragile and writing it tends to be error-prone. Bash is stringly-typed, and character escaping rules have accreted over decades of backward-compatibility. 
- Bad, because bash can be difficult to read (see example above)

### [Ansible](https://docs.ansible.com/ansible/latest/index.html)

We would run Ansible locally on the new instance, perhaps downloading the code to run using `ansible-pull`. We would _not_ have a central server that tries to run the Ansible on the instances remotely via SSH. (We would not actually make any changes to the overall architecture of Exosphere at all. You'd still just have a static web app, 2 proxy servers, and a cloud.)

- Good, because Ansible is designed to support host provisioning in a way that is idempotent and distro-specific
- Good, because Exosphere and Jetstream Cloud team have experience with Ansible
- Good, because Ansible has solved many common issues which we would need to solve explicitly/manually in bash (for example, waiting for package database locking)
- Bad, because it needs to be installed on the instance; this increases the time to deploy an instance and also consumes space on disk.

#### Ansible Installation Options

`ansible-base`/`ansible-core` are the lightest-weight options:

```
|                                |    CentOS     |    Ubuntu     |
| Installation Option            | Time  | Disk  | Time  | Disk  |
|--------------------------------|-------|-------|-------|-------|
| package in distro default repo |   44s | 101MB |   37s |  90MB |
| ansible on PyPI, fresh venv    |  3m9s | 399MB | 4m56s | 438MB |
| ansible-base on PyPI           |   12s |  46MB |   10s |  43MB |
| ansible-core on PyPI           |   12s |  47MB |    9s |  44MB |
```

PyPI packages were each installed in a fresh virtual environment using this command:

```
time pip --no-cache-dir install package-name-here
```

### [Make](https://www.gnu.org/software/make/)

- Good, because supports incremental execution with targets (a form of idempotence)
- Bad, because it mostly still relies on shell commands. Does not include batteries for configuring a system.

### More Sophisticated Use of cloud-init

Unclear if this is even possible. Cloud-init isn't intended to be a full-featured configuration language -- just enough to bootstrap a cloud instance and run your own provisioning code. The cloud-init modules are not composable in arbitrary order of execution, and the way to run arbitrary code (`runcmd`) still relies on bash.


## Decision and Consequences

- Create a new `instance-provision` repository
- Write Ansible code to replace what is in the [guacamole-config](https://gitlab.com/exosphere/guacamole-config) repository; deprecate that repo
- Document that Exosphere uses Ansible to provision instances in `contributing.md` and any other appropriate spots
- If this works well, consider migrating some/most of the bash in [`src/ServerDeploy.elm`](https://gitlab.com/exosphere/exosphere/-/blob/c583d345ce2d9f65ec5fbf65e148ad93104197ed/src/ServerDeploy.elm) to Ansible code in the `instance-provision` repo