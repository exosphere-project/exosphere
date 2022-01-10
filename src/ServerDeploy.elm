module ServerDeploy exposing (cloudInitUserDataTemplate)


cloudInitUserDataTemplate : String
cloudInitUserDataTemplate =
    {-
       The virtualenv case expression is due to CentOS 7 requiring use of `virtualenv-3`,
       Ubuntu 18 requiring `python3 -m virtualenv`, and everything else just using `virtualenv`.
    -}
    """#cloud-config
users:
  - default
  - name: exouser
    shell: /bin/bash
    groups: sudo, admin
    sudo: ['ALL=(ALL) NOPASSWD:ALL']{ssh-authorized-keys}
ssh_pwauth: true
package_update: true
package_upgrade: {install-os-updates}
packages:
  - python3-virtualenv
  - git
write_files:
- path: /root/openrc.sh
  content: |
    export OS_AUTH_TYPE=v3applicationcredential
    export OS_AUTH_URL={os-auth-url}
    export OS_IDENTITY_API_VERSION=3
    export OS_REGION_NAME="RegionOne"
    export OS_INTERFACE=public
    export OS_APPLICATION_CREDENTIAL_ID="{os-ac-id}"
    export OS_APPLICATION_CREDENTIAL_SECRET="{os-ac-secret}"
  owner: root:root
  permissions: '0600'

runcmd:
  - echo on > /proc/sys/kernel/printk_devkmsg || true  # Disable console rate limiting for distros that use kmsg
  - sleep 1  # Ensures that console log output from any previous command completes before the following command begins
  - echo '{"exoSetup":"running"}' | tee --append /dev/console > /dev/kmsg || true
  - chmod 640 /var/log/cloud-init-output.log
  - |
    (which virtualenv && virtualenv /opt/ansible-venv) || (which virtualenv-3 && virtualenv-3 /opt/ansible-venv) || python3 -m virtualenv /opt/ansible-venv
    . /opt/ansible-venv/bin/activate
    pip install ansible-core
    ansible-pull --url "{instance-config-mgt-repo-url}" --checkout "{instance-config-mgt-repo-checkout}" --directory /opt/instance-config-mgt -i /opt/instance-config-mgt/ansible/hosts -e "{ansible-extra-vars}" /opt/instance-config-mgt/ansible/playbook.yml
    pip install python-openstackclient
    cd /opt
    git clone --branch cluster-create-local --single-branch https://github.com/julianpistorius/CRI_Jetstream_Cluster.git
    cd CRI_Jetstream_Cluster
    ssh-keygen -q -N "" -f /root/.ssh/id_rsa
    ./cluster_create_local.sh -n "$(hostname --short)" -o /root/openrc.sh
  - sleep 1  # Ensures that console log output from previous command completes before the following command begins
  - echo '{"exoSetup":"complete"}' | tee --append /dev/console > /dev/kmsg || true
mount_default_fields: [None, None, "ext4", "user,exec,rw,auto,nofail,x-systemd.makefs,x-systemd.automount", "0", "2"]
mounts:
  - [ /dev/sdb, /media/volume/sdb ]
  - [ /dev/sdc, /media/volume/sdc ]
  - [ /dev/sdd, /media/volume/sdd ]
  - [ /dev/sde, /media/volume/sde ]
  - [ /dev/sdf, /media/volume/sdf ]
  - [ /dev/sdg, /media/volume/sdg ]
  - [ /dev/sdh, /media/volume/sdh ]
  - [ /dev/sdi, /media/volume/sdi ]
  - [ /dev/sdj, /media/volume/sdj ]
  - [ /dev/sdk, /media/volume/sdk ]
  - [ /dev/vdb, /media/volume/vdb ]
  - [ /dev/vdc, /media/volume/vdc ]
  - [ /dev/vdd, /media/volume/vdd ]
  - [ /dev/vde, /media/volume/vde ]
  - [ /dev/vdf, /media/volume/vdf ]
  - [ /dev/vdg, /media/volume/vdg ]
  - [ /dev/vdh, /media/volume/vdh ]
  - [ /dev/vdi, /media/volume/vdi ]
  - [ /dev/vdj, /media/volume/vdj ]
  - [ /dev/vdk, /media/volume/vdk ]
"""
