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
  - git{write-files}
runcmd:
  - echo on > /proc/sys/kernel/printk_devkmsg || true  # Disable console rate limiting for distros that use kmsg
  - sleep 1  # Ensures that console log output from any previous command completes before the following command begins
  - >-
    echo '{"status":"running", "epoch": '$(date '+%s')'000}' | tee --append /dev/console > /dev/kmsg || true
  - chmod 640 /var/log/cloud-init-output.log
  - {create-cluster-command}
  - (which apt-get && apt-get install -y python3-venv) # Install python3-venv on Debian-based platforms
  - (which yum     && yum     install -y python3)      # Install python3 on RHEL-based platforms
  - |-
    python3 -m venv /opt/ansible-venv
    . /opt/ansible-venv/bin/activate
    pip install --upgrade pip
    pip install ansible-core
    ansible-pull \\
      --url "{instance-config-mgt-repo-url}" \\
      --checkout "{instance-config-mgt-repo-checkout}" \\
      --directory /opt/instance-config-mgt \\
      -i /opt/instance-config-mgt/ansible/hosts \\
      -e "{ansible-extra-vars}" \\
      /opt/instance-config-mgt/ansible/playbook.yml
  - ANSIBLE_RETURN_CODE=$?
  - if [ $ANSIBLE_RETURN_CODE -eq 0 ]; then STATUS="complete"; else STATUS="error"; fi
  - sleep 1  # Ensures that console log output from any previous commands complete before the following command begins
  - >-
    echo '{"status":"'$STATUS'", "epoch": '$(date '+%s')'000}' | tee --append /dev/console > /dev/kmsg || true
"""
