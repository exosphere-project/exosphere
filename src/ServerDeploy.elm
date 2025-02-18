module ServerDeploy exposing (cloudInitUserDataTemplate)

import Helpers.Multipart as Multipart


exosphereBootHook : String
exosphereBootHook =
    """#!/bin/bash

# Ensure this hook only runs on the first boot
cloud-init-per once exosphere-setup-starting /bin/true || exit 0

echo on > /proc/sys/kernel/printk_devkmsg || true  # Disable console rate limiting for distros that use kmsg
sleep 1  # Ensures that console log output from any previous command completes before the following command begins
echo '{"status":"starting", "epoch": '$(date '+%s')'000}' | tee --append /dev/console > /dev/kmsg || true
chmod 640 /var/log/cloud-init-output.log
"""


exosphereSetupRunning : String
exosphereSetupRunning =
    """#!/bin/bash

sleep 1  # Ensures that console log output from any previous command completes before the following command begins
echo '{"status":"running", "epoch": '$(date '+%s')'000}' | tee --append /dev/console > /dev/kmsg || true
"""


exosphereAnsibleSetup : String
exosphereAnsibleSetup =
    """#!/bin/bash
set +e

retry() {
  local max_attempt=3
  local attempt=0
  while [ $attempt -lt $max_attempt ]; do
    if "$@"; then
      return 0
    fi
    echo "Command failed: $@"
    attempt=$((attempt + 1))
    if [ $attempt -lt $max_attempt ]; then
      sleep 5
    fi
  done
  echo "All retries of command failed: $@"
  return 1
}

{create-cluster-command}
(which apt-get && retry apt-get install -y python3-venv) # Install python3-venv on Debian-based platforms
(which yum     && retry yum install -y python3)      # Install python3 on RHEL-based platforms
python3 -m venv /opt/ansible-venv
. /opt/ansible-venv/bin/activate
retry pip install --upgrade pip
retry pip install ansible-core passlib
retry git clone \\
  --depth=1 \\
  --branch="{instance-config-mgt-repo-checkout}" \\
  "{instance-config-mgt-repo-url}" \\
  /opt/instance-config-mgt
ansible-playbook \\
  -i /opt/instance-config-mgt/ansible/hosts \\
  -e "{ansible-extra-vars}" \\
  /opt/instance-config-mgt/ansible/playbook.yml
ANSIBLE_RETURN_CODE=$?
if [ $ANSIBLE_RETURN_CODE -eq 0 ]; then STATUS="complete"; else STATUS="error"; fi
sleep 1  # Ensures that console log output from any previous commands complete before the following command begins
echo '{"status":"'$STATUS'", "epoch": '$(date '+%s')'000}' | tee --append /dev/console > /dev/kmsg || true
"""


exosphereCloudConfig : String
exosphereCloudConfig =
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
"""


cloudInitUserDataTemplate : String
cloudInitUserDataTemplate =
    Multipart.mixed (Multipart.boundary "===============exosphere-user-data==")
        |> Multipart.addAttachment "text/cloud-boothook" "00-exosphere-boothook.sh" [] exosphereBootHook
        |> Multipart.addAttachment "text/x-shellscript" "00-exosphere-setup-running.sh" [] exosphereSetupRunning
        |> Multipart.addAttachment "text/x-shellscript" "90-exosphere-ansible-setup.sh" [] exosphereAnsibleSetup
        |> Multipart.addAttachment "text/cloud-config" "exosphere.yml" [] exosphereCloudConfig
        |> Multipart.string
        -- Strip carriage returns, cloud-init doesn't require them when parsing
        -- but trailing carriage returns sometimes get left on shellscripts
        |> String.replace "\u{000D}" ""
