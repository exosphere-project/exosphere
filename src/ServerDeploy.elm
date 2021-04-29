module ServerDeploy exposing (cloudInitUserDataTemplate, desktopEnvironmentUserData, guacamoleUserData)


cloudInitUserDataTemplate : String
cloudInitUserDataTemplate =
    """#cloud-config
users:
  - default
  - name: exouser
    shell: /bin/bash
    groups: sudo, admin
    sudo: ['ALL=(ALL) NOPASSWD:ALL']{ssh-authorized-keys}
ssh_pwauth: true
yum_repos:
  epel-release:
    baseurl: https://download.fedoraproject.org/pub/epel/8/Everything/x86_64
    enabled: true
    failovermethod: priority
    gpgcheck: true
    gpgkey: https://download.fedoraproject.org/pub/epel/RPM-GPG-KEY-EPEL-8
    name: Extra Packages for Enterprise Linux 8 - Release
package_update: true
package_upgrade: {install-os-updates}
packages:
  - haveged
  - python3-virtualenv
runcmd:
  - sleep 1  # Ensures that console log output from any previous command completes before the following command begins
  - echo '{"exoSetup":"running"}' > /dev/console
  - |
    WORDS_URL=https://gitlab.com/exosphere/exosphere/snippets/1943838/raw
    WORDS_SHA512=a71dd2806263d6bce2b45775d80530a4187921a6d4d974d6502f02f6228612e685e2f6dcc1d7f53f5e2a260d0f8a14773458a1a6e7553430727a9b46d5d6e002
    wget --quiet --output-document=words $WORDS_URL
    if echo $WORDS_SHA512 words | sha512sum --check --quiet; then
      PASSPHRASE=$(cat words | shuf --random-source=/dev/urandom --head-count 11 | paste --delimiters=' ' --serial | head -c -1)
      POST_URL=http://169.254.169.254/openstack/latest/password
      if curl --fail --silent --request POST $POST_URL --data "$PASSPHRASE"; then
        echo exouser:$PASSPHRASE | chpasswd
      fi
    fi
  - "usermod -a -G centos exouser || usermod -a -G ubuntu exouser || true  # Using usermod because native cloud-init will create non-existent groups, and a centos/ubuntu group on Ubuntu/CentOS could be confusing"
  - "mkdir -p /media/volume"
  - "cd /media/volume; for x in b c d e f g h i j k; do mkdir -p sd$x; mkdir -p vd$x; done"
  - "systemctl daemon-reload"
  - ""
  - |
    for x in sdb sdc sdd sde sdf sdg sdh sdi sdj sdk vdb vdc vdd vde vdf vdg vdh vdi vdj vdk; do
      systemctl start media-volume-$x.automount;

      cat << EOF > /etc/systemd/system/exouser-owns-media-volume-$x.service
      [Unit]
      Description=ExouserOwnsVolume$x
      Requires=media-volume-$x.mount
      After=media-volume-$x.mount

      [Service]
      ExecStart=/bin/chown exouser:exouser /media/volume/$x

      [Install]
      WantedBy=media-volume-$x.mount
    EOF

      systemctl enable exouser-owns-media-volume-$x.service
    done
  - "chown exouser:exouser /media/volume/*"
  - |
    SYS_LOAD_SCRIPT_URL=https://gitlab.com/exosphere/exosphere/-/snippets/2015130/raw
    SYS_LOAD_SCRIPT_SHA512=0667348aeb268ac8e0b642b03c14a8f87ddd38e11a50243fe1ab6ee764ebd724949c5ec98f95c03aaa7c16c77652bc968cc7aba50f6b1038b1a20ceefc133a73
    SYS_LOAD_SCRIPT_FILE=/opt/system_load_json.py
    wget --quiet --output-document=$SYS_LOAD_SCRIPT_FILE $SYS_LOAD_SCRIPT_URL
    if echo $SYS_LOAD_SCRIPT_SHA512 $SYS_LOAD_SCRIPT_FILE | sha512sum --check --quiet; then
      chmod +x $SYS_LOAD_SCRIPT_FILE
      $SYS_LOAD_SCRIPT_FILE > /dev/console
      echo "* * * * * root $SYS_LOAD_SCRIPT_FILE > /dev/console" >> /etc/crontab
    fi
  - |
    {desktop-environment-setup}
  - |
    {guacamole-setup}
  - unset PASSPHRASE
  - "command -v apt-get && DEBIAN_FRONTEND=noninteractive apt-get install -yq haveged  # This is for stubborn Ubuntu 18"
  - sleep 1  # Ensures that console log output from previous command completes before the following command begins
  - echo '{"exoSetup":"complete"}' > /dev/console
mount_default_fields: [None, None, "ext4", "user,rw,auto,nofail,x-systemd.makefs,x-systemd.automount", "0", "2"]
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


guacamoleUserData : String
guacamoleUserData =
    """virtualenv /opt/ansible-venv
    . /opt/ansible-venv/bin/activate
    pip install ansible-base
    ansible-pull --url https://gitlab.com/exosphere/instance-config-mgt.git --checkout ab2e0fbbd0980777814b94dbf7c983fde425f877 --directory /opt/instance-config-mgt -i /opt/instance-config-mgt/ansible/hosts -e "exouser_password=\\"$PASSPHRASE\\"" -e "{ansible-extra-vars}" /opt/instance-config-mgt/ansible/playbook.yml
"""


desktopEnvironmentUserData : String
desktopEnvironmentUserData =
    """if grep --ignore-case --quiet "ubuntu" /etc/issue; then
      DEBIAN_FRONTEND=noninteractive apt-get install -yq ubuntu-desktop-minimal
    elif grep --ignore-case --quiet "centos" /etc/redhat-release; then
      yum -y groupinstall workstation
    fi

    systemctl enable graphical.target
    systemctl set-default graphical.target
    systemctl isolate graphical.target
"""
