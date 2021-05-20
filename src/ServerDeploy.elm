module ServerDeploy exposing (cloudInitUserDataTemplate)


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
package_update: true
package_upgrade: {install-os-updates}
packages:
  - python3-virtualenv
  - git
  - wget
runcmd:
  - sleep 1  # Ensures that console log output from any previous command completes before the following command begins
  - echo '{"exoSetup":"running"}' > /dev/console
  - |
    WORDS_URL=https://gitlab.com/exosphere/exosphere/snippets/1943838/raw
    WORDS_SHA512=a71dd2806263d6bce2b45775d80530a4187921a6d4d974d6502f02f6228612e685e2f6dcc1d7f53f5e2a260d0f8a14773458a1a6e7553430727a9b46d5d6e002
    wget --quiet --output-document=words $WORDS_URL
    if echo $WORDS_SHA512 words | sha512sum --check --quiet; then
      export PASSPHRASE="$(cat words | shuf --random-source=/dev/urandom --head-count 11 | paste --delimiters=' ' --serial | head -c -1)"
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
  - chmod 640 /var/log/cloud-init-output.log
  - |
    virtualenv /opt/ansible-venv
    . /opt/ansible-venv/bin/activate
    pip install ansible-base
    ansible-pull --url "{instance-config-mgt-repo-url}" --checkout "{instance-config-mgt-repo-checkout}" --directory /opt/instance-config-mgt -i /opt/instance-config-mgt/ansible/hosts -e "{ansible-extra-vars}" /opt/instance-config-mgt/ansible/playbook.yml
  - unset PASSPHRASE
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
