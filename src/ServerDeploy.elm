module ServerDeploy exposing (cloudInitUserDataTemplate, guacamoleUserData)


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
packages:
  - cockpit
runcmd:
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
  - systemctl enable cockpit.socket
  - systemctl start cockpit.socket
  - systemctl daemon-reload
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
    {guacamole-setup}
  - unset PASSPHRASE
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
    """cd /opt
    GUAC_CONFIG_GIT_TAG=3
    GUAC_CONFIG_URL=https://gitlab.com/exosphere/guacamole-config/-/archive/$GUAC_CONFIG_GIT_TAG/guacamole-config-$GUAC_CONFIG_GIT_TAG.tar.gz
    GUAC_CONFIG_SHA512=ccc3cae8576f5db576476422dd3ce93af154d27bc964dc0eb59ac02ca1564101ff2d7ad28b37246bf11e104650df1f02883875c557f36095a3cb6cff835b07cf
    wget --quiet --output-document=guacamole-config.tar.gz $GUAC_CONFIG_URL
    if echo $GUAC_CONFIG_SHA512 guacamole-config.tar.gz | sha512sum --check --quiet; then
      tar -zxvf guacamole-config.tar.gz
      mv guacamole-config-$GUAC_CONFIG_GIT_TAG guacamole-config
      cd guacamole-config
      /bin/bash deploy-guac.sh "$PASSPHRASE"
    fi
"""
