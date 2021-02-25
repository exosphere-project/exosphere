module TestData exposing
    ( cinderLimits
    , cinderQuotaSetDetail
    , glanceImageAtmoInclude
    , glanceImageAtmoIncludeBare
    , novaLimits
    , novaQuotaSetDetail
    , openrcPreV3
    , openrcV3
    , openrcV3withComments
    )


openrcV3withComments : String
openrcV3withComments =
    """
#!/usr/bin/env bash
# To use an OpenStack cloud you need to authenticate against the Identity
# service named keystone, which returns a **Token** and **Service Catalog**.
# The catalog contains the endpoints for all services the user/tenant has
# access to - such as Compute, Image Service, Identity, Object Storage, Block
# Storage, and Networking (code-named nova, glance, keystone, swift,
# cinder, and neutron).
#
# *NOTE*: Using the 3 *Identity API* does not necessarily mean any other
# OpenStack API is version 3. For example, your cloud provider may implement
# Image API v1.1, Block Storage API v2, and Compute API v2.0. OS_AUTH_URL is
# only for the Identity API served through keystone.
export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
# With the addition of Keystone we have standardized on the term **project**
# as the entity that owns the resources.
export OS_PROJECT_ID=1d00d4b1de1d00d4b1de1d00d4b1de
export OS_PROJECT_NAME="cloud-riders"
export OS_USER_DOMAIN_NAME="Default"
if [ -z "$OS_USER_DOMAIN_NAME" ]; then unset OS_USER_DOMAIN_NAME; fi
export OS_PROJECT_DOMAIN_ID="default"
if [ -z "$OS_PROJECT_DOMAIN_ID" ]; then unset OS_PROJECT_DOMAIN_ID; fi
# unset v2.0 items in case set
unset OS_TENANT_ID
unset OS_TENANT_NAME
# In addition to the owning entity (tenant), OpenStack stores the entity
# performing the action as the **user**.
export OS_USERNAME="enfysnest"
# With Keystone you pass the keystone password.
echo "Please enter your OpenStack Password for project $OS_PROJECT_NAME as user $OS_USERNAME: "
read -sr OS_PASSWORD_INPUT
export OS_PASSWORD=$OS_PASSWORD_INPUT
# If your configuration has multiple regions, we set that information here.
# OS_REGION_NAME is optional and only valid in certain environments.
export OS_REGION_NAME="CellOne"
# Don't leave a blank variable, unset it if it was empty
if [ -z "$OS_REGION_NAME" ]; then unset OS_REGION_NAME; fi
export OS_INTERFACE=public
export OS_IDENTITY_API_VERSION=3
"""


openrcV3 : String
openrcV3 =
    """
#!/usr/bin/env bash

export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
export OS_PROJECT_ID=1d00d4b1de1d00d4b1de1d00d4b1de
export OS_PROJECT_NAME="cloud-riders"
export OS_USER_DOMAIN_NAME="Default"
if [ -z "$OS_USER_DOMAIN_NAME" ]; then unset OS_USER_DOMAIN_NAME; fi
export OS_PROJECT_DOMAIN_ID="default"
if [ -z "$OS_PROJECT_DOMAIN_ID" ]; then unset OS_PROJECT_DOMAIN_ID; fi
unset OS_TENANT_ID
unset OS_TENANT_NAME
export OS_USERNAME="enfysnest"
echo "Please enter your OpenStack Password for project $OS_PROJECT_NAME as user $OS_USERNAME: "
read -sr OS_PASSWORD_INPUT
export OS_PASSWORD=$OS_PASSWORD_INPUT
export OS_REGION_NAME="CellOne"
if [ -z "$OS_REGION_NAME" ]; then unset OS_REGION_NAME; fi
export OS_INTERFACE=public
export OS_IDENTITY_API_VERSION=3
"""


openrcPreV3 : String
openrcPreV3 =
    """
export OS_PROJECT_NAME="cloud-riders"
export OS_USERNAME="enfysnest"
export OS_IDENTITY_API_VERSION=3
export OS_USER_DOMAIN_NAME="default"
export OS_TENANT_NAME="enfysnest"
export OS_AUTH_URL="https://cell.alliance.rebel:35357/v3"
export OS_PROJECT_DOMAIN_NAME="default"
export OS_REGION_NAME="CellOne"
export OS_PASSWORD=$OS_PASSWORD_INPUT
    """


cinderQuotaSetDetail : String
cinderQuotaSetDetail =
    """
{
    "quota_set": {
        "per_volume_gigabytes": {
            "reserved": 0,
            "allocated": 0,
            "limit": -1,
            "in_use": 0
        },
        "groups": {
            "reserved": 0,
            "allocated": 0,
            "limit": 10,
            "in_use": 0
        },
        "gigabytes": {
            "reserved": 0,
            "allocated": 0,
            "limit": 1000,
            "in_use": 82
        },
        "backup_gigabytes": {
            "reserved": 0,
            "allocated": 0,
            "limit": 1000,
            "in_use": 267
        },
        "snapshots": {
            "reserved": 0,
            "allocated": 0,
            "limit": 10,
            "in_use": 0
        },
        "volumes_rbd": {
            "reserved": 0,
            "allocated": 0,
            "limit": -1,
            "in_use": 0
        },
        "volumes": {
            "reserved": 0,
            "allocated": 0,
            "limit": 10,
            "in_use": 5
        },
        "gigabytes_rbd": {
            "reserved": 0,
            "allocated": 0,
            "limit": -1,
            "in_use": 0
        },
        "backups": {
            "reserved": 0,
            "allocated": 0,
            "limit": -1,
            "in_use": 13
        },
        "snapshots_rbd": {
            "reserved": 0,
            "allocated": 0,
            "limit": -1,
            "in_use": 0
        },
        "id": "1b29b1a686d34c769b501e7dbb64765a"
    }
}
    """


cinderLimits : String
cinderLimits =
    """
{
    "limits": {
        "rate": [],
        "absolute": {
            "totalSnapshotsUsed": 0,
            "maxTotalBackups": -1,
            "maxTotalVolumeGigabytes": 1000,
            "maxTotalSnapshots": 10,
            "maxTotalBackupGigabytes": 1000,
            "totalBackupGigabytesUsed": 267,
            "maxTotalVolumes": 10,
            "totalVolumesUsed": 5,
            "totalBackupsUsed": 13,
            "totalGigabytesUsed": 82
        }
    }
}
    """


novaQuotaSetDetail : String
novaQuotaSetDetail =
    """
{
    "quota_set": {
        "injected_file_content_bytes": {
            "reserved": 0,
            "limit": 10240,
            "in_use": 0
        },
        "metadata_items": {
            "reserved": 0,
            "limit": 128,
            "in_use": 0
        },
        "server_group_members": {
            "reserved": 0,
            "limit": 10,
            "in_use": 0
        },
        "server_groups": {
            "reserved": 0,
            "limit": 10,
            "in_use": 0
        },
        "ram": {
            "reserved": 0,
            "limit": 999999,
            "in_use": 1024
        },
        "floating_ips": {
            "reserved": 0,
            "limit": 10,
            "in_use": 0
        },
        "key_pairs": {
            "reserved": 0,
            "limit": 100,
            "in_use": 0
        },
        "id": "1b29b1a686d34c769b501e7dbb64765a",
        "instances": {
            "reserved": 0,
            "limit": 10,
            "in_use": 1
        },
        "security_group_rules": {
            "reserved": 0,
            "limit": 20,
            "in_use": 0
        },
        "injected_files": {
            "reserved": 0,
            "limit": 5,
            "in_use": 0
        },
        "cores": {
            "reserved": 0,
            "limit": 48,
            "in_use": 1
        },
        "fixed_ips": {
            "reserved": 0,
            "limit": -1,
            "in_use": 0
        },
        "injected_file_path_bytes": {
            "reserved": 0,
            "limit": 255,
            "in_use": 0
        },
        "security_groups": {
            "reserved": 0,
            "limit": 10,
            "in_use": 1
        }
    }
}
    """


novaLimits : String
novaLimits =
    """
{
    "limits": {
        "rate": [],
        "absolute": {
            "maxServerMeta": 128,
            "maxPersonality": 5,
            "totalServerGroupsUsed": 0,
            "maxImageMeta": 128,
            "maxPersonalitySize": 10240,
            "maxTotalKeypairs": 100,
            "maxSecurityGroupRules": 20,
            "maxServerGroups": 10,
            "totalCoresUsed": 1,
            "totalRAMUsed": 1024,
            "totalInstancesUsed": 1,
            "maxSecurityGroups": 10,
            "totalFloatingIpsUsed": 0,
            "maxTotalCores": 48,
            "maxServerGroupMembers": 10,
            "maxTotalFloatingIps": 10,
            "totalSecurityGroupsUsed": 1,
            "maxTotalInstances": 10,
            "maxTotalRAMSize": 999999
        }
    }
}
    """


glanceImageAtmoIncludeBare : String
glanceImageAtmoIncludeBare =
    """
        {
            "atmo_image_exclude": "false",
            "atmo_image_include": "true"
        }
    """


glanceImageAtmoInclude : String
glanceImageAtmoInclude =
    """
        {
            "application_description": "Updated version of: maker 2.31.9 with CC tools v3.1. This image also contains BUSCO and hhmer to train Augustus",
            "application_name": "WQmaker 2.31.9 with BUSCO",
            "application_owner": "scijake",
            "application_tags": ["genomics", "annotation", "NGS"],
            "application_uuid": "0eb33584-ab5c-5d58-8e59-f7bea25dff04",
            "atmo_image_exclude": "false",
            "atmo_image_include": "true",
            "checksum": "99a988b319a3c391b79435e754d637bd",
            "container_format": "bare",
            "created_at": "2019-09-20T16:40:50Z",
            "direct_url": "rbd://63ec33d7-f6cd-4276-bff9-61769ac13960/glance-images/1354da3d-aece-4c1a-a70b-5b262c766475/snap",
            "disk_format": "raw",
            "file": "/v2/images/1354da3d-aece-4c1a-a70b-5b262c766475/file",
            "hw_disk_bus": "scsi",
            "hw_qemu_guest_agent": "yes",
            "hw_scsi_model": "virtio-scsi",
            "id": "1354da3d-aece-4c1a-a70b-5b262c766475",
            "locations": [
                {
                    "metadata": {},
                    "url": "rbd://63ec33d7-f6cd-4276-bff9-61769ac13960/glance-images/1354da3d-aece-4c1a-a70b-5b262c766475/snap"
                }
            ],
            "min_disk": 0,
            "min_ram": 0,
            "name": "WQmaker 2.31.9 with BUSCO v.1.0",
            "os_hash_algo": "sha512",
            "os_hash_value": "6f4213d02fcc35c4c424a1b236da4f5004216ef9f363b34524ae2717fc20f0a2e3c89a7cd6061f6b15c0c328a423166aad91b4655c2a0f2d68b12959cdebb7e3",
            "os_hidden": false,
            "os_require_quiesce": "yes",
            "owner": "e0c43302e9a740a480d05381d20aa66e",
            "protected": false,
            "schema": "/v2/schemas/image",
            "self": "/v2/images/1354da3d-aece-4c1a-a70b-5b262c766475",
            "size": 64424509440,
            "status": "active",
            "tags": [],
            "updated_at": "2019-09-20T17:35:52Z",
            "version_changelog": "v.1.0 - updated Upendra Devisetty's image and installed BUSO and hhmer",
            "version_name": "1.0",
            "virtual_size": null,
            "visibility": "public"
        }
    """


glanceImageSkipAtmosphere : String
glanceImageSkipAtmosphere =
    """
        {
            "checksum": "28e2e88aeac653d4a8f91b6ab1bd0f75",
            "container_format": "bare",
            "created_at": "2021-02-24T15:28:32Z",
            "direct_url": "rbd://63ec33d7-f6cd-4276-bff9-61769ac13960/glance-images/fc60c79b-5dfa-4928-a561-76df79ef5f6e/snap",
            "disk_format": "raw",
            "file": "/v2/images/fc60c79b-5dfa-4928-a561-76df79ef5f6e/file",
            "hw_disk_bus": "scsi",
            "hw_qemu_guest_agent": "yes",
            "hw_scsi_model": "virtio-scsi",
            "id": "fc60c79b-5dfa-4928-a561-76df79ef5f6e",
            "locations": [
                {
                    "metadata": {},
                    "url": "rbd://63ec33d7-f6cd-4276-bff9-61769ac13960/glance-images/fc60c79b-5dfa-4928-a561-76df79ef5f6e/snap"
                }
            ],
            "min_disk": 0,
            "min_ram": 0,
            "name": "JS-API-Featured-Ubuntu18-Latest",
            "os_hash_algo": "sha512",
            "os_hash_value": "34fd8f0c4a713bfabb46facfbf27c31aa2ef09b54e5acf00bbf6c6e8857344190226293167aa701e54916358121656f919537f10f6c23139c6c68b1911b372cf",
            "os_hidden": false,
            "os_require_quiesce": "yes",
            "owner": "cc910278de874519816f7e8b9a1c086b",
            "protected": false,
            "schema": "/v2/schemas/image",
            "self": "/v2/images/fc60c79b-5dfa-4928-a561-76df79ef5f6e",
            "size": 8589934592,
            "skip_atmosphere": "yes",
            "status": "active",
            "tags": [],
            "updated_at": "2021-02-24T15:54:08Z",
            "virtual_size": null,
            "visibility": "public"
        }
    """
