module TestData exposing
    ( cinderLimits
    , cinderQuotaSetDetail
    , novaLimits
    , novaQuotaSetDetail
    , openrcNoExportKeyword
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


openrcNoExportKeyword : String
openrcNoExportKeyword =
    """
OS_REGION_NAME=redacted
OS_PROJECT_DOMAIN_ID=deadbeef
OS_INTERFACE=public
OS_AUTH_URL=https://mycloud.whatever:5000/v3/
OS_USERNAME=redactedusername
OS_PROJECT_ID=deadbeef
OS_USER_DOMAIN_NAME=Default
OS_PROJECT_NAME=redactedprojectname
OS_PASSWORD=redactedpassword
OS_IDENTITY_API_VERSION=3
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
