# Dev Notes

## Elm project

```bash
elm-reactor
```

## Mimic & OpenStack

### Start Mimic

```bash
cd ~/dev/mimic
workon mimic
twistd -n mimic
```

### Set your debugging proxy environment variable

```bash
# I run Charles HTTP proxy on port 7777
export HTTP_PROXY=http://127.0.0.1:7777/
export HTTPS_PROXY=${HTTP_PROXY}
```

### Get token using `curl`

```bash
curl --proxy ${HTTP_PROXY} http://localhost:8900
```

```plain
To get started with Mimic, POST an authentication request to:

/identity/v2.0/tokens
```

```bash
curl --silent --proxy ${HTTP_PROXY} -XPOST --data '{"auth":{"RAX-KSKEY:apiKeyCredentials":{"username":"mimic","apiKey":"12345"}}}' http://localhost:8900/identity/v2.0/tokens | python -m json.tool
```

```json
{
    "access": {
        "token": {
            "RAX-AUTH:authenticatedBy": [
                "PASSWORD"
            ],
            "expires": "2017-11-25T14:01:34.999-05:00",
            "id": "token_b0bea817-fcfd-49e5-8b38-d299d7c17a35",
            "tenant": {
                "id": "921375382732800",
                "name": "921375382732800"
            }
        },
        "serviceCatalog": [
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/NovaApi-9a6e11/ORD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/NovaApi-9a6e11/ORD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "DFW",
                        "publicURL": "http://localhost:8900/mimicking/NovaApi-9a6e11/DFW/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/NovaApi-9a6e11/DFW/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "IAD",
                        "publicURL": "http://localhost:8900/mimicking/NovaApi-9a6e11/IAD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/NovaApi-9a6e11/IAD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "compute",
                "name": "cloudServersOpenStack"
            },
            {
                "endpoints": [
                    {
                        "region": "DFW",
                        "publicURL": "http://localhost:8900/mimicking/CinderApi-68bb58/DFW/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/CinderApi-68bb58/DFW/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/CinderApi-68bb58/ORD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/CinderApi-68bb58/ORD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "IAD",
                        "publicURL": "http://localhost:8900/mimicking/CinderApi-68bb58/IAD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/CinderApi-68bb58/IAD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "volume",
                "name": "cinder"
            },
            {
                "endpoints": [
                    {
                        "region": "DFW",
                        "publicURL": "http://localhost:8900/mimicking/CinderApi-68bb58/DFW/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/CinderApi-68bb58/DFW/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/CinderApi-68bb58/ORD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/CinderApi-68bb58/ORD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "IAD",
                        "publicURL": "http://localhost:8900/mimicking/CinderApi-68bb58/IAD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/CinderApi-68bb58/IAD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "volumev2",
                "name": "cinderv2"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/MaasApi-48ab01/ORD/v1.0/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/MaasApi-48ab01/ORD/v1.0/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "rax:monitor",
                "name": "cloudMonitoring"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/HeatApi-2696df/ORD/v1/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/HeatApi-2696df/ORD/v1/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "orchestration",
                "name": "cloudOrchestration"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/CloudFeedsApi-c1dedc/ORD/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/CloudFeedsApi-c1dedc/ORD/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "rax:feeds",
                "name": "cloudFeeds"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/SwiftMock-f72cb9/ORD/v1/MossoCloudFS_0cb50adc-502c-5a55-8fa9-e42d8892807f",
                        "internalURL": "http://localhost:8900/mimicking/SwiftMock-f72cb9/ORD/v1/MossoCloudFS_0cb50adc-502c-5a55-8fa9-e42d8892807f",
                        "tenantId": "MossoCloudFS_0cb50adc-502c-5a55-8fa9-e42d8892807f"
                    },
                    {
                        "region": "DFW",
                        "publicURL": "http://localhost:8900/mimicking/SwiftMock-f72cb9/DFW/v1/MossoCloudFS_0cb50adc-502c-5a55-8fa9-e42d8892807f",
                        "internalURL": "http://localhost:8900/mimicking/SwiftMock-f72cb9/DFW/v1/MossoCloudFS_0cb50adc-502c-5a55-8fa9-e42d8892807f",
                        "tenantId": "MossoCloudFS_0cb50adc-502c-5a55-8fa9-e42d8892807f"
                    },
                    {
                        "region": "IAD",
                        "publicURL": "http://localhost:8900/mimicking/SwiftMock-f72cb9/IAD/v1/MossoCloudFS_0cb50adc-502c-5a55-8fa9-e42d8892807f",
                        "internalURL": "http://localhost:8900/mimicking/SwiftMock-f72cb9/IAD/v1/MossoCloudFS_0cb50adc-502c-5a55-8fa9-e42d8892807f",
                        "tenantId": "MossoCloudFS_0cb50adc-502c-5a55-8fa9-e42d8892807f"
                    }
                ],
                "type": "object-store",
                "name": "cloudFiles"
            },
            {
                "endpoints": [
                    {
                        "region": "",
                        "publicURL": "http://localhost:8900/mimicking/DNSApi-bce61e/v1.0/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/DNSApi-bce61e/v1.0/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "rax:dns",
                "name": "cloudDNS"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/RackConnectV3-e9a82b/ORD/v3/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/RackConnectV3-e9a82b/ORD/v3/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "rax:rackconnect",
                "name": "rackconnect"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/QueueApi-647d23/ORD/v1/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/QueueApi-647d23/ORD/v1/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "DFW",
                        "publicURL": "http://localhost:8900/mimicking/QueueApi-647d23/DFW/v1/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/QueueApi-647d23/DFW/v1/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "IAD",
                        "publicURL": "http://localhost:8900/mimicking/QueueApi-647d23/IAD/v1/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/QueueApi-647d23/IAD/v1/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "rax:queues",
                "name": "cloudQueues"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/LoadBalancerApi-4812ad/ORD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/LoadBalancerApi-4812ad/ORD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "rax:load-balancer",
                "name": "cloudLoadBalancers"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/MaasControlApi-8f661a/ORD/v1.0/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/MaasControlApi-8f661a/ORD/v1.0/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "rax:monitor",
                "name": "cloudMonitoringControl"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/NovaControlApi-32ad60/ORD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/NovaControlApi-32ad60/ORD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "DFW",
                        "publicURL": "http://localhost:8900/mimicking/NovaControlApi-32ad60/DFW/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/NovaControlApi-32ad60/DFW/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "IAD",
                        "publicURL": "http://localhost:8900/mimicking/NovaControlApi-32ad60/IAD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/NovaControlApi-32ad60/IAD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "compute",
                "name": "cloudServersBehavior"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/LoadBalancerControlApi-7a3a26/ORD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/LoadBalancerControlApi-7a3a26/ORD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "rax:load-balancer",
                "name": "cloudLoadBalancerControl"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/GlanceApi-048b43/ORD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/GlanceApi-048b43/ORD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "DFW",
                        "publicURL": "http://localhost:8900/mimicking/GlanceApi-048b43/DFW/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/GlanceApi-048b43/DFW/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "IAD",
                        "publicURL": "http://localhost:8900/mimicking/GlanceApi-048b43/IAD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/GlanceApi-048b43/IAD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "image",
                "name": "cloudImages"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/CloudFeedsControlApi-e0e1d9/ORD/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/CloudFeedsControlApi-e0e1d9/ORD/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "rax:feeds",
                "name": "cloudFeedsControl"
            },
            {
                "endpoints": [
                    {
                        "region": "ORD",
                        "publicURL": "http://localhost:8900/mimicking/NeutronApi-2d851c/ORD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/NeutronApi-2d851c/ORD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "DFW",
                        "publicURL": "http://localhost:8900/mimicking/NeutronApi-2d851c/DFW/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/NeutronApi-2d851c/DFW/v2/921375382732800",
                        "tenantId": "921375382732800"
                    },
                    {
                        "region": "IAD",
                        "publicURL": "http://localhost:8900/mimicking/NeutronApi-2d851c/IAD/v2/921375382732800",
                        "internalURL": "http://localhost:8900/mimicking/NeutronApi-2d851c/IAD/v2/921375382732800",
                        "tenantId": "921375382732800"
                    }
                ],
                "type": "network",
                "name": "cloudNetworks"
            }
        ],
        "user": {
            "RAX-AUTH:defaultRegion": "DFW",
            "id": "-2476143622379761678",
            "roles": [
                {
                    "id": "3",
                    "name": "identity:user-admin",
                    "description": "User Admin Role."
                }
            ],
            "name": "mimic"
        }
    }
}
```


### Get Nova information using `curl`

```bash
export OS_TOKEN="token_id_from_response_above"
OS_BASE_URL=http://localhost
export OS_PROJECT_ID=tenant_id_from_response_above
export MIMIC_NOVA_API_SESSION=NovaApi-343f10  # From above
export MIMIC_ZONE=ORD  # From above
export OS_URL=${OS_BASE_URL}:8900/mimicking/${MIMIC_NOVA_API_SESSION}/${MIMIC_ZONE}/v2
export OS_COMPUTE_API=${OS_URL}/${OS_PROJECT_ID}

curl --proxy ${HTTP_PROXY} --insecure -s -H "X-Auth-Token: $OS_TOKEN" $OS_COMPUTE_API/flavors | python -mjson.tool
curl --proxy ${HTTP_PROXY} --insecure -s -H "X-Auth-Token: $OS_TOKEN" $OS_COMPUTE_API/images | python -m json.tool
curl --proxy ${HTTP_PROXY} --insecure -s -H "X-Auth-Token: $OS_TOKEN" $OS_COMPUTE_API/servers | python -m json.tool
```


### Get token using `python-openstackclient`

```bash
workon python-openstackclient
openstack --os-username mimic --os-password 1235 --os-auth-url http://localhost:8900/identity/v2.0/ token issue
```

```plain
+------------+--------------------------------------------+
| Field      | Value                                      |
+------------+--------------------------------------------+
| expires    | 2017-11-25T14:09:34-0500                   |
| id         | token_b0bea817-fcfd-49e5-8b38-d299d7c17a35 |
| project_id | 921375382732800                            |
| user_id    | -2476143622379761678                       |
+------------+--------------------------------------------+
```

Note: For some reason won't work without username & password

```bash
openstack --os-username mimic --os-access-token token_f98e6af3-16c5-439e-866c-6d21317b7282 --os-auth-url http://localhost:8900/identity/v2.0/ catalog list
```

### Get Nova information using `python-openstackclient`

_WIP_ Not currently working.

```bash
export HTTP_PROXY=http://127.0.0.1:7777/
export HTTPS_PROXY=${HTTP_PROXY}

export OS_TOKEN="token_196d06b6-0204-47ad-ac62-cc794bc035f0"
OS_BASE_URL=http://localhost
export OS_PROJECT_ID=5571277094912
MIMIC_NOVA_API_SESSION=NovaApi-343f10
MIMIC_ZONE=ORD
export OS_URL=${OS_BASE_URL}:8900/mimicking/${MIMIC_NOVA_API_SESSION}/${MIMIC_ZONE}/v2
openstack --insecure server list
```


### Compared with a real OpenStack

```bash
source openrc.sh
openstack --insecure catalog list
```

```plain
+----------+----------------+---------------------------------------------------------------------------------------+
| Name     | Type           | Endpoints                                                                             |
+----------+----------------+---------------------------------------------------------------------------------------+
| heat-cfn | cloudformation | RegionOne                                                                             |
|          |                |   public: https://marana-cloud.cyverse.org:8000/v1                                    |
|          |                | RegionOne                                                                             |
|          |                |   admin: http://172.29.236.130:8000/v1                                                |
|          |                | RegionOne                                                                             |
|          |                |   internal: http://172.29.236.130:8000/v1                                             |
|          |                |                                                                                       |
| neutron  | network        | RegionOne                                                                             |
|          |                |   public: https://marana-cloud.cyverse.org:9696                                       |
|          |                | RegionOne                                                                             |
|          |                |   admin: http://172.29.236.130:9696                                                   |
|          |                | RegionOne                                                                             |
|          |                |   internal: http://172.29.236.130:9696                                                |
|          |                |                                                                                       |
| cinderv2 | volumev2       | RegionOne                                                                             |
|          |                |   admin: http://172.29.236.130:8776/v2/deadbeefdeadbeefdeadbeefdeadbeef               |
|          |                | RegionOne                                                                             |
|          |                |   public: https://marana-cloud.cyverse.org:8776/v2/deadbeefdeadbeefdeadbeefdeadbeef   |
|          |                | RegionOne                                                                             |
|          |                |   internal: http://172.29.236.130:8776/v2/deadbeefdeadbeefdeadbeefdeadbeef            |
|          |                |                                                                                       |
| nova     | compute        | RegionOne                                                                             |
|          |                |   admin: http://172.29.236.130:8774/v2.1/deadbeefdeadbeefdeadbeefdeadbeef             |
|          |                | RegionOne                                                                             |
|          |                |   public: https://marana-cloud.cyverse.org:8774/v2.1/deadbeefdeadbeefdeadbeefdeadbeef |
|          |                | RegionOne                                                                             |
|          |                |   internal: http://172.29.236.130:8774/v2.1/deadbeefdeadbeefdeadbeefdeadbeef          |
|          |                |                                                                                       |
| keystone | identity       | RegionOne                                                                             |
|          |                |   internal: http://172.29.236.130:5000/v3                                             |
|          |                | RegionOne                                                                             |
|          |                |   public: https://marana-cloud.cyverse.org:5000/v3                                    |
|          |                | RegionOne                                                                             |
|          |                |   admin: https://marana-cloud.cyverse.org:35357/v3                                    |
|          |                |                                                                                       |
| heat     | orchestration  | RegionOne                                                                             |
|          |                |   internal: http://172.29.236.130:8004/v1/deadbeefdeadbeefdeadbeefdeadbeef            |
|          |                | RegionOne                                                                             |
|          |                |   admin: http://172.29.236.130:8004/v1/deadbeefdeadbeefdeadbeefdeadbeef               |
|          |                | RegionOne                                                                             |
|          |                |   public: https://marana-cloud.cyverse.org:8004/v1/deadbeefdeadbeefdeadbeefdeadbeef   |
|          |                |                                                                                       |
| cinder   | volume         | RegionOne                                                                             |
|          |                |   admin: http://172.29.236.130:8776/v1/deadbeefdeadbeefdeadbeefdeadbeef               |
|          |                | RegionOne                                                                             |
|          |                |   internal: http://172.29.236.130:8776/v1/deadbeefdeadbeefdeadbeefdeadbeef            |
|          |                | RegionOne                                                                             |
|          |                |   public: https://marana-cloud.cyverse.org:8776/v1/deadbeefdeadbeefdeadbeefdeadbeef   |
|          |                |                                                                                       |
| glance   | image          | RegionOne                                                                             |
|          |                |   internal: http://172.29.236.130:9292                                                |
|          |                | RegionOne                                                                             |
|          |                |   public: https://marana-cloud.cyverse.org:9292                                       |
|          |                | RegionOne                                                                             |
|          |                |   admin: http://172.29.236.130:9292                                                   |
|          |                |                                                                                       |
+----------+----------------+---------------------------------------------------------------------------------------+
```

```bash
openstack --insecure server list
```

```plain
+--------------------------------------+---------------------------+--------+------------------------------------------+---------------------------------+--------+
| ID                                   | Name                      | Status | Networks                                 | Image                           | Flavor |
+--------------------------------------+---------------------------+--------+------------------------------------------+---------------------------------+--------+
| 1eadbeef-dead-beef-dead-beefdeadbeef | Ubuntu 16_04 Non-GUI Base | ACTIVE | julianp-net=172.30.71.17, 128.196.142.83 | Ubuntu 16.04 Non-GUI Base v.1.0 | tiny1  |
| 5eadbeef-dead-beef-dead-beefdeadbeef | Ubuntu 16_04 Non-GUI Base | ACTIVE | julianp-net=172.30.71.14                 | Ubuntu 16.04 Non-GUI Base       | tiny1  |
| ceadbeef-dead-beef-dead-beefdeadbeef | Ubuntu 14_04_2 XFCE Base  | ACTIVE | julianp-net=172.30.71.5                  | Ubuntu 14.04.2 XFCE Base        | tiny1  |
| feadbeef-dead-beef-dead-beefdeadbeef | Redash 2017-07-22         | ACTIVE | julianp-net=172.30.71.13, 128.196.142.71 | Ubuntu 16.04 Non-GUI Base       | small2 |
+--------------------------------------+---------------------------+--------+------------------------------------------+---------------------------------+--------+
```

Notes:

- OpenStack sends username and password to `/v3/auth/tokens` which returns a token which is used to get servers and such.

Try to use this directly:

```bash
openstack --insecure token issue
```

```plain
+------------+-------------------------------+
| Field      | Value                         |
+------------+-------------------------------+
| expires    | 2017-11-25T09:57:23+0000      |
| id         | gAAAAAB....                   |
| project_id | d6fa4f2b....                  |
| user_id    | 8c9eb3bc....                  |
+------------+-------------------------------+
```


```bash
#export OS_AUTH_TOKEN=<long-X-Subject-Token-from-response-same-as-id-from-above>
unset OS_PASSWORD
unset OS_AUTH_TYPE
export OS_AUTH_TYPE=v3token
export OS_USER_ID=8c9eb3bc....
export OS_PROJECT_ID=d6fa4f2b....
export OS_TOKEN=gAAAAAB....v90
export OS_ACCESS_TOKEN=gAAAAAB....v90

```

Here we go...

```bash
export HTTP_PROXY=http://127.0.0.1:7777/
export HTTPS_PROXY=${HTTP_PROXY}

curl --insecure -v -s -X POST $OS_AUTH_URL/auth/tokens?nocatalog   -H "Content-Type: application/json"   -d '{ "auth": { "identity": { "methods": ["password"],"password": {"user": {"domain": {"name": "'"$OS_USER_DOMAIN_NAME"'"},"name": "'"$OS_USERNAME"'", "password": "'"$OS_PASSWORD"'"} } }, "scope": { "project": { "domain": { "name": "'"$OS_PROJECT_DOMAIN_NAME"'" }, "name":  "'"$OS_PROJECT_NAME"'" } } }}' \
| python -m json.tool

export OS_TOKEN="<X-Subject-Token from response above>"
OS_BASE_URL=https://marana-cloud.cyverse.org
export OS_PROJECT_ID=d6fa4f2b....
export OS_URL=${OS_BASE_URL}:8774/v2.1
export OS_COMPUTE_API=${OS_URL}/${OS_PROJECT_ID}

curl --insecure -s -H "X-Auth-Token: $OS_TOKEN" $OS_COMPUTE_API/flavors | python -mjson.tool
curl --insecure -s -H "X-Auth-Token: $OS_TOKEN" $OS_COMPUTE_API/images | python -m json.tool
curl --insecure -s -H "X-Auth-Token: $OS_TOKEN" $OS_COMPUTE_API/servers | python -m json.tool
```

New terminal:

```bash
export HTTP_PROXY=http://127.0.0.1:7777/
export HTTPS_PROXY=${HTTP_PROXY}

export OS_TOKEN="<X-Subject-Token from response above>"
OS_BASE_URL=https://marana-cloud.cyverse.org
export OS_PROJECT_ID=d6fa4f2b....
export OS_URL=${OS_BASE_URL}:8774/v2.1
openstack --insecure server list
```

Note: This does not work for Mimic for some reason.


### Compared with openstackinabox

Once patches are applied...

Note: Initial user data:

```python
{
    'username': u'system',
    'apikey': u'537461636b496e41426f78',
    'user_id': 1,
    'tenant_id': 1,
    'enabled': True,
    'password': u'stackinabox',
    'email': u'system@stackinabox'
}
```

```bash
export HTTP_PROXY=http://127.0.0.1:7777/
export HTTPS_PROXY=${HTTP_PROXY}

curl -s --dump-header - --proxy ${HTTP_PROXY} -H "X-Session-ID: a8e44979-8434-4028-950b-01af2b1dd653" -XPOST http://localhost:8081/admin/

curl -s --proxy ${HTTP_PROXY} http://localhost:8081/admin/ | python -mjson.tool

curl -s --proxy ${HTTP_PROXY} -XPOST --data '{"auth":{"RAX-KSKEY:apiKeyCredentials":{"username":"system","apiKey":"537461636b496e41426f78"}}}' http://localhost:8081/stackinabox/a8e44979-8434-4028-950b-01af2b1dd653/keystone/v2.0/tokens | python -mjson.tool
```


#### References

- <https://developer.openstack.org/api-ref/identity/v2/> (For dealing with Mimic)
- <https://docs.openstack.org/python-openstackclient/latest/cli/authentication.html>
- <https://docs.openstack.org/keystone/pike/admin/identity-tokens.html>

> *Project-scoped tokens*
> 
> Project-scoped tokens are the bread and butter of OpenStack. They express your authorization to operate in a specific tenancy of the cloud and are useful to authenticate yourself when working with most other services.
> 
> They contain a service catalog, a set of roles, and details of the project upon which you have authorization.

- <https://docs.openstack.org/keystone/latest/api_curl_examples.html>
- <https://docs.openstack.org/keystone/pike/api_curl_examples.html>

This might be it:

- <https://developer.openstack.org/api-guide/quick-start/api-quick-start.html>

> When you send API requests, you include the token in the X-Auth-Token header. If you access multiple OpenStack services, you must get a token for each service.

The fuck!? ^^^
Update: I don't think this is true based on my experiments above.


