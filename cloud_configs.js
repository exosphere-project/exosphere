'use strict'

var cloud_configs = {
  "clouds":[
    {
      "keystoneHostname":"iu.jetstream-cloud.org",
      "userAppProxy":"proxy-j7m-iu.exosphere.app",
      "imageExcludeFilter":{
        "filterKey":"atmo_image_include",
        "filterValue":"true"
      },
      "featuredImageNamePrefix":"JS-API-Featured",
      "operatingSystemChoices":[
        {
          "friendlyName":"Ubuntu",
          "description":"- Wide compatibility with community software packages\n\n- Good choice for new users",
          "logo":"assets/img/ubuntu.svg",
          "versions":[
            {
              "friendlyName":"20.04 (latest)",
              "isPrimary":true,
              "filters":{
                "name":"JS-API-Featured-Ubuntu20-Latest",
                "visibility":"public"
              }
            },
            {
              "friendlyName":"20.04 with NVIDIA drivers",
              "isPrimary":false,
              "filters":{
                "name":"JS-API-Featured-Ubuntu20-NVIDIA-Latest",
                "visibility":"public"
              }
            },
            {
              "friendlyName":"18.04",
              "isPrimary":false,
              "filters":{
                "name":"JS-API-Featured-Ubuntu18-Latest",
                "visibility":"public"
              }
            },
            {
              "friendlyName":"18.04 with MATLAB",
              "isPrimary":false,
              "filters":{
                "name":"JS-API-Featured-Ubuntu18-MATLAB-Latest",
                "visibility":"public"
              }
            }
          ]
        },
        {
          "friendlyName":"CentOS",
          "description":"- Based on Red Hat Enterprise Linux (RHEL)\n\n- Compatible with RPM-based software",
          "logo":"assets/img/centos.svg",
          "versions":[
            {
              "friendlyName":"8 (latest)",
              "isPrimary":true,
              "filters":{
                "name":"JS-API-Featured-CentOS8-Latest",
                "visibility":"public"
              }
            },
            {
              "friendlyName":"7",
              "isPrimary":false,
              "filters":{
                "name":"JS-API-Featured-CentOS7-Latest",
                "visibility":"public"
              }
            },
            {
              "friendlyName":"7 with NVIDIA drivers",
              "isPrimary":false,
              "filters":{
                "name":"JS-API-Featured-CentOS7-NVIDIA-Latest",
                "visibility":"public"
              }
            },
            {
              "friendlyName":"7 with Intel compiler",
              "isPrimary":false,
              "filters":{
                "name":"JS-API-Featured-CentOS7-Intel-Developer-Latest",
                "visibility":"public"
              }
            }
          ]
        }
      ]
    },
    {
      "keystoneHostname":"tacc.jetstream-cloud.org",
      "userAppProxy":"proxy-j7m-tacc.exosphere.app",
      "imageExcludeFilter":{
        "filterKey":"atmo_image_include",
        "filterValue":"true"
      },
      "featuredImageNamePrefix":"JS-API-Featured",
      "operatingSystemChoices":[
        {
          "friendlyName":"Ubuntu",
          "description":"- Wide compatibility with community software packages\n\n- Good choice for new users",
          "logo":"assets/img/ubuntu.svg",
          "versions":[
            {
              "friendlyName":"20.04 (latest)",
              "isPrimary":true,
              "filters":{
                "name":"JS-API-Featured-Ubuntu20-Latest",
                "visibility":"public"
              }
            },
            {
              "friendlyName":"18.04",
              "isPrimary":false,
              "filters":{
                "name":"JS-API-Featured-Ubuntu18-Latest",
                "visibility":"public"
              }
            },
            {
              "friendlyName":"16.04 with MATLAB",
              "isPrimary":false,
              "filters":{
                "name":"JS-API-Featured-Ubuntu16-MATLAB-Latest",
                "visibility":"public"
              }
            }
          ]
        },
        {
          "friendlyName":"CentOS",
          "description":"- Based on Red Hat Enterprise Linux (RHEL)\n\n- Compatible with RPM-based software",
          "logo":"assets/img/centos.svg",
          "versions":[
            {
              "friendlyName":"8 (latest)",
              "isPrimary":true,
              "filters":{
                "name":"JS-API-Featured-CentOS8-Latest",
                "visibility":"public"
              }
            },
            {
              "friendlyName":"7",
              "isPrimary":false,
              "filters":{
                "name":"JS-API-Featured-CentOS7-Latest",
                "visibility":"public"
              }
            },
            {
              "friendlyName":"7 with Intel compiler",
              "isPrimary":false,
              "filters":{
                "name":"JS-API-Featured-CentOS7-Intel-Developer-Latest",
                "visibility":"public"
              }
            }
          ]
        }
      ]
    }
  ],
}