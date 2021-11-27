'use strict'

var cloud_configs = {
  "clouds":[
    {
      "keystoneHostname":"iu.jetstream-cloud.org",
      "friendlyName":"Jetstream Cloud",
      "friendlySubName":"Indiana University",
      "userAppProxy":"proxy-j7m-iu.exosphere.app",
      "imageExcludeFilter":{
        "filterKey":"atmo_image_include",
        "filterValue":"true"
      },
      "featuredImageNamePrefix":"JS-API-Featured",
      "instanceTypes":[
        {
          "friendlyName":"Ubuntu",
          "description":"- Wide compatibility with community software packages\n\n- Good choice for new users",
          "logo":"assets/img/ubuntu.svg",
          "versions":[
            {
              "friendlyName":"20.04 (latest)",
              "isPrimary":true,
              "imageFilters":{
                "name":"JS-API-Featured-Ubuntu20-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
            },
            {
              "friendlyName":"20.04 with GPU",
              "isPrimary":false,
              "imageFilters":{
                "name":"JS-API-Featured-Ubuntu20-NVIDIA-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":[
                "24",
                "25",
                "26",
                "27",
                "28",
                "29",
                "30",
                "31"
              ]
            },
            {
              "friendlyName":"18.04",
              "isPrimary":false,
              "imageFilters":{
                "name":"JS-API-Featured-Ubuntu18-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
            },
            {
              "friendlyName":"18.04 with MATLAB",
              "isPrimary":false,
              "imageFilters":{
                "name":"JS-API-Featured-Ubuntu18-MATLAB-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
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
              "imageFilters":{
                "name":"JS-API-Featured-CentOS8-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
            },
            {
              "friendlyName":"7",
              "isPrimary":false,
              "imageFilters":{
                "name":"JS-API-Featured-CentOS7-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
            },
            {
              "friendlyName":"7 with GPU",
              "isPrimary":false,
              "imageFilters":{
                "name":"JS-API-Featured-CentOS7-NVIDIA-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":[
                "24",
                "25",
                "26",
                "27",
                "28",
                "29",
                "30",
                "31"
              ]
            },
            {
              "friendlyName":"7 with Intel compiler",
              "isPrimary":false,
              "imageFilters":{
                "name":"JS-API-Featured-CentOS7-Intel-Developer-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
            }
          ]
        }
      ]
    },
    {
      "keystoneHostname":"tacc.jetstream-cloud.org",
      "friendlyName":"Jetstream Cloud",
      "friendlySubName":"TACC",
      "userAppProxy":"proxy-j7m-tacc.exosphere.app",
      "imageExcludeFilter":{
        "filterKey":"atmo_image_include",
        "filterValue":"true"
      },
      "featuredImageNamePrefix":"JS-API-Featured",
      "instanceTypes":[
        {
          "friendlyName":"Ubuntu",
          "description":"- Wide compatibility with community software packages\n\n- Good choice for new users",
          "logo":"assets/img/ubuntu.svg",
          "versions":[
            {
              "friendlyName":"20.04 (latest)",
              "isPrimary":true,
              "imageFilters":{
                "name":"JS-API-Featured-Ubuntu20-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
            },
            {
              "friendlyName":"18.04",
              "isPrimary":false,
              "imageFilters":{
                "name":"JS-API-Featured-Ubuntu18-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
            },
            {
              "friendlyName":"16.04 with MATLAB",
              "isPrimary":false,
              "imageFilters":{
                "name":"JS-API-Featured-Ubuntu16-MATLAB-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
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
              "imageFilters":{
                "name":"JS-API-Featured-CentOS8-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
            },
            {
              "friendlyName":"7",
              "isPrimary":false,
              "imageFilters":{
                "name":"JS-API-Featured-CentOS7-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
            },
            {
              "friendlyName":"7 with Intel compiler",
              "isPrimary":false,
              "imageFilters":{
                "name":"JS-API-Featured-CentOS7-Intel-Developer-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
            }
          ]
        }
      ]
    },
    {
      "keystoneHostname":"keystone.rc.nectar.org.au",
      "friendlyName":"Nectar Cloud",
      "friendlySubName":null,
      "userAppProxy":null,
      "imageExcludeFilter":null,
      "featuredImageNamePrefix":null,
      "instanceTypes":[]
    }
  ]
}
