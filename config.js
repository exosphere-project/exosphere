'use strict'

var config = {
// These are configuration values that allow Exosphere to be customized for a specific organization or use case.
// See README.md for documentation on how to use them.
  showDebugMsgs: false,
  cloudCorsProxyUrl: "https://try-dev.exosphere.app/proxy",
  urlPathPrefix: null,
  palette: null,
  logo: null,
  favicon: null,
  appTitle: null,
  topBarShowAppTitle: true,
  defaultLoginView: null,
  aboutAppMarkdown: null,
  supportInfoMarkdown: null,
  userSupportEmail: null,
  openIdConnectLoginConfig: null,
  localization: null,
  clouds: [
    {
      keystoneHostname: "iu.jetstream-cloud.org",
      userAppProxy: "proxy-j7m-iu.exosphere.app",
      imageExcludeFilter: {
        filterKey: "atmo_image_include",
        filterValue: "true"
      },
      featuredImageNamePrefix: "JS-API-Featured",
      operatingSystemChoices: [
      {
        "friendlyName":"Ubuntu",
        "description":"Wide compatibility with community software packages, good choice for new users",
        "logo":"assets/img/ubuntu.svg",
        "versions":[
          {
            "friendlyName":"20.04 (Latest)",
            "isPrimary":true,
            "filters":{
              "name":"JS-API-Featured-Ubuntu20-Latest",
              "visibility": "public"
            }
          },
          {
            "friendlyName":"18.04",
            "isPrimary":false,
            "filters":{
              "osDistro":"ubuntu",
              "osVersion":"18.04",
              "visibility": "public"
            }
          }
        ]
      },
      {
        "friendlyName":"CentOS",
        "description":"Based on Red Hat Enterprise Linux (RHEL), compatible with RPM-based software",
        "logo":"assets/img/centos.svg",
        "versions":[
          {
            "friendlyName":"8 (Latest)",
            "isPrimary":true,
            "filters":{
              "name":"JS-API-Featured-CentOS8-Latest",
              "visibility": "public"
            }
          },
          {
            "friendlyName":"7",
            "isPrimary":false,
            "filters":{
              "name":"JS-API-Featured-CentOS7-Latest",
              "visibility": "public"
            }
          }
        ]
      }
    ]


    },
    {
      keystoneHostname: "tacc.jetstream-cloud.org",
      userAppProxy: "proxy-j7m-tacc.exosphere.app",
      imageExcludeFilter: {
        filterKey: "atmo_image_include",
        filterValue: "true"
      },
      featuredImageNamePrefix: "JS-API-Featured",
      operatingSystemChoices: []

    }
  ],
  instanceConfigMgtRepoUrl: null,
  instanceConfigMgtRepoCheckout: null
}
