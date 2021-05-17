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
      featuredImageNamePrefix: "JS-API-Featured"
    },
    {
      keystoneHostname: "tacc.jetstream-cloud.org",
      userAppProxy: "proxy-j7m-tacc.exosphere.app",
      imageExcludeFilter: {
        filterKey: "atmo_image_include",
        filterValue: "true"
      },
      featuredImageNamePrefix: "JS-API-Featured"
    }
  ]
}
