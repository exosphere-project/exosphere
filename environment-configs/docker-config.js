'use strict'

var config = {
// These are configuration values that allow Exosphere to be customized for a specific organization or use case.
// See README.me for documentation on how to use them.
  showDebugMsgs: false,
  cloudCorsProxyUrl: "/proxy",
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
      userAppProxy: null,
      imageExcludeFilter: {
        filterKey: "atmo_image_include",
        filterValue: "true"
      },
      featuredImageNamePrefix: "JS-API-Featured",
      operatingSystemChoices: []
    },
    {
      keystoneHostname: "tacc.jetstream-cloud.org",
      userAppProxy: null,
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
