'use strict'

var config = {
// These are configuration values that allow Exosphere to be customized for a specific organization or use case.
// See README.me for documentation on how to use them.
showDebugMsgs : false,
cloudCorsProxyUrl: "https://try-dev.exosphere.app/proxy",
cloudsWithUserAppProxy:
[ ["iu.jetstream-cloud.org", "proxy-j7m-iu.exosphere.app"],
  ["tacc.jetstream-cloud.org", "proxy-j7m-tacc.exosphere.app"],
],
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
featuredImageNamePrefix: null,
defaultImageExcludeFilter: null,
localization: null
}
