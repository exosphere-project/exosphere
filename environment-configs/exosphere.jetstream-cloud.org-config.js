'use strict'

var config = {
// These are configuration values that allow Exosphere to be customized for a specific organization or use case.
// See README.me for documentation on how to use them.
showDebugMsgs : false,
cloudCorsProxyUrl: "https://exosphere.jetstream-cloud.org/proxy",
cloudsWithUserAppProxy:
[ ["iu.jetstream-cloud.org", "proxy-j7m-iu.exosphere.app"],
  ["tacc.jetstream-cloud.org", "proxy-j7m-tacc.exosphere.app"],
],
urlPathPrefix: "exosphere",
palette: { primary: {r: 155, g: 33, b: 35}, secondary: {r: 52, g: 122, b: 140} },
logo: "assets/img/jetstream-logo.svg",
favicon: "assets/img/jetstream-favicon.ico",
appTitle: "Jetstream Cloud",
defaultLoginView: "jetstream",
aboutAppMarkdown: "This is the Exosphere interface for [Jetstream Cloud](https://jetstream-cloud.org), currently in beta. If you require assistance, please email help@jetstream-cloud.org and specify you are using Exosphere.",
}