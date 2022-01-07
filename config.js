'use strict'

// These are configuration values that allow Exosphere to be customized for a specific organization or use case.
// See README.md for documentation on how to use them.

var config = {
  "showDebugMsgs":false,
  "cloudCorsProxyUrl":"https://try-dev.exosphere.app/proxy",
  "urlPathPrefix":null,
  "palette":null,
  "logo":null,
  "favicon":null,
  "appTitle":null,
  "topBarShowAppTitle":true,
  "defaultLoginView":"oidc",
  "aboutAppMarkdown":null,
  "supportInfoMarkdown":null,
  "userSupportEmail":null,
  "openIdConnectLoginConfig":{
    "keystoneAuthUrl":"https://js2.jetstream-cloud.org:5000/identity/v3",
    "webssoKeystoneEndpoint":"/auth/OS-FEDERATION/websso/openid?origin=https://try-dev.exosphere.app/exosphere/oidc-redirector",
    "oidcLoginIcon":"assets/img/XSEDE_Logo_Black_INF.png",
    "oidcLoginButtonLabel":"Add XSEDE Account",
    "oidcLoginButtonDescription":"Jetstream 2 only"
  },
  "localization":null,
  "instanceConfigMgtRepoUrl":null,
  "instanceConfigMgtRepoCheckout":null
}
