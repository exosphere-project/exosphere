"use strict";

/** @type {Exosphere.Configuration} */
var config = {
  showDebugMsgs: false,
  cloudCorsProxyUrl: "https://try-dev.exosphere.app/proxy",
  urlPathPrefix: "exosphere",
  palette: null,
  logo: null,
  favicon: null,
  appTitle: null,
  topBarShowAppTitle: true,
  defaultLoginView: "oidc",
  aboutAppMarkdown:
    "Exosphere is a user-friendly, extensible client for cloud computing. Check out our [README on GitLab](https://gitlab.com/exosphere/exosphere/blob/master/README.md). To ask for help, report a bug, or request a new feature, [create an issue](https://gitlab.com/exosphere/exosphere/issues) on Exosphere's GitLab project. Someone will respond within a day or so. For real-time assistance, try Exosphere chat. Our chat is on [Gitter](https://gitter.im/exosphere-app/community) and [Matrix](https://matrix.to/#/#exosphere:matrix.org). The chat is bridged across both platforms, so join whichever you prefer.\\\n\\\nUse of this site is subject to the Exosphere hosted sites [Privacy Policy](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/privacy-policy.md) and [Acceptable Use Policy](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/acceptable-use-policy.md).",
  supportInfoMarkdown: null,
  userSupportEmailAddress:
    "incoming+exosphere-exosphere-6891229-issue-@incoming.gitlab.com",
  userSupportEmailSubject: null,
  openIdConnectLoginConfig: {
    keystoneAuthUrl: "https://js2.jetstream-cloud.org:5000/identity/v3",
    webssoKeystoneEndpoint:
      "/auth/OS-FEDERATION/websso/openid?origin=https://try-dev.exosphere.app/exosphere/oidc-redirector",
    oidcLoginIcon: "assets/img/access-logo.jpg",
    oidcLoginButtonLabel: "Add ACCESS Account",
    oidcLoginButtonDescription: "Jetstream 2 only",
  },
  localization: null,
  instanceConfigMgtRepoUrl: null,
  instanceConfigMgtRepoCheckout: "dev",
  sentryConfig: {
    dsnPublicKey: "2c1487a758db4414b30ea690ab46b338",
    dsnHost: "o1143942.ingest.sentry.io",
    dsnProjectId: "6205105",
    releaseVersion: "latest",
    environmentName: "try-dev.exosphere.app",
  },
};

/* Matomo tracking code */
var _paq = (window._paq = window._paq || []);
/* tracker methods like "setCustomDimension" should be called before "trackPageView" */
_paq.push(["trackPageView"]);
_paq.push(["enableLinkTracking"]);
(function () {
  var u = "//matomo.exosphere.app/";
  _paq.push(["setTrackerUrl", u + "matomo.php"]);
  _paq.push(["setSiteId", "1"]);
  var d = document,
    g = d.createElement("script"),
    s = d.getElementsByTagName("script")[0];
  g.type = "text/javascript";
  g.async = true;
  g.src = u + "matomo.js";
  // @ts-ignore
  s.parentNode.insertBefore(g, s);
})();
