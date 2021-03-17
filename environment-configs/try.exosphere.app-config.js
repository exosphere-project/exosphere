'use strict'

var config = {
  showDebugMsgs: false,
  cloudCorsProxyUrl: "https://try.exosphere.app/proxy",
  urlPathPrefix: "exosphere",
  palette: null,
  logo: null,
  favicon: null,
  appTitle: null,
  defaultLoginView: null,
  aboutAppMarkdown: "Exosphere is a user-friendly, extensible client for cloud computing. Check out our [README on GitLab](https://gitlab.com/exosphere/exosphere/blob/master/README.md). To ask for help, report a bug, or request a new feature, [create an issue](https://gitlab.com/exosphere/exosphere/issues) on Exosphere's GitLab project. Someone will respond within a day or so. For real-time assistance, try Exosphere chat. Our chat is on [Gitter](https://gitter.im/exosphere-app/community) and [Matrix via Element](https://riot.im/app/#/room/#exosphere:matrix.org). The chat is bridged across both platforms, so join whichever you prefer.\\\n\\\nUse of this site is subject to the Exosphere hosted sites [Privacy Policy](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/privacy-policy.md) and [Acceptable Use Policy](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/acceptable-use-policy.md).",
  supportInfoMarkdown: null,
  userSupportEmail: "incoming+exosphere-exosphere-6891229-issue-@incoming.gitlab.com",
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

/* Matomo tracking code */
var _paq = window._paq = window._paq || [];
/* tracker methods like "setCustomDimension" should be called before "trackPageView" */
_paq.push(['trackPageView']);
_paq.push(['enableLinkTracking']);
(function () {
  var u = "//matomo.exosphere.app/";
  _paq.push(['setTrackerUrl', u + 'matomo.php']);
  _paq.push(['setSiteId', '2']);
  var d = document, g = d.createElement('script'), s = d.getElementsByTagName('script')[0];
  g.type = 'text/javascript';
  g.async = true;
  g.src = u + 'matomo.js';
  s.parentNode.insertBefore(g, s);
})();

