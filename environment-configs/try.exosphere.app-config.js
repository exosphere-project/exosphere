'use strict'

var config = {
showDebugMsgs : false,
cloudCorsProxyUrl: "https://try.exosphere.app/proxy",
cloudsWithUserAppProxy:
[ ["iu.jetstream-cloud.org", "proxy-j7m-iu.exosphere.app"],
  ["tacc.jetstream-cloud.org", "proxy-j7m-tacc.exosphere.app"],
],
urlPathPrefix: "exosphere",
palette: null,
logo: null,
favicon: null,
appTitle: null,
defaultLoginView: null,
aboutAppMarkdown: null,
supportInfoMarkdown: null,
userSupportEmail: "incoming+exosphere-exosphere-6891229-issue-@incoming.gitlab.com"
}

/* Matomo tracking code */
var _paq = window._paq = window._paq || [];
/* tracker methods like "setCustomDimension" should be called before "trackPageView" */
_paq.push(['trackPageView']);
_paq.push(['enableLinkTracking']);
(function() {
  var u="//matomo.exosphere.app/";
  _paq.push(['setTrackerUrl', u+'matomo.php']);
  _paq.push(['setSiteId', '2']);
  var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
  g.type='text/javascript'; g.async=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
})();

