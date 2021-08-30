'use strict'

var config = {
  showDebugMsgs: false,
  cloudCorsProxyUrl: "https://exosphere.jetstream-cloud.org/proxy",
  urlPathPrefix: "exosphere",
  palette: {primary: {r: 155, g: 33, b: 35}, secondary: {r: 52, g: 122, b: 140}},
  logo: "assets/img/jetstream-logo-white.svg",
  favicon: "assets/img/jetstream-favicon.ico",
  appTitle: "Exosphere for Jetstream Cloud",
  topBarShowAppTitle: false,
  defaultLoginView: "jetstream",
  aboutAppMarkdown: "This is the Exosphere interface for [Jetstream Cloud](https://jetstream-cloud.org), currently in beta. If you require assistance, please email help@jetstream-cloud.org and specify you are using Exosphere.\\\n\\\nUse of this site is subject to the Exosphere hosted sites [Privacy Policy](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/privacy-policy.md) and [Acceptable Use Policy](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/acceptable-use-policy.md).",
  supportInfoMarkdown: "Please read about [using instances](https://iujetstream.atlassian.net/wiki/display/JWT/Jetstream+Public+Wiki) or [troubleshooting instances](https://wiki.jetstream-cloud.org/Troubleshooting+and+FAQ) for answers to common problems before submitting a request to support staff.",
  userSupportEmail: "help@jetstream-cloud.org",
  openIdConnectLoginConfig:
    {
      keystoneAuthUrl: "https://iu.jetstream-cloud.org:5000/v3",
      webssoKeystoneEndpoint: "/auth/OS-FEDERATION/websso/openid?origin=https://exosphere.jetstream-cloud.org/exosphere/oidc-redirector",
      oidcLoginIcon: "assets/img/XSEDE_Logo_Black_INF.png",
      oidcLoginButtonLabel: "Add XSEDE Account",
      oidcLoginButtonDescription: "Under construction, may not work, Jetstream IU Cloud only"
    },
  localization: {
    openstackWithOwnKeystone: "cloud",
    openstackSharingKeystoneWithAnother: "region",
    unitOfTenancy: "allocation",
    maxResourcesPerProject: "quota",
    pkiPublicKeyForSsh: "SSH public key",
    virtualComputer: "instance",
    virtualComputerHardwareConfig: "flavor",
    cloudInitData: "boot script",
    commandDrivenTextInterface: "web shell",
    staticRepresentationOfBlockDeviceContents: "image",
    blockDevice: "volume",
    nonFloatingIpAddress: "internal IP address",
    floatingIpAddress: "public IP address",
    publiclyRoutableIpAddress: "public IP address",
    graphicalDesktopEnvironment: "web desktop"
  },
  clouds: [
    {
      keystoneHostname: "iu.jetstream-cloud.org",
      userAppProxy: "proxy-j7m-iu.exosphere.app",
      imageExcludeFilter: {
        filterKey: "atmo_image_include",
        filterValue: "true"
      },
      featuredImageNamePrefix: "JS-API-Featured",
      operatingSystemChoices: []
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

/* Matomo tracking code */
var _paq = window._paq = window._paq || [];
/* tracker methods like "setCustomDimension" should be called before "trackPageView" */
_paq.push(['trackPageView']);
_paq.push(['enableLinkTracking']);
(function () {
  var u = "//matomo.exosphere.app/";
  _paq.push(['setTrackerUrl', u + 'matomo.php']);
  _paq.push(['setSiteId', '3']);
  var d = document, g = d.createElement('script'), s = d.getElementsByTagName('script')[0];
  g.type = 'text/javascript';
  g.async = true;
  g.src = u + 'matomo.js';
  s.parentNode.insertBefore(g, s);
})();
