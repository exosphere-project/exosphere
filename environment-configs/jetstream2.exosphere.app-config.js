"use strict";

var config = {
  showDebugMsgs: false,
  cloudCorsProxyUrl: "https://jetstream2.exosphere.app/proxy",
  urlPathPrefix: "exosphere",
  palette: {
    light: {
      primary: {
        r: 150,
        g: 35,
        b: 38,
      },
      secondary: {
        r: 0,
        g: 0,
        b: 0,
      },
    },
    dark: {
      primary: {
        r: 255,
        g: 70,
        b: 95,
      },
      secondary: {
        r: 0,
        g: 0,
        b: 0,
      },
    },
  },
  logo: "assets/img/jetstream2-logo-white.svg",
  favicon: "assets/img/jetstream2-favicon.ico",
  appTitle: "Exosphere for Jetstream2",
  topBarShowAppTitle: false,
  defaultLoginView: "oidc",
  aboutAppMarkdown:
    "This is the Exosphere interface for [Jetstream2](https://jetstream-cloud.org), currently in beta. If you require assistance, please email help@jetstream-cloud.org and specify you are using jetstream2.exosphere.app.\\\n\\\nUse of this site is subject to the Exosphere hosted sites [Privacy Policy](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/privacy-policy.md) and [Acceptable Use Policy](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/acceptable-use-policy.md).",
  supportInfoMarkdown:
    "Please read about [instance management actions](https://docs.jetstream-cloud.org/general/instancemgt/) or [troubleshooting](https://docs.jetstream-cloud.org/faq/trouble/) for answers to common problems before submitting a request to support staff.",
  userSupportEmailAddress: "help@jetstream-cloud.org",
  openIdConnectLoginConfig: {
    keystoneAuthUrl: "https://js2.jetstream-cloud.org:5000/identity/v3",
    webssoKeystoneEndpoint:
      "/auth/OS-FEDERATION/websso/openid?origin=https://jetstream2.exosphere.app/exosphere/oidc-redirector",
    oidcLoginIcon: "assets/img/access-logo.jpg",
    oidcLoginButtonLabel: "Add ACCESS Account",
    oidcLoginButtonDescription: "Recommended login method for Jetstream2",
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
    graphicalDesktopEnvironment: "web desktop",
  },
  instanceConfigMgtRepoUrl: null,
  instanceConfigMgtRepoCheckout: null,
  sentryConfig: {
    dsnPublicKey: "2c1487a758db4414b30ea690ab46b338",
    dsnHost: "o1143942.ingest.sentry.io",
    dsnProjectId: "6205105",
    releaseVersion: "latest",
    environmentName: "jetstream2.exosphere.app",
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
  _paq.push(["setSiteId", "4"]);
  var d = document,
    g = d.createElement("script"),
    s = d.getElementsByTagName("script")[0];
  g.type = "text/javascript";
  g.async = true;
  g.src = u + "matomo.js";
  s.parentNode.insertBefore(g, s);
})();
