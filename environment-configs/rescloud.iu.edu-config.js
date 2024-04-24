"use strict";

/** @type {Exosphere.Configuration} */
var config = {
  showDebugMsgs: false,
  cloudCorsProxyUrl: "https://rescloud.iu.edu/proxy",
  urlPathPrefix: "exosphere",
  logo: null,
  favicon: null,
  appTitle: "Exosphere for Rescloud",
  topBarShowAppTitle: true,
  defaultLoginView: "oidc",
  aboutAppMarkdown:
    "This is the Exosphere interface for the IU Research Cloud. If you require assistance, please email rci@iu.edu and specify you are using rescloud.iu.edu.\\\n\\\nUse of this site is subject to the Exosphere hosted sites [Privacy Policy](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/privacy-policy.md) and [Acceptable Use Policy](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/acceptable-use-policy.md).",
  supportInfoMarkdown:
    "See the [documentation](https://TODO) to learn about using Exosphere with ResCloud.\\\n\\\nIn particular, please read about [instance management actions](https://TODO) or [troubleshooting](https://TODO) for answers to common problems before submitting a request to support staff.",
  userSupportEmailAddress: "rci@iu.edu",
  userSupportEmailSubject:
    "[Rescloud] Support Request From Exosphere for Rescloud",
  openIdConnectLoginConfig: {
    keystoneAuthUrl: "https://rci.uits.iu.edu:5000/identity/v3",
    webssoKeystoneEndpoint:
      "/auth/OS-FEDERATION/websso/openid?origin=https://rescloud.iu.edu/exosphere/oidc-redirector",
    oidcLoginIcon: "assets/img/iu-logo.png",
    oidcLoginButtonLabel: "IU Login",
    oidcLoginButtonDescription: "Recommended login method for Rescloud",
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
    share: "share",
    accessRule: "share rule",
    exportLocation: "export location",
    nonFloatingIpAddress: "internal IP address",
    floatingIpAddress: "public IP address",
    publiclyRoutableIpAddress: "public IP address",
    securityGroup: "security group",
    graphicalDesktopEnvironment: "web desktop",
    hostname: "hostname",
    credential: "credential",
  },
  instanceConfigMgtRepoUrl: null,
  instanceConfigMgtRepoCheckout: null,
  sentryConfig: null,
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
};
