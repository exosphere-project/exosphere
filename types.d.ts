declare namespace Exosphere {
  type RGB = {
    r: number;
    g: number;
    b: number;
  };

  type Palette = {
    primary: RGB;
    secondary: RGB;
  };

  type Theme = {
    light: Palette;
    dark: Palette;
  };

  type SentryConfig = {
    dsnPublicKey: string;
    dsnHost: string;
    dsnProjectId: string;
    releaseVersion: string;
    environmentName: string;
  };

  type OpenIdConnectLoginConfig = {
    keystoneAuthUrl: string;
    webssoUrl: string;
    oidcLoginIcon: string;
    oidcLoginButtonLabel: string;
    oidcLoginButtonDescription: string;
  };

  type Localization = {
    openstackWithOwnKeystone: "cloud" | string;
    openstackSharingKeystoneWithAnother: "region" | string;
    unitOfTenancy: "project" | string;
    maxResourcesPerProject: "resource limit" | string;
    pkiPublicKeyForSsh: "SSH public key" | string;
    virtualComputer: "instance" | string;
    virtualComputerHardwareConfig: "size" | string;
    cloudInitData: "boot script" | string;
    commandDrivenTextInterface: "terminal" | string;
    staticRepresentationOfBlockDeviceContents: "image" | string;
    blockDevice: "volume" | string;
    share: "share" | string;
    accessRule: "access rule" | string;
    exportLocation: "export location" | string;
    nonFloatingIpAddress: "internal IP address" | string;
    floatingIpAddress: "floating IP address" | string;
    publiclyRoutableIpAddress: "public IP address" | string;
    securityGroup: "firewall ruleset" | string;
    graphicalDesktopEnvironment: "graphical desktop" | string;
    hostname: "hostname" | string;
    credential: "credential" | string;
  };

  type CloudUserApplicationProxy = {
    region: null | string;
    hostname: string;
  };

  type CloudMetadataFilter = {
    filterKey: string;
    filterValue: string;
  };

  type CloudInstanceType = {
    friendlyName: string;
    description: string;
    logo: string;
    versions: Array<{
      friendlyName: string;
      isPrimary: boolean;
      imageFilters: {
        name?: string;
        uuid?: string;
        visibility?: "private" | "shared" | "community" | "public";
        osDistro?: string;
        osVersion?: string;
        metadata?: CloudMetadataFilter;
      };
      restrictFlavorIds: null | Array<string>;
    }>;
  };

  type ServerActions =
    | "Confirm"
    | "Revert"
    | "Lock"
    | "Unlock"
    | "Start"
    | "Unpause"
    | "Resume"
    | "Unshelve"
    | "Suspend"
    | "Shelve"
    | "Resize"
    | "Reboot"
    | "Delete"
    | "Pause"
    | "Stop";

  type CloudFlavorGroup = {
    matchOn: string;
    title: string;
    description: null | string;
    disallowedActions: Array<ServerActions | string>;
  };

  type CloudSecurityGroup = {
    name: string;
    description: null | string;
    region?: null | string;
    rules: Array<{
      ethertype: string;
      direction: string;
      protocol?: null | string;
      port_range_min?: null | number;
      port_range_max?: null | number;
      remote_ip_prefix?: null | string;
      remote_group_id?: null | string;
      description?: null | string;
    }>;
  };

  type CloudConfig = {
    keystoneHostname: string;
    friendlyName: string;
    userAppProxy: null | Array<CloudUserApplicationProxy>;

    imageExcludeFilter: null | CloudMetadataFilter;
    featuredImageNamePrefix: null | string;
    instanceTypes: Array<CloudInstanceType>;
    flavorGroups: Array<CloudFlavorGroup>;
    securityGroups?: null | Array<CloudSecurityGroup>;
    desktopMessage: null | string;
  };

  type CloudConfigs = {
    clouds: Array<CloudConfig>;
  };

  type Banner = {
    message: string;
    level?: "default" | "info" | "success" | "warning" | "danger";
    startsAt?: string;
    endsAt?: string;
  };

  type Configuration = {
    showDebugMsgs: boolean;
    cloudCorsProxyUrl: null | string;
    urlPathPrefix: null | string;
    palette: null | Theme;
    topBarShowAppTitle: boolean;

    appTitle: null | string;
    logo: null | string;

    favicon: null | string;
    defaultLoginView: null | string;
    aboutAppMarkdown: null | string;
    supportInfoMarkdown: null | string;
    userSupportEmailAddress: null | string;
    userSupportEmailSubject: null | string;
    instanceConfigMgtRepoUrl: null | string;
    instanceConfigMgtRepoCheckout: null | string;
    sentryConfig: null | SentryConfig;
    openIdConnectLoginConfig: null | OpenIdConnectLoginConfig;
    localization: null | Localization;
    bannersUrl: null | string;
  };
}
