# Configuration Options

These options are primarily intended for cloud operators who wish to offer a customized deployment of Exosphere to their user community. Set these in `config.js`.

| *Option*                      | *Possible Values*       | *Description*                                                          |
|-------------------------------|-------------------------|------------------------------------------------------------------------|
| showDebugMsgs                 | false, true             |                                                                        |
| cloudCorsProxyUrl             | null, string            | See `docs/solving-cors-problem.md`; required to use app in web browser |
| clouds                        | array                   | Imported from `cloud_configs.js`; see example below                    |
| palette                       | null, JSON object       | Pass custom colors to style Exosphere, see example below               |
| logo                          | null, string            | Path to custom logo to show in top-left corner of app                  |
| favicon                       | null, string            | Path to custom favicon                                                 |
| appTitle                      | null, string            | Title to show throughout the app                                       |
| topBarShowAppTitle            | true (default), false   | Whether to show or hide appTitle in the top navigation bar             |
| defaultLoginView              | null, openstack, oidc   | Which login view to display by default                                 |
| aboutAppMarkdown              | null, string (markdown) | What to show in the "About the app" section of Help/About view         |
| supportInfoMarkdown           | null, string (markdown) | What to show when user clicks "Get support" button                     |
| userSupportEmailAddress       | null, string (markdown) | Email address to ask users to send problem report                      |
| userSupportEmailSubject       | null, string            | Text to include in subject line of support request email               |
| openIdConnectLoginConfig      | null, JSON object       | See `docs/federated-login.md` for more info and example JSON           |
| localization                  | null, JSON object       | Pass custom localization strings for the UI, see example below         |
| instanceConfigMgtRepoUrl      | null, string            | Set a custom repository to use for instance setup code                 |
| instanceConfigMgtRepoCheckout | null, string            | Check out specific branch/tag/commit of instance setup code            |
| bannersUrl                    | null, string            | Customizable URL for loading banners, see example below                |
| sentryConfig                  | null, JSON object       | Pass Sentry DSN for error logging, see example below                   |

## Example cloud configuration

The `clouds` flag is an array containing JSON objects for each cloud with a custom configuration.

By default, the `clouds` flag is imported from `cloud_configs.js`. As a deployer, you can add your own cloud(s) to that file, or override it entirely by defining a `clouds` member of the `config` object in `config.js`.

Each of these JSON objects contains the following properties:

- `keystoneHostname` (string): Used to look up the custom configuration for a cloud, e.g. `openstack.example.cloud`
- `friendlyName` (string): Name of cloud to display to user
- `userAppProxy` (null, array): An array of User Application proxy (UAP) information for this cloud. See `docs/user-app-proxy.md` for more information. This _must_ be set for Guacamole support (in-browser shell and desktop) to work on a given cloud.
- `imageExcludeFilter` (null, JSON object): A key:value property to exclude images from UI, see example below
- `featuredImageNamePrefix` (null, string): A (public) image is 'featured' if the name starts with this string
- `instanceTypes` (array): An array of instance types specific to this cloud, can be left empty. See `docs/instance-types.md` for more information.
- `flavorGroups` (array): An array of flavor groups specific to this cloud, can be left empty. See `docs/flavor-groups.md` for more information.
- `desktopMessage` (null, string): Override message to show users who select a graphical desktop environment when creating an instance. `null` will display a default message, while an empty string will display no message.
- `securityGroups` (null, JSON object): A map of default Security Groups for each cloud region, using `noRegion` as a fallback. See `docs/security-groups.md` for more information.

```javascript
var cloud_configs = {
  "clouds":[
    {
      "keystoneHostname":"openstack.example.cloud",
      "friendlyName":"My Example Cloud 1",
      "userAppProxy":[
        { region: null,
          hostname: "uap.openstack.example.cloud",
        },
      ]
      "imageExcludeFilter":null,
      "featuredImageNamePrefix":null,
      "instanceTypes":[
        
      ]
    },
    {
      "keystoneHostname":"iu.jetstream-cloud.org",
      "friendlyName":"Jetstream Cloud",      
      "userAppProxy": [
        { region: null,
          hostname: "proxy-j7m-iu.exosphere.app",
        },
      ]
      "imageExcludeFilter":{
        "filterKey":"atmo_image_include",
        "filterValue":"true"
      },
      "featuredImageNamePrefix":"JS-API-Featured",
      "instanceTypes":[
        {
          "friendlyName":"Ubuntu",
          "description":"Wide compatibility with community software packages, good choice for new users",
          "logo":"assets/img/ubuntu.svg",
          "versions":[
            {
              "friendlyName":"20.04 (latest)",
              "isPrimary":true,
              "imageFilters":{
                "name":"JS-API-Featured-Ubuntu20-Latest",
                "visibility":"public"
              },
              "restrictFlavorIds":null
            }
          ]
        }
      ],
      "flavorGroups":[
        {
          "matchOn":"m1\..*",
          "title":"General-purpose",
          "description":null,
          "disallowedActions":[],
        },
        {
          "matchOn":"g1\..*",
          "title":"GPU",
          "description":"These have a graphics processing unit.",
          "disallowedActions":["Suspend"],
        }        
      ],
      "desktopMessage":null,
      "securityGroups":{
        "noRegion": {
          "description": "Allow all traffic",
          "name": "permissive",
          "rules": [
            {
              "description": "Mosh",
              "direction": "ingress",
              "ethertype": "IPv4",
              "port_range_max": 61000,
              "port_range_min": 60000,
              "protocol": "udp",
              "remote_group_id": null,
              "remote_ip_prefix": "0.0.0.0/0"
            },
            {
              "description": "SSH",
              "direction": "ingress",
              "ethertype": "IPv4",
              "port_range_max": 22,
              "port_range_min": 22,
              "protocol": "tcp",
              "remote_group_id": null,
              "remote_ip_prefix": null
            },
            {
              "description": null,
              "direction": "egress",
              "ethertype": "IPv4",
              "port_range_max": null,
              "port_range_min": null,
              "protocol": null,
              "remote_group_id": null,
              "remote_ip_prefix": null
            },
            {
              "description": null,
              "direction": "egress",
              "ethertype": "IPv6",
              "port_range_max": null,
              "port_range_min": null,
              "protocol": null,
              "remote_group_id": null,
              "remote_ip_prefix": null
            },
            {
              "description": "Ping",
              "direction": "ingress",
              "ethertype": "IPv4",
              "port_range_max": null,
              "port_range_min": null,
              "protocol": "icmp",
              "remote_group_id": null,
              "remote_ip_prefix": null
            },
            {
              "description": "Expose all incoming ports",
              "direction": "ingress",
              "ethertype": "IPv4",
              "port_range_max": null,
              "port_range_min": null,
              "protocol": "tcp",
              "remote_group_id": null,
              "remote_ip_prefix": null
            }
          ]
        },
        "IU": {
          "name": "restrictive",
          "description": "Only allow SSH",
          "rules": [
            {
              "description": "SSH",
              "direction": "ingress",
              "ethertype": "IPv4",
              "port_range_max": 22,
              "port_range_min": 22,
              "protocol": "tcp",
              "remote_group_id": null,
              "remote_ip_prefix": null
            }
          ]
        }
      }
    }
  ]
}
```

### Example Image Exclude Filter

This excludes images built by, and intended for the Atmosphere platform.

```
  imageExcludeFilter: {
    filterKey: "atmo_image_include",
    filterValue: "true"
  }
```

## Example Custom Palette

This declares a primary and secondary color in the app, for each of dark and light modes.

```
palette: {
  "light": {
    "primary": {
      "r": 150,
      "g": 35,
      "b": 38
    },
    "secondary": {
      "r": 0,
      "g": 0,
      "b": 0
    }
  },
  "dark": {
    "primary": {
      "r": 221,
      "g": 0,
      "b": 49
    },
    "secondary": {
      "r": 0,
      "g": 0,
      "b": 0
    }
  }
} 
```

## Example Localization JSON object

This allows a deployer to customize terms used by Exosphere for their organization or community.

```
localization: {
    openstackWithOwnKeystone: "cloud",
    openstackSharingKeystoneWithAnother: "region",
    unitOfTenancy: "project",
    maxResourcesPerProject: "resource limits",
    pkiPublicKeyForSsh: "SSH public key",
    virtualComputer: "instance",
    virtualComputerHardwareConfig: "size",
    cloudInitData: "boot script",
    commandDrivenTextInterface: "terminal",
    staticRepresentationOfBlockDeviceContents: "image",
    blockDevice: "volume",
    share: "share",
    accessRule: "access rule",
    exportLocation: "export location",
    nonFloatingIpAddress: "internal IP address",
    floatingIpAddress: "floating IP address",
    publiclyRoutableIpAddress: "public IP address",
    securityGroup: "firewall ruleset",
    graphicalDesktopEnvironment: "graphical desktop environment",
    hostname: "hostname",
    credentials: "credentials"
}
```

## Example Banners Configuration

Banners are deployed through an optional JSON file, allowing deployers to show and update banners in the Exosphere interface. This JSON file will be polled by the Exosphere client.

In your `config.js`, you can change `bannersUrl` to use a custom endpoint (any URL which returns a valid `Banners` JSON response). If you leave this set to null, it will default to `banners.json`.

```
"bannersUrl": "/banners.json",
```

Banners may be configured with both start and end times to display banners during a certain time period, as well as a `level` to adjust the color and icon shown along with a banner. 

![Banners Example (light)](assets/banners-light.png) ![Banners Example (dark)](assets/banners-dark.png)

Banner messages are parsed as markdown, allowing rich links and formatting in your notifications.

| *Option*            | *Possible Values*       | *Description*                                                                                                           |
|---------------------|-------------------------|-------------------------------------------------------------------------------------------------------------------------|
| message             | string (markdown)       | The message to display to the user, parsed as markdown                                                                  |
| level (optional)    | string (banner)         | One of "default", "info", "success", "warning", or "danger"                                                             |
| startsAt (optional) | string (date and time)  | A date and time, such as "2024-05-28T13:00:00−05:00", formatted using [ISO8601](https://en.wikipedia.org/wiki/ISO_8601) |
| endsAt (optional)   | string (date and time)  | A date and time, such as "2024-05-28T15:00:00−05:00", formatted using [ISO8601](https://en.wikipedia.org/wiki/ISO_8601) |

The example shown here would show a first banner for a pre-maintenance warning, a second banner during a maintenance period, and a third banner when maintenance is complete.

```json
[
  {
    "message": "Maintenance period begins on 5/28/2024, at 1:00pm Eastern. Some functionality may be degraded", 
    "level": "info", 
    "endsAt": "2024-05-28T13:00:00-05:00" 
  },
  {
    "message": "This Cloud is under Maintenance until 5/28/2024, 3:00pm Eastern. Some functionality may be degraded", 
    "level": "warning",
    "startsAt": "2024-05-28T13:00:00-05:00", 
    "endsAt": "2024-05-28T15:00:00-05:00" 
  },
  {
    "message": "Maintenance has been completed. Please notify support of any found issues", 
    "level": "success",
    "startsAt": "2024-05-28T15:00:00-05:00", 
    "endsAt": "2024-05-30T00:00:00-05:00" 
  },
]
```

## Example Sentry Configuration

[Here](https://package.elm-lang.org/packages/romariolopezc/elm-sentry/latest/Sentry#config) are instructions for determining the DSN fields.

```
"sentryConfig":{
  "dsnPublicKey":"1900942c246350fdacb4c9369cac2ets",
  "dsnHost":"o298593.ingest.sentry.io",
  "dsnProjectId":"2312456",
  "releaseVersion":"latest",
  "environmentName":"prod"
}
```

## Other Services

See [Other Services](./other-services.md) for how to integrate services other than Sentry.
