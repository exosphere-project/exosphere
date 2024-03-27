module Types.Defaults exposing (localization)

import Types.HelperTypes as HelperTypes


localization : HelperTypes.Localization
localization =
    { openstackWithOwnKeystone = "cloud"
    , openstackSharingKeystoneWithAnother = "region"
    , unitOfTenancy = "project"
    , maxResourcesPerProject = "resource limit"
    , pkiPublicKeyForSsh = "SSH public key"
    , virtualComputer = "instance"
    , virtualComputerHardwareConfig = "size"
    , cloudInitData = "boot script"
    , commandDrivenTextInterface = "terminal"
    , staticRepresentationOfBlockDeviceContents = "image"
    , blockDevice = "volume"
    , share = "share"
    , accessRule = "access rule"
    , exportLocation = "export location"
    , nonFloatingIpAddress = "internal IP address"
    , floatingIpAddress = "floating IP address"
    , publiclyRoutableIpAddress = "public IP address"
    , securityGroup = "security group"
    , graphicalDesktopEnvironment = "graphical desktop"
    , hostname = "hostname"
    , credential = "credential"
    }
