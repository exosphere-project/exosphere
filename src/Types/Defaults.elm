module Types.Defaults exposing
    ( localization
    , projectViewParams
    )

import Types.HelperTypes as HelperTypes
import Types.View as ViewTypes


localization : HelperTypes.Localization
localization =
    { openstackWithOwnKeystone = "cloud"
    , openstackSharingKeystoneWithAnother = "region"
    , unitOfTenancy = "project"
    , maxResourcesPerProject = "resource limits"
    , pkiPublicKeyForSsh = "SSH public key"
    , virtualComputer = "instance"
    , virtualComputerHardwareConfig = "size"
    , cloudInitData = "boot script"
    , commandDrivenTextInterface = "terminal"
    , staticRepresentationOfBlockDeviceContents = "image"
    , blockDevice = "volume"
    , nonFloatingIpAddress = "internal IP address"
    , floatingIpAddress = "floating IP address"
    , publiclyRoutableIpAddress = "public IP address"
    , graphicalDesktopEnvironment = "graphical desktop"
    }



-- Most of the code below should become page-specific init functions as legacy views are migrated to pages


projectViewParams : ViewTypes.ProjectViewParams
projectViewParams =
    { createPopup = False }
