module Types.Defaults exposing
    ( allResourcesListViewParams
    , assignFloatingIpViewParams
    , createServerViewParams
    , createVolumeView
    , floatingIpListViewParams
    , imageListViewParams
    , jetstreamCreds
    , keypairListViewParams
    , localization
    , openStackLoginViewParams
    , openstackCreds
    , projectViewParams
    , serverDetailViewParams
    , serverListViewParams
    , sortTableParams
    , volumeListViewParams
    )

import OpenStack.Types as OSTypes
import ServerDeploy exposing (cloudInitUserDataTemplate)
import Set
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Types.Types as Types


localization : Types.Localization
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


openStackLoginViewParams : Types.OpenstackLoginViewParams
openStackLoginViewParams =
    { creds = openstackCreds
    , openRc = ""
    , formEntryType = Types.LoginViewCredsEntry
    }


openstackCreds : OSTypes.OpenstackLogin
openstackCreds =
    { authUrl = ""
    , userDomain = ""
    , username = ""
    , password = ""
    }


jetstreamCreds : Types.JetstreamCreds
jetstreamCreds =
    { jetstreamProviderChoice = Types.BothJetstreamClouds
    , taccUsername = ""
    , taccPassword = ""
    }


projectViewParams : Types.ProjectViewParams
projectViewParams =
    { createPopup = False }


imageListViewParams : Types.ImageListViewParams
imageListViewParams =
    { searchText = ""
    , tags = Set.empty
    , onlyOwnImages = False
    , expandImageDetails = Set.empty
    }


sortTableParams : Types.SortTableParams
sortTableParams =
    { title = ""
    , asc = True
    }


allResourcesListViewParams : Types.AllResourcesListViewParams
allResourcesListViewParams =
    { serverListViewParams = serverListViewParams
    , volumeListViewParams = volumeListViewParams
    , keypairListViewParams = keypairListViewParams
    , floatingIpListViewParams = floatingIpListViewParams
    }


serverListViewParams : Types.ServerListViewParams
serverListViewParams =
    { onlyOwnServers = True
    , selectedServers = Set.empty
    , deleteConfirmations = []
    }


serverDetailViewParams : Types.ServerDetailViewParams
serverDetailViewParams =
    { showCreatedTimeTooltip = False
    , verboseStatus = False
    , passwordVisibility = Types.PasswordHidden
    , ipInfoLevel = Types.IPSummary
    , serverActionNamePendingConfirmation = Nothing
    , serverNamePendingConfirmation = Nothing
    , activeTooltip = Nothing
    , retainFloatingIpsWhenDeleting = False
    }


createServerViewParams : String -> String -> Maybe Bool -> Types.CreateServerViewParams
createServerViewParams imageUuid imageName deployGuacamole =
    { serverName = imageName
    , imageUuid = imageUuid
    , imageName = imageName
    , count = 1
    , flavorUuid = ""
    , volSizeTextInput = Nothing
    , userDataTemplate = cloudInitUserDataTemplate
    , networkUuid = Nothing
    , showAdvancedOptions = False
    , keypairName = Nothing
    , deployGuacamole = deployGuacamole
    , deployDesktopEnvironment = False
    , installOperatingSystemUpdates = True
    , floatingIpCreationOption = Types.Automatic
    }


createVolumeView : Types.ProjectViewConstructor
createVolumeView =
    Types.CreateVolume "" (ValidNumericTextInput 10)


volumeListViewParams : Types.VolumeListViewParams
volumeListViewParams =
    Types.VolumeListViewParams [] []


floatingIpListViewParams : Types.FloatingIpListViewParams
floatingIpListViewParams =
    Types.FloatingIpListViewParams [] True


assignFloatingIpViewParams : Types.AssignFloatingIpViewParams
assignFloatingIpViewParams =
    Types.AssignFloatingIpViewParams Nothing Nothing


keypairListViewParams : Types.KeypairListViewParams
keypairListViewParams =
    Types.KeypairListViewParams [] []
