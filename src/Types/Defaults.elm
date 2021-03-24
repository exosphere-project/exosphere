module Types.Defaults exposing
    ( allResourcesListViewParams
    , createServerViewParams
    , createVolumeView
    , imageListViewParams
    , jetstreamCreds
    , keypairListViewParams
    , localization
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
    , floatingIpAddress = "public IP address"
    , graphicalDesktopEnvironment = "graphical desktop environment"
    }


openstackCreds : OSTypes.OpenstackLogin
openstackCreds =
    { authUrl = ""
    , projectDomain = ""
    , projectName = ""
    , userDomain = ""
    , username = ""
    , password = ""
    }


jetstreamCreds : Types.JetstreamCreds
jetstreamCreds =
    { jetstreamProviderChoice = Types.BothJetstreamClouds
    , jetstreamProjectName = ""
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
    }


serverListViewParams : Types.ServerListViewParams
serverListViewParams =
    { onlyOwnServers = True
    , selectedServers = Set.empty
    , deleteConfirmations = []
    }


serverDetailViewParams : Types.ServerDetailViewParams
serverDetailViewParams =
    { verboseStatus = False
    , passwordVisibility = Types.PasswordHidden
    , ipInfoLevel = Types.IPSummary
    , serverActionNamePendingConfirmation = Nothing
    , serverNamePendingConfirmation = Nothing
    , activeTooltip = Nothing
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
    , networkUuid = ""
    , showAdvancedOptions = False
    , keypairName = Nothing
    , deployGuacamole = deployGuacamole
    , deployDesktopEnvironment = False
    }


createVolumeView : Types.ProjectViewConstructor
createVolumeView =
    Types.CreateVolume "" (ValidNumericTextInput 10)


volumeListViewParams : Types.VolumeListViewParams
volumeListViewParams =
    Types.VolumeListViewParams [] []


keypairListViewParams : Types.KeypairListViewParams
keypairListViewParams =
    Types.KeypairListViewParams [] []
