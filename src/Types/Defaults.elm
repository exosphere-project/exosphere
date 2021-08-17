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
    , projectViewParams
    , serverDetailViewParams
    , serverListViewParams
    , sortTableParams
    , volumeListViewParams
    )

import ServerDeploy exposing (cloudInitUserDataTemplate)
import Set
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
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


jetstreamCreds : HelperTypes.JetstreamCreds
jetstreamCreds =
    { jetstreamProviderChoice = HelperTypes.BothJetstreamClouds
    , taccUsername = ""
    , taccPassword = ""
    }


projectViewParams : ViewTypes.ProjectViewParams
projectViewParams =
    { createPopup = False }


imageListViewParams : ViewTypes.ImageListViewParams
imageListViewParams =
    { searchText = ""
    , tags = Set.empty
    , onlyOwnImages = False
    , expandImageDetails = Set.empty
    , visibilityFilter = imageListVisibilityFilter
    }


imageListVisibilityFilter : ViewTypes.ImageListVisibilityFilter
imageListVisibilityFilter =
    { public = True
    , community = True
    , shared = True
    , private = True
    }


sortTableParams : ViewTypes.SortTableParams
sortTableParams =
    { title = ""
    , asc = True
    }


allResourcesListViewParams : ViewTypes.AllResourcesListViewParams
allResourcesListViewParams =
    { serverListViewParams = serverListViewParams
    , volumeListViewParams = volumeListViewParams
    , keypairListViewParams = keypairListViewParams
    , floatingIpListViewParams = floatingIpListViewParams
    }


serverListViewParams : ViewTypes.ServerListViewParams
serverListViewParams =
    { onlyOwnServers = True
    , selectedServers = Set.empty
    , deleteConfirmations = []
    }


serverDetailViewParams : ViewTypes.ServerDetailViewParams
serverDetailViewParams =
    { showCreatedTimeToggleTip = False
    , verboseStatus = False
    , passwordVisibility = ViewTypes.PasswordHidden
    , ipInfoLevel = ViewTypes.IPSummary
    , serverActionNamePendingConfirmation = Nothing
    , serverNamePendingConfirmation = Nothing
    , activeInteractionToggleTip = Nothing
    , retainFloatingIpsWhenDeleting = False
    }


createServerViewParams : String -> String -> Maybe Bool -> HelperTypes.CreateServerViewParams
createServerViewParams imageUuid imageName deployGuacamole =
    { serverName = imageName
    , imageUuid = imageUuid
    , imageName = imageName
    , count = 1
    , flavorUuid = ""
    , volSizeTextInput = Nothing
    , userDataTemplate = cloudInitUserDataTemplate
    , networkUuid = Nothing
    , customWorkflowSource = Nothing
    , customWorkflowSourceInput = Nothing
    , showCustomWorkflowOptions = False
    , showAdvancedOptions = False
    , keypairName = Nothing
    , deployGuacamole = deployGuacamole
    , deployDesktopEnvironment = False
    , installOperatingSystemUpdates = True
    , floatingIpCreationOption = HelperTypes.Automatic
    }


createVolumeView : ViewTypes.ProjectViewConstructor
createVolumeView =
    ViewTypes.CreateVolume "" (ValidNumericTextInput 10)


volumeListViewParams : ViewTypes.VolumeListViewParams
volumeListViewParams =
    ViewTypes.VolumeListViewParams [] []


floatingIpListViewParams : ViewTypes.FloatingIpListViewParams
floatingIpListViewParams =
    ViewTypes.FloatingIpListViewParams [] True


assignFloatingIpViewParams : ViewTypes.AssignFloatingIpViewParams
assignFloatingIpViewParams =
    ViewTypes.AssignFloatingIpViewParams Nothing Nothing


keypairListViewParams : ViewTypes.KeypairListViewParams
keypairListViewParams =
    ViewTypes.KeypairListViewParams [] []
