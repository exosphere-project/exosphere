module Types.Defaults exposing
    ( allResourcesListViewParams
    , createServerViewParams
    , imageListViewParams
    , localization
    , projectViewParams
    , serverListViewParams
    , sortTableParams
    )

import Page.FloatingIpList
import Page.KeypairList
import Page.VolumeList
import ServerDeploy exposing (cloudInitUserDataTemplate)
import Set
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
    , volumeListViewParams = Page.VolumeList.init
    , keypairListViewParams = Page.KeypairList.init
    , floatingIpListViewParams = Page.FloatingIpList.init
    }


serverListViewParams : ViewTypes.ServerListViewParams
serverListViewParams =
    { onlyOwnServers = True
    , selectedServers = Set.empty
    , deleteConfirmations = []
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
