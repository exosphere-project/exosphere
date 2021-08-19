module Types.Defaults exposing
    ( allResourcesListViewParams
    , imageListViewParams
    , localization
    , projectViewParams
    , sortTableParams
    )

import Page.FloatingIpList
import Page.KeypairList
import Page.ServerList
import Page.VolumeList
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
    { serverListViewParams = Page.ServerList.init
    , volumeListViewParams = Page.VolumeList.init
    , keypairListViewParams = Page.KeypairList.init
    , floatingIpListViewParams = Page.FloatingIpList.init
    }
