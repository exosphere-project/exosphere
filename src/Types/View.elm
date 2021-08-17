module Types.View exposing
    ( AllResourcesListViewParams
    , DeleteConfirmation
    , IPInfoLevel(..)
    , ImageListViewParams
    , ImageListVisibilityFilter
    , LoginView(..)
    , NonProjectViewConstructor(..)
    , PasswordVisibility(..)
    , ProjectViewConstructor(..)
    , ProjectViewParams
    , ServerDetailViewParams
    , ServerListViewParams
    , ServerSelection
    , SortTableParams
    , VerboseStatus
    , ViewState(..)
    )

import OpenStack.Types as OSTypes
import Page.FloatingIpAssign
import Page.FloatingIpList
import Page.GetSupport
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginJetstream
import Page.LoginOpenstack
import Page.MessageLog
import Page.SelectProjects
import Page.VolumeAttach
import Page.VolumeCreate
import Page.VolumeDetail
import Page.VolumeList
import Set
import Types.HelperTypes as HelperTypes
import Types.Interaction exposing (Interaction)



-- Types that describe view state. After the Legacy Views are migrated to pages, we may want to move these to OuterModel.


type ViewState
    = NonProjectView NonProjectViewConstructor
    | ProjectView HelperTypes.ProjectIdentifier ProjectViewParams ProjectViewConstructor


type NonProjectViewConstructor
    = LoginPicker
    | Login LoginView
    | LoadingUnscopedProjects OSTypes.AuthTokenString
    | SelectProjects Page.SelectProjects.Model
    | MessageLog Page.MessageLog.Model
    | Settings
    | GetSupport Page.GetSupport.Model
    | HelpAbout
    | PageNotFound


type LoginView
    = LoginOpenstack Page.LoginOpenstack.Model
    | LoginJetstream Page.LoginJetstream.Model


type
    ProjectViewConstructor
    -- TODO order these
    = AllResources AllResourcesListViewParams
    | ListImages ImageListViewParams SortTableParams
    | ListProjectServers ServerListViewParams
    | ServerDetail OSTypes.ServerUuid ServerDetailViewParams
    | CreateServer HelperTypes.CreateServerViewParams
    | CreateServerImage OSTypes.ServerUuid String
    | VolumeList Page.VolumeList.Model
    | VolumeDetail Page.VolumeDetail.Model
    | VolumeCreate Page.VolumeCreate.Model
    | VolumeAttach Page.VolumeAttach.Model
    | MountVolInstructions OSTypes.VolumeAttachment
    | FloatingIpList Page.FloatingIpList.Model
    | FloatingIpAssign Page.FloatingIpAssign.Model
    | KeypairList Page.KeypairList.Model
    | KeypairCreate Page.KeypairCreate.Model



-- Everything below will be moved to page-specific models as Legacy Views are migrated over.
-- Model for image list view


type alias ImageListViewParams =
    { searchText : String
    , tags : Set.Set String
    , onlyOwnImages : Bool
    , expandImageDetails : Set.Set OSTypes.ImageUuid
    , visibilityFilter : ImageListVisibilityFilter
    }


type alias ImageListVisibilityFilter =
    { public : Bool
    , community : Bool
    , shared : Bool
    , private : Bool
    }


type alias SortTableParams =
    { title : String
    , asc : Bool
    }



-- Model for any project-specific view


type alias ProjectViewParams =
    { createPopup : Bool
    }



-- Model for all resources view


type alias AllResourcesListViewParams =
    { serverListViewParams : ServerListViewParams
    , volumeListViewParams : Page.VolumeList.Model
    , keypairListViewParams : Page.KeypairList.Model
    , floatingIpListViewParams : Page.FloatingIpList.Model
    }



-- Model for instance list view


type alias ServerListViewParams =
    { onlyOwnServers : Bool
    , selectedServers : Set.Set ServerSelection
    , deleteConfirmations : List DeleteConfirmation
    }


type alias ServerSelection =
    OSTypes.ServerUuid


type alias DeleteConfirmation =
    OSTypes.ServerUuid



-- Model for instance details view


type alias ServerDetailViewParams =
    { showCreatedTimeToggleTip : Bool
    , verboseStatus : VerboseStatus
    , passwordVisibility : PasswordVisibility
    , ipInfoLevel : IPInfoLevel
    , serverActionNamePendingConfirmation : Maybe String
    , serverNamePendingConfirmation : Maybe String
    , activeInteractionToggleTip : Maybe Interaction
    , retainFloatingIpsWhenDeleting : Bool
    }


type IPInfoLevel
    = IPDetails
    | IPSummary


type alias VerboseStatus =
    Bool


type PasswordVisibility
    = PasswordShown
    | PasswordHidden
