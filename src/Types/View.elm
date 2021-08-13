module Types.View exposing
    ( AllResourcesListViewParams
    , AssignFloatingIpViewParams
    , DeleteConfirmation
    , DeleteVolumeConfirmation
    , FloatingIpListViewParams
    , IPInfoLevel(..)
    , ImageListViewParams
    , ImageListVisibilityFilter
    , KeypairIdentifier
    , KeypairListViewParams
    , LoginView(..)
    , NonProjectViewConstructor(..)
    , PasswordVisibility(..)
    , ProjectViewConstructor(..)
    , ProjectViewParams
    , ServerDetailViewParams
    , ServerListViewParams
    , ServerSelection
    , SortTableParams
    , SupportableItemType(..)
    , VerboseStatus
    , ViewState(..)
    , VolumeListViewParams
    )

import OpenStack.Types as OSTypes
import Page.LoginOpenstack
import Set
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
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
    | SelectProjects OSTypes.KeystoneUrl (List HelperTypes.UnscopedProviderProject)
    | MessageLog Bool
    | Settings
    | GetSupport (Maybe ( SupportableItemType, Maybe HelperTypes.Uuid )) String Bool
    | HelpAbout
    | PageNotFound


type LoginView
    = LoginOpenstack Page.LoginOpenstack.Model
    | LoginJetstream HelperTypes.JetstreamCreds


type ProjectViewConstructor
    = AllResources AllResourcesListViewParams
    | ListImages ImageListViewParams SortTableParams
    | ListProjectServers ServerListViewParams
    | ListProjectVolumes VolumeListViewParams
    | ListFloatingIps FloatingIpListViewParams
    | AssignFloatingIp AssignFloatingIpViewParams
    | ListKeypairs KeypairListViewParams
    | CreateKeypair String String
    | ServerDetail OSTypes.ServerUuid ServerDetailViewParams
    | CreateServerImage OSTypes.ServerUuid String
    | VolumeDetail OSTypes.VolumeUuid (List DeleteVolumeConfirmation)
    | CreateServer HelperTypes.CreateServerViewParams
    | CreateVolume OSTypes.VolumeName NumericTextInput
    | AttachVolumeModal (Maybe OSTypes.ServerUuid) (Maybe OSTypes.VolumeUuid)
    | MountVolInstructions OSTypes.VolumeAttachment



-- Everything below will be moved to page-specific models as Legacy Views are migrated over.
-- Model for get support view


type SupportableItemType
    = SupportableProject
    | SupportableImage
    | SupportableServer
    | SupportableVolume



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
    , volumeListViewParams : VolumeListViewParams
    , keypairListViewParams : KeypairListViewParams
    , floatingIpListViewParams : FloatingIpListViewParams
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



-- Model for volume list view


type alias VolumeListViewParams =
    { expandedVols : List OSTypes.VolumeUuid
    , deleteConfirmations : List DeleteVolumeConfirmation
    }


type alias DeleteVolumeConfirmation =
    OSTypes.VolumeUuid



-- Model for floating IP list view


type alias FloatingIpListViewParams =
    { deleteConfirmations : List OSTypes.IpAddressUuid
    , hideAssignedIps : Bool
    }



-- Model for assign floating IP view


type alias AssignFloatingIpViewParams =
    { ipUuid : Maybe OSTypes.IpAddressUuid
    , serverUuid : Maybe OSTypes.ServerUuid
    }



-- Model for keypair list view


type alias KeypairListViewParams =
    { expandedKeypairs : List KeypairIdentifier
    , deleteConfirmations : List KeypairIdentifier
    }


type alias KeypairIdentifier =
    ( OSTypes.KeypairName, OSTypes.KeypairFingerprint )