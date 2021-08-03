module Types.View exposing
    ( AllResourcesListViewParams
    , AssignFloatingIpViewParams
    , CreateServerViewParams
    , DeleteConfirmation
    , DeleteVolumeConfirmation
    , FloatingIpListViewParams
    , IPInfoLevel(..)
    , ImageListViewParams
    , ImageListVisibilityFilter
    , JetstreamCreds
    , JetstreamProvider(..)
    , KeypairIdentifier
    , KeypairListViewParams
    , LoginView(..)
    , NonProjectViewConstructor(..)
    , OpenstackLoginFormEntryType(..)
    , OpenstackLoginViewParams
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
import Set
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Types.HelperTypes as HelperTypes
import Types.Interaction exposing (Interaction)



{- View state types -}


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


type
    SupportableItemType
    -- Ideally this would be in View.Types, eh
    = SupportableProject
    | SupportableImage
    | SupportableServer
    | SupportableVolume


type LoginView
    = LoginOpenstack OpenstackLoginViewParams
    | LoginJetstream JetstreamCreds


type alias OpenstackLoginViewParams =
    { creds : OSTypes.OpenstackLogin
    , openRc : String
    , formEntryType : OpenstackLoginFormEntryType
    }


type OpenstackLoginFormEntryType
    = LoginViewCredsEntry
    | LoginViewOpenRcEntry


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


type alias ProjectViewParams =
    { createPopup : Bool
    }


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
    | CreateServer CreateServerViewParams
    | CreateVolume OSTypes.VolumeName NumericTextInput
    | AttachVolumeModal (Maybe OSTypes.ServerUuid) (Maybe OSTypes.VolumeUuid)
    | MountVolInstructions OSTypes.VolumeAttachment


type alias AllResourcesListViewParams =
    { serverListViewParams : ServerListViewParams
    , volumeListViewParams : VolumeListViewParams
    , keypairListViewParams : KeypairListViewParams
    , floatingIpListViewParams : FloatingIpListViewParams
    }


type alias ServerListViewParams =
    { onlyOwnServers : Bool
    , selectedServers : Set.Set ServerSelection
    , deleteConfirmations : List DeleteConfirmation
    }


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


type alias VolumeListViewParams =
    { expandedVols : List OSTypes.VolumeUuid
    , deleteConfirmations : List DeleteVolumeConfirmation
    }


type alias CreateServerViewParams =
    { serverName : String
    , imageUuid : OSTypes.ImageUuid
    , imageName : String
    , count : Int
    , flavorUuid : OSTypes.FlavorUuid
    , volSizeTextInput : Maybe NumericTextInput
    , userDataTemplate : String
    , networkUuid : Maybe OSTypes.NetworkUuid
    , showAdvancedOptions : Bool
    , keypairName : Maybe String
    , deployGuacamole : Maybe Bool -- Nothing when cloud doesn't support Guacamole
    , deployDesktopEnvironment : Bool
    , installOperatingSystemUpdates : Bool
    , floatingIpCreationOption : HelperTypes.FloatingIpOption
    }


type alias ServerSelection =
    OSTypes.ServerUuid


type alias DeleteConfirmation =
    OSTypes.ServerUuid


type alias DeleteVolumeConfirmation =
    OSTypes.VolumeUuid


type alias FloatingIpListViewParams =
    { deleteConfirmations : List OSTypes.IpAddressUuid
    , hideAssignedIps : Bool
    }


type alias AssignFloatingIpViewParams =
    { ipUuid : Maybe OSTypes.IpAddressUuid
    , serverUuid : Maybe OSTypes.ServerUuid
    }


type alias KeypairListViewParams =
    { expandedKeypairs : List KeypairIdentifier
    , deleteConfirmations : List KeypairIdentifier
    }


type alias KeypairIdentifier =
    ( OSTypes.KeypairName, OSTypes.KeypairFingerprint )


type IPInfoLevel
    = IPDetails
    | IPSummary


type alias VerboseStatus =
    Bool


type PasswordVisibility
    = PasswordShown
    | PasswordHidden


type alias JetstreamCreds =
    { jetstreamProviderChoice : JetstreamProvider
    , taccUsername : String
    , taccPassword : String
    }


type JetstreamProvider
    = IUCloud
    | TACCCloud
    | BothJetstreamClouds
