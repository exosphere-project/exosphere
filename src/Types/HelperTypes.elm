module Types.HelperTypes exposing
    ( CloudSpecificConfig
    , CloudSpecificConfigMap
    , CreateServerPageModel
    , DefaultLoginView(..)
    , FlavorGroup
    , FloatingIpAssignmentStatus(..)
    , FloatingIpOption(..)
    , FloatingIpReuseOption(..)
    , Hostname
    , HttpRequestMethod(..)
    , IPv4AddressPublicRoutability(..)
    , InstanceType
    , InstanceTypeImageFilters
    , InstanceTypeVersion
    , Jetstream1Creds
    , Jetstream1Provider(..)
    , KeystoneHostname
    , Localization
    , MetadataFilter
    , OpenIdConnectLoginConfig
    , Passphrase
    , ProjectIdentifier
    , SentryConfig
    , ServerResourceQtys
    , SupportableItemType(..)
    , UnscopedProvider
    , UnscopedProviderProject
    , UnscopedProviderRegion
    , Url
    , UserAppProxyHostname
    , Uuid
    , WindowSize
    )

import Dict exposing (Dict)
import OpenStack.Types as OSTypes
import RemoteData exposing (WebData)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput)



{- Primitive types -}


type alias Url =
    String


type alias Hostname =
    String


type alias UserAppProxyHostname =
    Hostname


type alias KeystoneHostname =
    Hostname


type alias Uuid =
    String


type alias ProjectIdentifier =
    -- We use this when referencing a Project in a Msg (or otherwise passing through the runtime)
    { projectUuid : Uuid
    , regionId : Maybe OSTypes.RegionId
    }


type alias Passphrase =
    String


type HttpRequestMethod
    = Get
    | Post
    | Put
    | Delete



{- Helper types for SharedModel -}


type DefaultLoginView
    = DefaultLoginOpenstack
    | DefaultLoginJetstream1
    | DefaultLoginOpenIdConnect


type alias Localization =
    { openstackWithOwnKeystone : String
    , openstackSharingKeystoneWithAnother : String
    , unitOfTenancy : String
    , maxResourcesPerProject : String
    , pkiPublicKeyForSsh : String
    , virtualComputer : String
    , virtualComputerHardwareConfig : String
    , cloudInitData : String
    , commandDrivenTextInterface : String
    , staticRepresentationOfBlockDeviceContents : String
    , blockDevice : String
    , nonFloatingIpAddress : String
    , floatingIpAddress : String
    , publiclyRoutableIpAddress : String
    , graphicalDesktopEnvironment : String
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias SentryConfig =
    { dsnPublicKey : String
    , dsnHost : String
    , dsnProjectId : String
    , releaseVersion : String
    , environmentName : String
    }


type alias MetadataFilter =
    { filterKey : String
    , filterValue : String
    }


type alias CloudSpecificConfig =
    { friendlyName : String
    , friendlySubName : Maybe String
    , userAppProxy : Maybe UserAppProxyHostname
    , imageExcludeFilter : Maybe MetadataFilter
    , featuredImageNamePrefix : Maybe String
    , instanceTypes : List InstanceType
    , flavorGroups : List FlavorGroup
    }


type alias CloudSpecificConfigMap =
    Dict KeystoneHostname CloudSpecificConfig


type alias InstanceType =
    { friendlyName : String
    , description : String
    , logo : Url
    , versions : List InstanceTypeVersion
    }


type alias InstanceTypeVersion =
    { friendlyName : String
    , isPrimary : Bool
    , imageFilters : InstanceTypeImageFilters
    , restrictFlavorIds : Maybe (List OSTypes.FlavorId)
    }


type alias InstanceTypeImageFilters =
    { nameFilter : Maybe String
    , uuidFilter : Maybe OSTypes.ImageUuid
    , visibilityFilter : Maybe OSTypes.ImageVisibility
    , osDistroFilter : Maybe String
    , osVersionFilter : Maybe String
    , metadataFilter : Maybe MetadataFilter
    }


type alias FlavorGroup =
    { matchOn : String
    , title : String
    , description : Maybe String
    }


type alias UnscopedProvider =
    { authUrl : OSTypes.KeystoneUrl
    , token : OSTypes.UnscopedAuthToken
    , projectsAvailable : WebData (List UnscopedProviderProject)
    , regionsAvailable : WebData (List UnscopedProviderRegion)
    }


type alias UnscopedProviderProject =
    { project : OSTypes.NameAndUuid
    , description : Maybe String
    , domainId : Uuid
    , enabled : Bool
    }


type alias UnscopedProviderRegion =
    OSTypes.Region


type alias OpenIdConnectLoginConfig =
    { keystoneAuthUrl : String
    , webssoKeystoneEndpoint : String
    , oidcLoginIcon : String
    , oidcLoginButtonLabel : String
    , oidcLoginButtonDescription : String
    }


type alias ServerResourceQtys =
    { cores : Int
    , vgpus : Maybe Int
    , ramGb : Int
    , rootDiskGb : Maybe Int
    }



{- Helpers for IP addresses -}


type IPv4AddressPublicRoutability
    = PrivateRfc1918Space
    | PublicNonRfc1918Space


type
    FloatingIpOption
    -- Wait to see if server gets a fixed IP in publicly routable space
    = Automatic
      -- Use a floating IP as soon as we are able to do so
    | UseFloatingIp FloatingIpReuseOption FloatingIpAssignmentStatus
    | DoNotUseFloatingIp


type FloatingIpReuseOption
    = CreateNewFloatingIp
    | UseExistingFloatingIp OSTypes.IpAddressUuid


type FloatingIpAssignmentStatus
    = Unknown
      -- We need an active server with a port and an external network before we can assign a floating IP address
    | WaitingForResources
    | Attemptable
    | AttemptedWaiting



{- Stuff that should move somewhere else -}


type alias Jetstream1Creds =
    { jetstream1ProviderChoice : Jetstream1Provider
    , taccUsername : String
    , taccPassword : String
    }


type Jetstream1Provider
    = IUCloud
    | TACCCloud
    | BothJetstream1Clouds


type SupportableItemType
    = SupportableProject
    | SupportableImage
    | SupportableServer
    | SupportableVolume


type alias CreateServerPageModel =
    { serverName : String
    , imageUuid : OSTypes.ImageUuid
    , imageName : String
    , restrictFlavorIds : Maybe (List OSTypes.FlavorId)
    , count : Int
    , flavorId : Maybe OSTypes.FlavorId
    , volSizeTextInput : Maybe NumericTextInput
    , userDataTemplate : String
    , networkUuid : Maybe OSTypes.NetworkUuid
    , showAdvancedOptions : Bool
    , keypairName : Maybe String
    , deployGuacamole : Maybe Bool -- Nothing when cloud doesn't support Guacamole
    , deployDesktopEnvironment : Bool
    , installOperatingSystemUpdates : Bool
    , floatingIpCreationOption : FloatingIpOption
    , includeWorkflow : Bool
    , workflowInputRepository : String
    , workflowInputReference : String
    , workflowInputPath : String
    , workflowInputIsValid : Maybe Bool
    , showWorkflowExplanationToggleTip : Bool
    , createCluster : Bool
    , showClusterExplanationToggleTip : Bool
    , showFormInvalidToggleTip : Bool
    }
