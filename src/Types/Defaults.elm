module Types.Defaults exposing
    ( createServerViewParams
    , jetstreamCreds
    , serverDetailViewParams
    , serverListViewParams
    )

import OpenStack.Types as OSTypes
import ServerDeploy exposing (cloudInitUserDataTemplate)
import Set
import Types.Types as Types


jetstreamCreds : Types.JetstreamCreds
jetstreamCreds =
    { jetstreamProviderChoice = Types.BothJetstreamClouds
    , jetstreamProjectName = ""
    , taccUsername = ""
    , taccPassword = ""
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


createServerViewParams : OSTypes.Image -> Maybe Bool -> Types.CreateServerViewParams
createServerViewParams image deployGuacamole =
    { serverName = image.name
    , imageUuid = image.uuid
    , imageName = image.name
    , count = 1
    , flavorUuid = ""
    , volSizeTextInput = Nothing
    , userDataTemplate = cloudInitUserDataTemplate
    , networkUuid = ""
    , showAdvancedOptions = False
    , keypairName = Nothing
    , deployGuacamole = deployGuacamole
    }
