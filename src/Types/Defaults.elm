module Types.Defaults exposing
    ( createServerRequest
    , createServerViewParams
    , serverDetailViewParams
    , serverListViewParams
    )

import Helpers.Helpers as Helpers
import OpenStack.Types as OSTypes
import ServerDeploy exposing (cloudInitUserData)
import Set
import Types.Types as Types


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
    }


createServerViewParams : Types.CreateServerRequest -> Maybe Bool -> Types.CreateServerViewParams
createServerViewParams createServerRequest_ deployGuacamole =
    { createServerRequest = createServerRequest_
    , volSizeTextInput = Nothing
    , deployGuacamole = deployGuacamole
    }


createServerRequest : Types.Project -> OSTypes.Image -> Types.CreateServerRequest
createServerRequest project image =
    { name = image.name
    , projectId = Helpers.getProjectId project
    , imageUuid = image.uuid
    , imageName = image.name
    , count = 1
    , flavorUuid = ""
    , volBackedSizeGb = Nothing
    , keypairName = Nothing
    , userData = cloudInitUserData
    , networkUuid = ""
    , showAdvancedOptions = False
    }
