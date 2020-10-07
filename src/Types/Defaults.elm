module Types.Defaults exposing
    ( createServerViewParams
    , serverDetailViewParams
    , serverListViewParams
    )

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
createServerViewParams createServerRequest deployGuacamole =
    { createServerRequest = createServerRequest
    , volSizeTextInput = Nothing
    , deployGuacamole = deployGuacamole
    }
