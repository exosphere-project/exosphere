module Types.Defaults exposing (serverListViewParams)

import Set
import Types.Types as Types


serverListViewParams : Types.ServerListViewParams
serverListViewParams =
    { onlyOwnServers = True
    , selectedServers = Set.empty
    , deleteConfirmations = []
    }
