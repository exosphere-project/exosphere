module Types.Defaults exposing (serverListViewParams)

import Types.Types as Types


serverListViewParams : Types.ServerListViewParams
serverListViewParams =
    { onlyOwnServers = True
    , deleteConfirmations = []
    }
