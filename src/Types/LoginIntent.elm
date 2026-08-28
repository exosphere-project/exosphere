module Types.LoginIntent exposing (HeldLoginToken, LoginIntent)

import OpenStack.Types as OSTypes


type alias LoginIntent =
    { name : String
    , authUrl : OSTypes.KeystoneUrl
    , appCredential : OSTypes.ApplicationCredential
    , regionName : Maybe OSTypes.RegionId
    }


type alias HeldLoginToken =
    { keystoneUrl : OSTypes.KeystoneUrl
    , authToken : OSTypes.ScopedAuthToken
    , appCredential : Maybe OSTypes.ApplicationCredential
    }
