module Types exposing (Auth, AuthIntro, Provider, decodeAuth)

import Json.Decode
import Json.Decode.Pipeline


type alias Provider =
    { name : String
    , identity_api_version : Int
    , auth :
        { username : String
        , password : String
        , project_name : String
        , auth_url : String
        }
    }


type alias AuthIntro =
    String


type alias Auth =
    { access : AuthAccess
    }


type alias AuthAccess =
    { token : AuthAccessToken
    }


type alias AuthAccessToken =
    { expires : String
    , id : String
    }


decodeAuth : Json.Decode.Decoder Auth
decodeAuth =
    Json.Decode.Pipeline.decode Auth
        |> Json.Decode.Pipeline.required "access" decodeAuthAccess


decodeAuthAccess : Json.Decode.Decoder AuthAccess
decodeAuthAccess =
    Json.Decode.Pipeline.decode AuthAccess
        |> Json.Decode.Pipeline.required "token" decodeAuthAccessToken


decodeAuthAccessToken : Json.Decode.Decoder AuthAccessToken
decodeAuthAccessToken =
    Json.Decode.Pipeline.decode AuthAccessToken
        |> Json.Decode.Pipeline.required "expires" Json.Decode.string
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
