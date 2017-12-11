module Types exposing (AuthResponse, AuthIntro, Provider, Service, EndPoint, CallApiEndPointResponse, decodeAuth)

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


type alias AuthResponse =
    { access : AuthAccess
    }


type alias AuthAccess =
    { token : AuthAccessToken
    , serviceCatalog : List Service
    }


type alias Service =
    { serviceName : String
    , serviceType : String
    , endPoints : List EndPoint
    }


type alias EndPoint =
    { region : String
    , publicURL : String
    , tenantId : String
    }


type alias AuthAccessToken =
    { expires : String
    , id : String
    }


type alias CallApiEndPointResponse =
    String


decodeAuth : Json.Decode.Decoder AuthResponse
decodeAuth =
    Json.Decode.Pipeline.decode AuthResponse
        |> Json.Decode.Pipeline.required "access" decodeAuthAccess


decodeAuthAccess : Json.Decode.Decoder AuthAccess
decodeAuthAccess =
    Json.Decode.Pipeline.decode AuthAccess
        |> Json.Decode.Pipeline.required "token" decodeAuthAccessToken
        |> Json.Decode.Pipeline.required "serviceCatalog" (Json.Decode.list decodeService)


decodeService : Json.Decode.Decoder Service
decodeService =
    Json.Decode.Pipeline.decode Service
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string
        |> Json.Decode.Pipeline.required "endpoints" (Json.Decode.list decodeEndPoint)


decodeEndPoint : Json.Decode.Decoder EndPoint
decodeEndPoint =
    Json.Decode.Pipeline.decode EndPoint
        |> Json.Decode.Pipeline.required "region" Json.Decode.string
        |> Json.Decode.Pipeline.required "publicURL" Json.Decode.string
        |> Json.Decode.Pipeline.required "tenantId" Json.Decode.string


decodeAuthAccessToken : Json.Decode.Decoder AuthAccessToken
decodeAuthAccessToken =
    Json.Decode.Pipeline.decode AuthAccessToken
        |> Json.Decode.Pipeline.required "expires" Json.Decode.string
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
