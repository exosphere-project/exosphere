module IdentityTypes exposing (..)

import Json.Encode
import Json.Decode
import Json.Decode.Pipeline


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
        |> Json.Decode.Pipeline.required "access" (decodeAuthAccess)


decodeAuthAccess : Json.Decode.Decoder AuthAccess
decodeAuthAccess =
    Json.Decode.Pipeline.decode AuthAccess
        |> Json.Decode.Pipeline.required "token" (decodeAuthAccessToken)


decodeAuthAccessToken : Json.Decode.Decoder AuthAccessToken
decodeAuthAccessToken =
    Json.Decode.Pipeline.decode AuthAccessToken
        |> Json.Decode.Pipeline.required "expires" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "id" (Json.Decode.string)
