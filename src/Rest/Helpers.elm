module Rest.Helpers exposing (openstackCredentialedRequest)

import Http
import Types.HelperTypes exposing (..)
import Types.RestTypes exposing (..)
import Types.Types exposing (..)


httpRequestMethodStr : HttpRequestMethod -> String
httpRequestMethodStr method =
    case method of
        Get ->
            "GET"

        Post ->
            "POST"

        Delete ->
            "DELETE"


openstackCredentialedRequest : Provider -> HttpRequestMethod -> Url -> Http.Body -> Http.Expect a -> (Result Http.Error a -> Msg) -> Cmd Msg
openstackCredentialedRequest provider method url requestBody expect resultMsg =
    -- This wraps Http.request and Http.send, lets us do logic of only sending when we have a valid auth token
    let
        request =
            Http.request
                { method = httpRequestMethodStr method
                , headers = [ Http.header "X-Auth-Token" provider.auth.tokenValue ]
                , url = url
                , body = requestBody
                , expect = expect
                , timeout = Nothing
                , withCredentials = False
                }
    in
    Http.send resultMsg request
