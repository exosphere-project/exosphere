module Rest.Helpers exposing
    ( idOrName
    , iso8601StringToPosixDecodeError
    , keystoneUrlWithVersion
    , openstackCredentialedRequest
    , proxyifyRequest
    )

import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import OpenStack.Types as OSTypes
import Task
import Time
import Types.HelperTypes as HelperTypes
import Types.Types as TT
import Url


httpRequestMethodStr : TT.HttpRequestMethod -> String
httpRequestMethodStr method =
    case method of
        TT.Get ->
            "GET"

        TT.Post ->
            "POST"

        TT.Delete ->
            "DELETE"


openstackCredentialedRequest : TT.Project -> Maybe HelperTypes.Url -> TT.HttpRequestMethod -> String -> Http.Body -> Http.Expect TT.Msg -> Cmd TT.Msg
openstackCredentialedRequest project maybeProxyUrl method origUrl requestBody expect =
    {-
       In order to ensure request is made with a valid token, perform a task
       which checks the time to see if our auth token is still valid or has
       expired. Pass along a function which accepts an auth token, and returns
       a "hydrated" Cmd Msg (which sends the request to OpenStack API).

    -}
    let
        ( url, headers ) =
            case maybeProxyUrl of
                Just proxyUrl ->
                    proxyifyRequest proxyUrl origUrl

                Nothing ->
                    ( origUrl, [] )

        tokenToRequestCmd : OSTypes.AuthTokenString -> Cmd TT.Msg
        tokenToRequestCmd token =
            Http.request
                { method = httpRequestMethodStr method
                , headers = Http.header "X-Auth-Token" token :: headers
                , url = url
                , body = requestBody
                , expect = expect
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Task.perform
        (\posixTime -> TT.ProjectMsg (Helpers.getProjectId project) (TT.ValidateTokenForCredentialedRequest tokenToRequestCmd posixTime))
        Time.now


proxyifyRequest : String -> String -> ( String, List Http.Header )
proxyifyRequest proxyServerUrl requestUrlStr =
    {- Returns URL to pass to proxy server, and a list of HTTP headers -}
    let
        {- Todo should we pass Url.Url around the app instead of URLs as strings? The following string-to-URL conversion is ugly -}
        defaultUrl =
            Url.Url
                Url.Https
                "thisisbroken.pizza"
                Nothing
                ""
                Nothing
                Nothing

        requestUrl =
            Url.fromString requestUrlStr
                |> Maybe.withDefault defaultUrl

        origHost =
            requestUrl.host

        origPort =
            requestUrl.port_ |> Maybe.withDefault 443

        pathQuery =
            case requestUrl.query of
                Just query ->
                    requestUrl.path ++ "?" ++ query

                Nothing ->
                    requestUrl.path

        proxyRequestUrl =
            proxyServerUrl ++ pathQuery
    in
    ( proxyRequestUrl
    , [ Http.header "exo-proxy-orig-host" origHost
      , Http.header "exo-proxy-orig-port" <| String.fromInt origPort
      ]
    )


keystoneUrlWithVersion : String -> String
keystoneUrlWithVersion inputUrl =
    -- Some clouds have a service catalog specifying "/v3" in the path of the Keystone admin API, whereas some don't, so we need to add it.
    case Url.fromString inputUrl of
        Nothing ->
            -- Cannot parse URL, return as-is
            inputUrl

        Just url ->
            if String.contains "/v3" url.path then
                inputUrl

            else
                Url.toString { url | path = "/v3" }


idOrName : String -> String
idOrName str =
    if Helpers.stringIsUuidOrDefault str then
        "id"

    else
        "name"


iso8601StringToPosixDecodeError str =
    case Helpers.iso8601StringToPosix str of
        Ok posix ->
            Decode.succeed posix

        Err error ->
            Decode.fail error
