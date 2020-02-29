module Rest.Helpers exposing
    ( idOrName
    , iso8601StringToPosixDecodeError
    , keystoneUrlWithVersion
    , openstackCredentialedRequest
    , proxyifyRequest
    , resultToMsg
    )

import Error exposing (ErrorContext)
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

        TT.Put ->
            "PUT"

        TT.Delete ->
            "DELETE"


openstackCredentialedRequest : TT.Project -> TT.HttpRequestMethod -> Maybe String -> String -> Http.Body -> Http.Expect TT.Msg -> Cmd TT.Msg
openstackCredentialedRequest project method maybeMicroversion origUrl requestBody expect =
    {-
       Prepare an HTTP request to OpenStack which requires a currently valid auth token and maybe a proxy server URL.

       To ensure request is made with a valid token, perform a task which checks the time to see if our auth token is
       still valid or has expired. Pass along a function which accepts an auth token, and returns a fully prepared
       Cmd Msg (which sends the request to OpenStack API).

    -}
    let
        requestProto : Maybe HelperTypes.Url -> OSTypes.AuthTokenString -> Cmd TT.Msg
        requestProto maybeProxyUrl token =
            let
                ( url, headers ) =
                    case maybeProxyUrl of
                        Just proxyUrl ->
                            proxyifyRequest proxyUrl origUrl

                        Nothing ->
                            ( origUrl, [] )
            in
            Http.request
                { method = httpRequestMethodStr method
                , headers =
                    List.concat
                        [ [ Http.header "X-Auth-Token" token ]
                        , case maybeMicroversion of
                            Just microversion ->
                                -- Using this to support Nova server tags, at some point we might want to move it higher
                                [ Http.header "OpenStack-API-Version" microversion ]

                            Nothing ->
                                []
                        , headers
                        ]
                , url = url
                , body = requestBody
                , expect = expect
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Task.perform
        (\posixTime -> TT.ProjectMsg (Helpers.getProjectId project) (TT.PrepareCredentialedRequest requestProto posixTime))
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


resultToMsg : ErrorContext -> (a -> TT.Msg) -> Result Http.Error a -> TT.Msg
resultToMsg errorContext successMsg result =
    -- Generates Msg to deal with result of API call
    case result of
        Err error ->
            TT.HandleApiError errorContext error

        Ok stuff ->
            successMsg stuff


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


iso8601StringToPosixDecodeError : String -> Decode.Decoder Time.Posix
iso8601StringToPosixDecodeError str =
    case Helpers.iso8601StringToPosix str of
        Ok posix ->
            Decode.succeed posix

        Err error ->
            Decode.fail error
