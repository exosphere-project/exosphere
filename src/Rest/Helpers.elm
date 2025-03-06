module Rest.Helpers exposing
    ( expectJsonWithErrorBody
    , expectStringWithErrorBody
    , httpResponseStringToResult
    , idOrName
    , keystoneUrlWithVersion
    , openstackCredentialedRequest
    , proxyifyRequest
    , resultToMsgErrorBody
    )

import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import OpenStack.Types as OSTypes
import Task
import Time
import Types.Error exposing (ErrorContext, HttpErrorWithBody)
import Types.HelperTypes as HelperTypes exposing (HttpRequestMethod(..))
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Url
import Url.Builder


httpRequestMethodStr : HttpRequestMethod -> String
httpRequestMethodStr method =
    case method of
        Get ->
            "GET"

        Post ->
            "POST"

        Put ->
            "PUT"

        Patch ->
            "PATCH"

        Delete ->
            "DELETE"


openstackCredentialedRequest :
    HelperTypes.ProjectIdentifier
    -> HttpRequestMethod
    -> Maybe String
    -> HelperTypes.Headers
    -> ( HelperTypes.Url, HelperTypes.UrlPath, HelperTypes.UrlParams )
    -> Http.Body
    -> Http.Expect SharedMsg
    -> Cmd SharedMsg
openstackCredentialedRequest projectId method maybeMicroversion additionalHeaders urlParts requestBody expect =
    {-
       Prepare an HTTP request to OpenStack which requires a currently valid auth token and maybe a proxy server URL.

       To ensure request is made with a valid token, perform a task which checks the time to see if our auth token is
       still valid or has expired. Pass along a function which accepts an auth token, and returns a fully prepared
       Cmd Msg (which sends the request to OpenStack API).

    -}
    let
        requestProto : Maybe HelperTypes.Url -> OSTypes.AuthTokenString -> Cmd SharedMsg
        requestProto maybeProxyUrl token =
            let
                origUrl =
                    let
                        ( baseUrl, urlParameters, urlQueryParameters ) =
                            urlParts
                    in
                    Url.Builder.crossOrigin baseUrl urlParameters urlQueryParameters

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
                        , List.map (\( k, v ) -> Http.header k v) additionalHeaders
                        ]
                , url = url
                , body = requestBody
                , expect = expect
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Task.perform
        (\posixTime -> ProjectMsg projectId (PrepareCredentialedRequest requestProto posixTime))
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


resultToMsgErrorBody : ErrorContext -> (a -> SharedMsg) -> Result HttpErrorWithBody a -> SharedMsg
resultToMsgErrorBody errorContext successMsg result =
    -- Generates Msg to deal with result of API call
    -- TODO this is a _transitional_ function that should be removed when
    -- TODO https://gitlab.com/exosphere/exosphere/-/issues/339 is fixed
    case result of
        Err error ->
            HandleApiErrorWithBody errorContext error

        Ok stuff ->
            successMsg stuff


httpResponseStringToResult : (String -> Result HttpErrorWithBody a) -> Http.Response String -> Result HttpErrorWithBody a
httpResponseStringToResult decode response =
    case response of
        Http.BadUrl_ url ->
            Err <| HttpErrorWithBody (Http.BadUrl url) ""

        Http.Timeout_ ->
            Err <| HttpErrorWithBody Http.Timeout ""

        Http.NetworkError_ ->
            Err <| HttpErrorWithBody Http.NetworkError ""

        Http.BadStatus_ metadata body ->
            Err <| HttpErrorWithBody (Http.BadStatus metadata.statusCode) body

        Http.GoodStatus_ _ body ->
            body |> decode


expectStringWithErrorBody : (Result HttpErrorWithBody String -> msg) -> Http.Expect msg
expectStringWithErrorBody toMsg =
    -- Implements the example here: https://package.elm-lang.org/packages/elm/http/latest/Http#expectStringResponse
    -- When we have an error with the response, we return the error along with the response body
    -- so that we can show the response body as an error message in the app.
    Http.expectStringResponse toMsg <|
        httpResponseStringToResult Ok


expectJsonWithErrorBody : (Result HttpErrorWithBody a -> msg) -> Decode.Decoder a -> Http.Expect msg
expectJsonWithErrorBody toMsg decoder =
    -- Implements the example here: https://package.elm-lang.org/packages/elm/http/latest/Http#expectStringResponse
    -- When we have an error with the response or the JSON decoding, we return the error along with the response body
    -- so that we can show the response body as an error message in the app.
    Http.expectStringResponse toMsg <|
        httpResponseStringToResult <|
            \body ->
                case Decode.decodeString decoder body of
                    Ok value ->
                        Ok value

                    Err err ->
                        Err <| HttpErrorWithBody (Http.BadBody (Decode.errorToString err)) body


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
