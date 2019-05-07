module Rest.Helpers exposing (openstackCredentialedRequest)

import Helpers.Helpers as Helpers
import Http
import OpenStack.Types as OSTypes
import Task
import Time
import Types.Types as TT


httpRequestMethodStr : TT.HttpRequestMethod -> String
httpRequestMethodStr method =
    case method of
        TT.Get ->
            "GET"

        TT.Post ->
            "POST"

        TT.Delete ->
            "DELETE"


openstackCredentialedRequest : TT.Project -> TT.HttpRequestMethod -> String -> Http.Body -> Http.Expect TT.Msg -> Cmd TT.Msg
openstackCredentialedRequest project method url requestBody expect =
    {-
       In order to ensure request is made with a valid token, perform a task
       which checks the time to see if our auth token is still valid or has
       expired. Pass along a function which accepts an auth token, and returns
       a "hydrated" Cmd Msg (which sends the request to OpenStack API).

    -}
    let
        tokenToRequestCmd : OSTypes.AuthTokenString -> Cmd TT.Msg
        tokenToRequestCmd token =
            Http.request
                { method = httpRequestMethodStr method
                , headers = [ Http.header "X-Auth-Token" token ]
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
