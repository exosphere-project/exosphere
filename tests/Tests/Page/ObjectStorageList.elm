module Tests.Page.ObjectStorageList exposing (s3CredentialPanelDecisionSuite)

import Expect
import Helpers.RemoteDataPlusPlus as RDPP
import Http
import OpenStack.Types as OSTypes
import Page.ObjectStorageList as ObjectStorageList exposing (S3CredentialPanelState(..))
import Test exposing (Test, describe, test)
import Time
import Types.Error exposing (HttpErrorWithBody)
import Types.Project exposing (ProjectSecret(..))


receivedTime : Time.Posix
receivedTime =
    Time.millisToPosix 0


httpError : Http.Error -> HttpErrorWithBody
httpError error =
    { error = error, body = "sentinel body" }


applicationCredential : ProjectSecret
applicationCredential =
    ApplicationCredential { uuid = "app-cred-uuid", secret = "app-cred-secret" }


ec2Credential : OSTypes.Ec2Credential
ec2Credential =
    { access = "access-key"
    , secret = "secret-key"
    , tenantId = "project-uuid"
    }


erroredCredentials : Http.Error -> RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.Ec2Credential)
erroredCredentials error =
    RDPP.RemoteDataPlusPlus
        RDPP.DontHave
        (RDPP.NotLoading (Just ( httpError error, receivedTime )))


staleCredentialsWithError : Http.Error -> List OSTypes.Ec2Credential -> RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.Ec2Credential)
staleCredentialsWithError error credentials =
    RDPP.RemoteDataPlusPlus
        (RDPP.DoHave credentials receivedTime)
        (RDPP.NotLoading (Just ( httpError error, receivedTime )))


s3CredentialPanelDecisionSuite : Test
s3CredentialPanelDecisionSuite =
    describe "Page.ObjectStorageList.s3CredentialPanelDecision chooses the S3 credential panel state"
        [ test "403 error state on list chooses guidance and hides create" <|
            \_ ->
                let
                    decision =
                        ObjectStorageList.s3CredentialPanelDecision applicationCredential (erroredCredentials (Http.BadStatus 403))
                in
                Expect.equal
                    { state = S3CredentialPanelApplicationCredentialForbidden
                    , showCreateButton = False
                    }
                    decision
        , test "403 arriving from create chooses guidance even when stale credentials exist" <|
            \_ ->
                let
                    decision =
                        ObjectStorageList.s3CredentialPanelDecision applicationCredential (staleCredentialsWithError (Http.BadStatus 403) [ ec2Credential ])
                in
                Expect.equal
                    { state = S3CredentialPanelApplicationCredentialForbidden
                    , showCreateButton = False
                    }
                    decision
        , test "403 without an application credential is a policy denial, so it takes the normal path" <|
            \_ ->
                let
                    decision =
                        ObjectStorageList.s3CredentialPanelDecision NoProjectSecret (erroredCredentials (Http.BadStatus 403))
                in
                Expect.equal
                    { state = S3CredentialPanelNormal
                    , showCreateButton = False
                    }
                    decision
        , test "non-403 errors use the normal RDPP path and keep current create-button behavior" <|
            \_ ->
                let
                    summarize rdpp =
                        let
                            decision =
                                ObjectStorageList.s3CredentialPanelDecision applicationCredential rdpp
                        in
                        ( decision.state, decision.showCreateButton )
                in
                Expect.equal
                    [ ( S3CredentialPanelNormal, False )
                    , ( S3CredentialPanelNormal, False )
                    , ( S3CredentialPanelNormal, True )
                    ]
                    [ summarize (erroredCredentials (Http.BadStatus 500))
                    , summarize (erroredCredentials Http.NetworkError)
                    , summarize (staleCredentialsWithError (Http.BadStatus 500) [])
                    ]
        ]
