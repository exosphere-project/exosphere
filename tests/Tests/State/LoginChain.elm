module Tests.State.LoginChain exposing (suite)

import Expect
import OpenStack.Types as OSTypes
import State.LoginChain
import Test exposing (Test, describe, test)
import Time
import Types.LoginIntent exposing (HeldLoginToken)


heldToken : OSTypes.KeystoneUrl -> OSTypes.ProjectUuid -> Maybe OSTypes.ApplicationCredential -> HeldLoginToken
heldToken keystoneUrl projectUuid maybeAppCredential =
    let
        nameAndUuid =
            { name = "cloud-riders", uuid = projectUuid }
    in
    { keystoneUrl = keystoneUrl
    , authToken =
        { catalog = []
        , project = nameAndUuid
        , projectDomain = nameAndUuid
        , user = nameAndUuid
        , userDomain = nameAndUuid
        , expiresAt = Time.millisToPosix 0
        , tokenValue = projectUuid
        }
    , appCredential = maybeAppCredential
    }


alliance : OSTypes.KeystoneUrl
alliance =
    "https://cell.alliance.rebel:5000/v3"


empire : OSTypes.KeystoneUrl
empire =
    "https://keystone.empire.gov:5000/v3"


fileCredential : OSTypes.ApplicationCredential
fileCredential =
    { uuid = "a1b2c3", secret = "hope" }


suite : Test
suite =
    describe "credential-file login chain"
        [ test "two region questions run in file order" <|
            \_ ->
                let
                    ( first, afterFirst ) =
                        State.LoginChain.popNext [ "cloud-a", "cloud-b" ]

                    ( second, afterSecond ) =
                        State.LoginChain.popNext afterFirst
                in
                Expect.equal
                    ( Just "cloud-a", Just "cloud-b", [] )
                    ( first, second, afterSecond )
        , test "cancel skips the current login and exposes the next one" <|
            \_ ->
                State.LoginChain.popNext [ "cloud-b" ]
                    |> Expect.equal ( Just "cloud-b", [] )
        , test "leaving region selection abandons the held login and remaining chain" <|
            \_ ->
                let
                    abandoned =
                        State.LoginChain.shouldAbandon True False

                    oldPending =
                        if abandoned then
                            []

                        else
                            [ "cloud-b" ]

                    ( freshFirst, freshRemaining ) =
                        State.LoginChain.popNext ("fresh-cloud" :: oldPending)
                in
                Expect.equal ( Just "fresh-cloud", [] ) ( freshFirst, freshRemaining )
        , test "a failed login advances to the next intent" <|
            \_ ->
                State.LoginChain.popNext [ "cloud-after-failure" ]
                    |> Expect.equal ( Just "cloud-after-failure", [] )
        , test "a stale response from an identical replacement chain is dropped" <|
            \_ ->
                let
                    firstGeneration =
                        State.LoginChain.nextGeneration 0

                    replacementGeneration =
                        State.LoginChain.nextGeneration firstGeneration
                in
                Expect.equal
                    ( False, True )
                    ( State.LoginChain.acceptsResponse replacementGeneration firstGeneration
                    , State.LoginChain.acceptsResponse replacementGeneration replacementGeneration
                    )
        , test "abandonment invalidates an in-flight response before it can restore state" <|
            \_ ->
                let
                    requestGeneration =
                        7

                    generationAfterAbandonment =
                        State.LoginChain.nextGeneration requestGeneration
                in
                State.LoginChain.acceptsResponse generationAfterAbandonment requestGeneration
                    |> Expect.equal False
        , test "an already-added notice survives a region question and remains deduplicated" <|
            \_ ->
                []
                    |> State.LoginChain.addNotice "cloud-riders"
                    |> State.LoginChain.addNotice "rogue-squadron"
                    |> State.LoginChain.addNotice "cloud-riders"
                    |> Expect.equal [ "cloud-riders", "rogue-squadron" ]
        , describe "tokens waiting on a region question"
            [ test "two projects logged in together both keep their tokens" <|
                \_ ->
                    let
                        heldTokens =
                            []
                                |> State.LoginChain.holdToken (heldToken alliance "project-a" Nothing)
                                |> State.LoginChain.holdToken (heldToken alliance "project-b" Nothing)
                    in
                    Expect.equal
                        ( Just (heldToken alliance "project-a" Nothing), Just (heldToken alliance "project-b" Nothing) )
                        ( State.LoginChain.findHeldToken alliance "project-a" heldTokens
                        , State.LoginChain.findHeldToken alliance "project-b" heldTokens
                        )
            , test "the same project UUID on two clouds is two tokens" <|
                \_ ->
                    let
                        heldTokens =
                            []
                                |> State.LoginChain.holdToken (heldToken alliance "project-a" Nothing)
                                |> State.LoginChain.holdToken (heldToken empire "project-a" (Just fileCredential))
                    in
                    Expect.equal
                        ( Just (heldToken alliance "project-a" Nothing), Just (heldToken empire "project-a" (Just fileCredential)) )
                        ( State.LoginChain.findHeldToken alliance "project-a" heldTokens
                        , State.LoginChain.findHeldToken empire "project-a" heldTokens
                        )
            , test "a second token for the same project replaces the first" <|
                \_ ->
                    []
                        |> State.LoginChain.holdToken (heldToken alliance "project-a" Nothing)
                        |> State.LoginChain.holdToken (heldToken alliance "project-a" (Just fileCredential))
                        |> Expect.equal [ heldToken alliance "project-a" (Just fileCredential) ]
            , test "answering one question releases only that token" <|
                \_ ->
                    []
                        |> State.LoginChain.holdToken (heldToken alliance "project-a" Nothing)
                        |> State.LoginChain.holdToken (heldToken alliance "project-b" Nothing)
                        |> State.LoginChain.holdToken (heldToken empire "project-a" Nothing)
                        |> State.LoginChain.releaseHeldToken alliance "project-a"
                        |> List.map (\token -> ( token.keystoneUrl, token.authToken.project.uuid ))
                        |> Expect.equal [ ( empire, "project-a" ), ( alliance, "project-b" ) ]
            , test "abandoning the credential-file chain keeps password logins waiting" <|
                \_ ->
                    []
                        |> State.LoginChain.holdToken (heldToken alliance "project-a" Nothing)
                        |> State.LoginChain.holdToken (heldToken empire "project-b" (Just fileCredential))
                        |> State.LoginChain.dropCredentialFileTokens
                        |> Expect.equal [ heldToken alliance "project-a" Nothing ]
            ]
        ]
