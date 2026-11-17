module Orchestration.GoalStatusPage exposing (goalPollStatusPages)

import Dict
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.StatusPage
import Rest.StatusPage
import Time
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (SharedMsg)


statusPagePollIntervalMs : Int
statusPagePollIntervalMs =
    -- 5 minutes
    300000


{-| Read the status page of each relevant cloud that has one, when it has never been read or was last
tried more than a poll interval ago. A cloud that just became relevant, e.g. on login, is read right away.
-}
goalPollStatusPages : Time.Posix -> SharedModel -> ( SharedModel, Cmd SharedMsg )
goalPollStatusPages time model =
    Helpers.StatusPage.statusPagesToPoll model
        |> List.foldl
            (\( keystoneHostname, config ) ( model_, cmds ) ->
                let
                    statusPage =
                        Dict.get keystoneHostname model_.statusPages
                            |> Maybe.withDefault RDPP.empty
                in
                if pollDue time statusPage then
                    ( { model_ | statusPages = Dict.insert keystoneHostname (RDPP.setLoading statusPage) model_.statusPages }
                    , Cmd.batch [ cmds, Rest.StatusPage.requestStatusPage keystoneHostname config ]
                    )

                else
                    ( model_, cmds )
            )
            ( model, Cmd.none )


{-| Unlike `Orchestration.Helpers.pollRDPP`, this also waits a full interval after a failed request when
older data is present, so an unreachable status page isn't asked again on every orchestration pass.
-}
pollDue : Time.Posix -> RDPP.RemoteDataPlusPlus e a -> Bool
pollDue time rdpp =
    let
        lastAttemptMillis =
            List.filterMap identity
                [ case rdpp.data of
                    RDPP.DoHave _ receivedTime ->
                        Just (Time.posixToMillis receivedTime)

                    RDPP.DontHave ->
                        Nothing
                , case rdpp.refreshStatus of
                    RDPP.NotLoading (Just ( _, errorTime )) ->
                        Just (Time.posixToMillis errorTime)

                    _ ->
                        Nothing
                ]
                |> List.maximum
    in
    case ( rdpp.refreshStatus, lastAttemptMillis ) of
        ( RDPP.Loading, _ ) ->
            False

        ( _, Nothing ) ->
            True

        ( _, Just lastAttempt ) ->
            Time.posixToMillis time >= lastAttempt + statusPagePollIntervalMs
