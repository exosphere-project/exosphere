module Tests.Rest.StatusPage exposing (statusIoDecoderSuite, statusPageConfigDecoderSuite)

import Expect
import Helpers.StatusPage
import Json.Decode as Decode
import Rest.StatusPage
import Test exposing (Test, describe, test)
import Time
import Types.StatusPage exposing (StatusLevel(..), StatusPageProvider(..))


{-| Trimmed from a real `GET https://api.status.io/1.0/status/{pageId}` response for Jetstream2 on
2026-11-09, plus one inactive incident. The upcoming maintenance is stale: its planned end had passed,
but status.io still listed it.
-}
statusIoJson : String
statusIoJson =
    """
    { "result":
        { "status_overall": { "updated": "2026-11-09T17:49:52.345Z", "status": "Service Disruption", "status_code": 500 }
        , "status": []
        , "incidents":
            [ { "_id": "6af207c0f7988a0425ce6537"
              , "name": "Jetstream2 unavailable due to storage failures"
              , "datetime_open": "2026-11-09T17:49:52.148Z"
              , "current_active": true
              , "messages":
                  [ { "details": "Jetstream2 is experiencing storage failures.", "state": 100, "status": 500, "datetime": "2026-11-09T17:49:52.192Z" } ]
              , "containers_affected": []
              , "components_affected": []
              }
            , { "_id": "6ab576500641a7056be8c598"
              , "name": "Scarce availability of GPU resources"
              , "datetime_open": "2026-10-28T19:13:00.000Z"
              , "current_active": true
              , "messages":
                  [ { "details": "Changing status to partial service disruption.", "state": 200, "status": 400, "datetime": "2026-11-05T22:06:12.667Z" }
                  , { "details": "Limited availability of GPUs.", "state": 200, "status": 100, "datetime": "2026-10-28T19:13:00.000Z" }
                  ]
              , "containers_affected": []
              , "components_affected": []
              }
            , { "_id": "6ae109842e972f05fd11fab7"
              , "name": "Scarce availability of floating IP addresses; Web Desktop and Web Shell impacted"
              , "datetime_open": "2026-10-27T20:28:00.000Z"
              , "current_active": true
              , "messages":
                  [ { "details": "Extreme scarcity of floating IP addresses.", "state": 200, "status": 400, "datetime": "2026-10-27T20:28:00.000Z" } ]
              , "containers_affected": []
              , "components_affected": []
              }
            , { "_id": "resolved-incident"
              , "name": "Resolved incident"
              , "datetime_open": "2026-10-01T00:00:00.000Z"
              , "current_active": false
              , "messages":
                  [ { "details": "Resolved.", "state": 400, "status": 100, "datetime": "2026-10-02T00:00:00.000Z" } ]
              }
            ]
        , "maintenance":
            { "active": []
            , "upcoming":
                [ { "_id": "6aea14750d2fc504258522ef"
                  , "datetime_planned_end": "2026-11-06T07:00:00.000Z"
                  , "datetime_planned_start": "2026-11-06T01:00:00.000Z"
                  , "name": "Jetstream2: Scheduled outage due to maintenance at the IU Data Center"
                  , "datetime_open": "2026-11-03T17:05:57.060Z"
                  , "messages": []
                  }
                ]
            }
        }
    }
    """


statusIoDecoderSuite : Test
statusIoDecoderSuite =
    let
        decoded =
            Decode.decodeString Rest.StatusPage.statusIoDecoder statusIoJson
    in
    describe "statusIoDecoder decodes status.io's public status response"
        [ test "decodes active incidents at the level of their latest message, leaving out inactive ones" <|
            \_ ->
                case decoded of
                    Ok statusPage ->
                        statusPage.incidents
                            |> List.map (\incident -> ( incident.id, incident.level ))
                            |> Expect.equal
                                [ ( "6af207c0f7988a0425ce6537", ServiceDisruption )
                                , ( "6ab576500641a7056be8c598", PartialServiceDisruption )
                                , ( "6ae109842e972f05fd11fab7", PartialServiceDisruption )
                                ]

                    Err e ->
                        Expect.fail (Decode.errorToString e)
        , test "decodes upcoming maintenance with its planned window" <|
            \_ ->
                case decoded of
                    Ok statusPage ->
                        statusPage.upcomingMaintenance
                            |> List.map (\maintenance -> ( maintenance.id, Time.posixToMillis maintenance.plannedStart, Time.posixToMillis maintenance.plannedEnd ))
                            |> Expect.equal [ ( "6aea14750d2fc504258522ef", 1793926800000, 1793948400000 ) ]

                    Err e ->
                        Expect.fail (Decode.errorToString e)
        , test "leaves out maintenance whose planned end has passed" <|
            \_ ->
                case decoded of
                    Ok statusPage ->
                        -- 2026-11-09T18:00:00Z
                        Helpers.StatusPage.maintenanceToShow (Time.millisToPosix 1794247200000) statusPage
                            |> Expect.equal { inProgress = [], upcoming = [] }

                    Err e ->
                        Expect.fail (Decode.errorToString e)
        , test "shows maintenance starting within three days" <|
            \_ ->
                case decoded of
                    Ok statusPage ->
                        -- 2026-11-04T12:00:00Z
                        Helpers.StatusPage.maintenanceToShow (Time.millisToPosix 1793793600000) statusPage
                            |> .upcoming
                            |> List.map .id
                            |> Expect.equal [ "6aea14750d2fc504258522ef" ]

                    Err e ->
                        Expect.fail (Decode.errorToString e)
        , test "counts upcoming maintenance as in progress from its planned start" <|
            \_ ->
                case decoded of
                    Ok statusPage ->
                        let
                            -- 2026-11-06T03:00:00Z, during the planned window, while status.io still lists it as upcoming
                            { inProgress, upcoming } =
                                Helpers.StatusPage.maintenanceToShow (Time.millisToPosix 1793934000000) statusPage
                        in
                        ( List.map .id inProgress, List.map .id upcoming )
                            |> Expect.equal ( [ "6aea14750d2fc504258522ef" ], [] )

                    Err e ->
                        Expect.fail (Decode.errorToString e)
        , test "keeps active maintenance in progress past its planned end" <|
            \_ ->
                case decoded of
                    Ok statusPage ->
                        -- 2026-11-09T18:00:00Z
                        { statusPage | activeMaintenance = statusPage.upcomingMaintenance, upcomingMaintenance = [] }
                            |> Helpers.StatusPage.maintenanceToShow (Time.millisToPosix 1794247200000)
                            |> .inProgress
                            |> List.map .id
                            |> Expect.equal [ "6aea14750d2fc504258522ef" ]

                    Err e ->
                        Expect.fail (Decode.errorToString e)
        , test "decodes a response without incidents or maintenance" <|
            \_ ->
                Decode.decodeString Rest.StatusPage.statusIoDecoder """{ "result": { "status_overall": { "status_code": 100 } } }"""
                    |> Expect.equal (Ok { incidents = [], activeMaintenance = [], upcomingMaintenance = [] })
        ]


statusPageConfigDecoderSuite : Test
statusPageConfigDecoderSuite =
    let
        decode json =
            Decode.decodeString Types.StatusPage.statusPageConfigDecoder json
    in
    describe "statusPageConfigDecoder decodes a cloud config's statusPage and never fails"
        [ test "decodes a status.io page" <|
            \_ ->
                decode """{ "provider": "status.io", "pageId": "61dc808a7e9a82053ce739d2", "url": "https://jetstream.status.io/" }"""
                    |> Expect.equal
                        (Ok (Just { provider = StatusIo, pageId = "61dc808a7e9a82053ce739d2", url = "https://jetstream.status.io/" }))
        , test "an unknown provider means no status page" <|
            \_ ->
                decode """{ "provider": "statuspage.io", "pageId": "abc", "url": "https://status.example.org/" }"""
                    |> Expect.equal (Ok Nothing)
        , test "null means no status page" <|
            \_ ->
                decode "null"
                    |> Expect.equal (Ok Nothing)
        , test "a malformed status page means no status page" <|
            \_ ->
                decode """{ "provider": "status.io", "url": "https://jetstream.status.io/" }"""
                    |> Expect.equal (Ok Nothing)
        ]
