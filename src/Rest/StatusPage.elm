module Rest.StatusPage exposing (requestStatusPage, statusIoDecoder)

import Helpers.Time exposing (makeIso8601StringToPosixDecoder)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import List.Extra
import Rest.Helpers exposing (expectJsonWithErrorBody)
import Time
import Types.HelperTypes exposing (KeystoneHostname)
import Types.SharedMsg exposing (SharedMsg(..))
import Types.StatusPage exposing (Incident, Maintenance, StatusLevel(..), StatusPage, StatusPageConfig, StatusPageProvider(..))
import Url
import Url.Builder


{-| Fetch a cloud's status page. status.io's public status API serves CORS headers, so the browser
reads it directly.
-}
requestStatusPage : KeystoneHostname -> StatusPageConfig -> Cmd SharedMsg
requestStatusPage keystoneHostname config =
    case config.provider of
        StatusIo ->
            Http.get
                { url =
                    Url.Builder.crossOrigin "https://api.status.io"
                        [ "1.0", "status", Url.percentEncode config.pageId ]
                        []
                , expect = expectJsonWithErrorBody (ReceiveStatusPage keystoneHostname) statusIoDecoder
                }


{-| Decodes the response of status.io's public status API, `GET /1.0/status/{pageId}`.
-}
statusIoDecoder : Decode.Decoder StatusPage
statusIoDecoder =
    Decode.field "result"
        (Decode.succeed StatusPage
            |> Pipeline.optional "incidents"
                (Decode.list statusIoIncidentDecoder |> Decode.map (List.filterMap identity))
                []
            |> Pipeline.optionalAt [ "maintenance", "active" ] (Decode.list statusIoMaintenanceDecoder) []
            |> Pipeline.optionalAt [ "maintenance", "upcoming" ] (Decode.list statusIoMaintenanceDecoder) []
        )


{-| An incident, or `Nothing` if status.io marks it inactive. Its current level is the status of its latest message.
-}
statusIoIncidentDecoder : Decode.Decoder (Maybe Incident)
statusIoIncidentDecoder =
    Decode.succeed
        (\id name openedAt messages active ->
            if active then
                Just
                    { id = id
                    , name = name
                    , openedAt = openedAt
                    , level =
                        messages
                            |> List.Extra.maximumBy (Tuple.first >> Time.posixToMillis)
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault Informational
                    }

            else
                Nothing
        )
        |> Pipeline.required "_id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "datetime_open" iso8601Decoder
        |> Pipeline.optional "messages"
            (Decode.list
                (Decode.map2 Tuple.pair
                    (Decode.field "datetime" iso8601Decoder)
                    (Decode.field "status" (Decode.map statusIoLevel Decode.int))
                )
            )
            []
        |> Pipeline.optional "current_active" Decode.bool True


statusIoMaintenanceDecoder : Decode.Decoder Maintenance
statusIoMaintenanceDecoder =
    Decode.succeed Maintenance
        |> Pipeline.required "_id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "datetime_planned_start" iso8601Decoder
        |> Pipeline.required "datetime_planned_end" iso8601Decoder


statusIoLevel : Int -> StatusLevel
statusIoLevel statusCode =
    case statusCode of
        200 ->
            PlannedMaintenance

        300 ->
            DegradedPerformance

        400 ->
            PartialServiceDisruption

        500 ->
            ServiceDisruption

        600 ->
            SecurityEvent

        _ ->
            -- 100 is "Operational"; anything else is a status code we don't know.
            Informational


iso8601Decoder : Decode.Decoder Time.Posix
iso8601Decoder =
    Decode.string |> Decode.andThen makeIso8601StringToPosixDecoder
