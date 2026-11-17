module Types.StatusPage exposing
    ( Incident
    , Maintenance
    , StatusLevel(..)
    , StatusPage
    , StatusPageConfig
    , StatusPageProvider(..)
    , statusPageConfigDecoder
    )

{-| A cloud's public status page, as configured per cloud by the deployer and as last read from it.
-}

import Json.Decode as Decode
import Time


{-| Status page services Exosphere knows how to read.
-}
type StatusPageProvider
    = StatusIo


type alias StatusPageConfig =
    { provider : StatusPageProvider
    , pageId : String
    , url : String
    }


{-| How bad things are, from least to most severe.
-}
type StatusLevel
    = Informational
    | PlannedMaintenance
    | DegradedPerformance
    | PartialServiceDisruption
    | ServiceDisruption
    | SecurityEvent


type alias Incident =
    { id : String
    , name : String
    , openedAt : Time.Posix
    , level : StatusLevel
    }


type alias Maintenance =
    { id : String
    , name : String
    , plannedStart : Time.Posix
    , plannedEnd : Time.Posix
    }


type alias StatusPage =
    { incidents : List Incident
    , activeMaintenance : List Maintenance
    , upcomingMaintenance : List Maintenance
    }


{-| Decodes the optional `statusPage` member of a cloud config. This never fails, so a status page
config that is malformed or names a provider Exosphere does not support means the cloud has no status
page, rather than breaking the decoding of every cloud config.
-}
statusPageConfigDecoder : Decode.Decoder (Maybe StatusPageConfig)
statusPageConfigDecoder =
    Decode.oneOf
        [ Decode.map3
            (\provider pageId url ->
                Maybe.map (\provider_ -> StatusPageConfig provider_ pageId url) provider
            )
            (Decode.field "provider" (Decode.map providerFromString Decode.string))
            (Decode.field "pageId" Decode.string)
            (Decode.field "url" Decode.string)
        , Decode.succeed Nothing
        ]


providerFromString : String -> Maybe StatusPageProvider
providerFromString provider =
    case provider of
        "status.io" ->
            Just StatusIo

        _ ->
            Nothing
