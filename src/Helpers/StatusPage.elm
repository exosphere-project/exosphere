module Helpers.StatusPage exposing
    ( banners
    , isStatusBannerId
    , maintenanceToShow
    , pruneDismissedStatusBanners
    , statusPagesToPoll
    )

import Dict
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.Time
import Helpers.Url as UrlHelpers
import List.Extra
import Set exposing (Set)
import Time
import Types.Banner exposing (Banner, BannerLevel(..), bannerId)
import Types.HelperTypes exposing (KeystoneHostname, Url)
import Types.SharedModel exposing (SharedModel)
import Types.StatusPage exposing (Incident, Maintenance, StatusLevel(..), StatusPage, StatusPageConfig)
import Url


{-| What a cloud's status page says now, in the order to show it: incidents worst first (newest first
among equally bad ones), maintenance in progress, and maintenance starting soon.
-}
type alias CloudStatus =
    { keystoneHostname : KeystoneHostname
    , cloudName : String
    , url : Url
    , incidents : List Incident
    , maintenanceInProgress : List Maintenance
    , maintenanceUpcoming : List Maintenance
    }


{-| How far ahead scheduled maintenance is shown.
-}
upcomingMaintenanceWindowMillis : Int
upcomingMaintenanceWindowMillis =
    3 * 24 * 60 * 60 * 1000


{-| The clouds the user has projects on, plus the cloud a deployment's OpenID Connect login goes to,
so that its status shows on the login page while nobody can log in.
-}
relevantKeystoneHostnames : SharedModel -> List KeystoneHostname
relevantKeystoneHostnames model =
    (List.map (.endpoints >> .keystone) model.projects
        ++ (model.openIdConnectLoginConfig
                |> Maybe.map (.keystoneAuthUrl >> List.singleton)
                |> Maybe.withDefault []
           )
    )
        |> List.map UrlHelpers.hostnameFromUrl
        |> List.Extra.unique


{-| Relevant clouds that have a status page configured.
-}
statusPagesToPoll : SharedModel -> List ( KeystoneHostname, StatusPageConfig )
statusPagesToPoll model =
    relevantKeystoneHostnames model
        |> List.filterMap
            (\keystoneHostname ->
                Dict.get keystoneHostname model.viewContext.cloudSpecificConfigs
                    |> Maybe.andThen .statusPage
                    |> Maybe.map (Tuple.pair keystoneHostname)
            )


cloudStatus : SharedModel -> KeystoneHostname -> Maybe CloudStatus
cloudStatus model keystoneHostname =
    Dict.get keystoneHostname model.viewContext.cloudSpecificConfigs
        |> Maybe.andThen
            (\cloudConfig ->
                Maybe.map2
                    (\statusPageConfig statusPage ->
                        let
                            { inProgress, upcoming } =
                                maintenanceToShow model.clientCurrentTime statusPage
                        in
                        { keystoneHostname = keystoneHostname
                        , cloudName = cloudConfig.friendlyName
                        , url = statusPageConfig.url
                        , incidents =
                            statusPage.incidents
                                |> List.sortBy
                                    (\incident ->
                                        ( negate (severity incident.level)
                                        , negate (Time.posixToMillis incident.openedAt)
                                        )
                                    )
                        , maintenanceInProgress = inProgress
                        , maintenanceUpcoming = upcoming
                        }
                    )
                    cloudConfig.statusPage
                    (Dict.get keystoneHostname model.statusPages |> Maybe.andThen RDPP.toMaybe)
            )


{-| Maintenance worth telling users about. Maintenance the status page lists as active is in progress,
even past its planned end. status.io keeps listing maintenance as upcoming until the operator starts or
closes it, so upcoming maintenance counts as in progress from its planned start, and is left out after
its planned end.
-}
maintenanceToShow : Time.Posix -> StatusPage -> { inProgress : List Maintenance, upcoming : List Maintenance }
maintenanceToShow currentTime statusPage =
    let
        now =
            Time.posixToMillis currentTime

        started maintenance =
            Time.posixToMillis maintenance.plannedStart <= now

        scheduled =
            List.filter (\maintenance -> Time.posixToMillis maintenance.plannedEnd > now) statusPage.upcomingMaintenance
    in
    { inProgress = statusPage.activeMaintenance ++ List.filter started scheduled
    , upcoming =
        List.filter
            (\maintenance -> not (started maintenance) && Time.posixToMillis maintenance.plannedStart <= now + upcomingMaintenanceWindowMillis)
            scheduled
    }


severity : StatusLevel -> Int
severity level =
    case level of
        Informational ->
            0

        PlannedMaintenance ->
            1

        DegradedPerformance ->
            2

        PartialServiceDisruption ->
            3

        ServiceDisruption ->
            4

        SecurityEvent ->
            5


levelLabel : StatusLevel -> String
levelLabel level =
    case level of
        Informational ->
            "Notice"

        PlannedMaintenance ->
            "Maintenance"

        DegradedPerformance ->
            "Degraded performance"

        PartialServiceDisruption ->
            "Partial service disruption"

        ServiceDisruption ->
            "Service disruption"

        SecurityEvent ->
            "Security event"


levelToBannerLevel : StatusLevel -> BannerLevel
levelToBannerLevel level =
    case level of
        Informational ->
            BannerDefault

        PlannedMaintenance ->
            BannerInfo

        DegradedPerformance ->
            BannerWarning

        PartialServiceDisruption ->
            BannerWarning

        ServiceDisruption ->
            BannerDanger

        SecurityEvent ->
            BannerDanger


{-| The status page's hostname, to name it in a link.
-}
statusPageHost : Url -> String
statusPageHost url =
    Url.fromString url
        |> Maybe.map .host
        |> Maybe.withDefault url



-- Banners


{-| Banners for what the status pages of relevant clouds report, shown before the deployer's banners.
-}
banners : SharedModel -> List Banner
banners model =
    relevantKeystoneHostnames model
        |> List.filterMap (cloudStatus model)
        |> List.concatMap (cloudStatusBanners model.clientCurrentTime model.timeZone)


cloudStatusBanners : Time.Posix -> Time.Zone -> CloudStatus -> List Banner
cloudStatusBanners currentTime timeZone status =
    List.map (incidentBanner status) status.incidents
        ++ List.map (maintenanceInProgressBanner currentTime timeZone status) status.maintenanceInProgress
        ++ List.map (upcomingMaintenanceBanner timeZone status) status.maintenanceUpcoming


incidentBanner : CloudStatus -> Incident -> Banner
incidentBanner status incident =
    banner status
        (incident.id ++ ":" ++ levelLabel incident.level)
        (levelToBannerLevel incident.level)
        [ bold (status.cloudName ++ ": " ++ String.toLower (levelLabel incident.level) ++ ".")
        , sentence incident.name
        , detailsLink status
        ]


maintenanceInProgressBanner : Time.Posix -> Time.Zone -> CloudStatus -> Maintenance -> Banner
maintenanceInProgressBanner currentTime timeZone status maintenance =
    banner status
        (maintenance.id ++ ":inProgress")
        BannerInfo
        (if Time.posixToMillis maintenance.plannedEnd > Time.posixToMillis currentTime then
            [ bold (status.cloudName ++ ": maintenance in progress")
            , "until " ++ Helpers.Time.humanReadableDateAndTimeInZone timeZone maintenance.plannedEnd ++ ", your local time."
            , sentence maintenance.name
            , detailsLink status
            ]

         else
            -- It is running past its planned end.
            [ bold (status.cloudName ++ ": maintenance in progress.")
            , sentence maintenance.name
            , detailsLink status
            ]
        )


upcomingMaintenanceBanner : Time.Zone -> CloudStatus -> Maintenance -> Banner
upcomingMaintenanceBanner timeZone status maintenance =
    banner status
        (maintenance.id ++ ":upcoming")
        BannerInfo
        [ bold (status.cloudName ++ ": scheduled maintenance")
        , String.concat
            [ "from "
            , Helpers.Time.humanReadableDateAndTimeInZone timeZone maintenance.plannedStart
            , " to "
            , Helpers.Time.humanReadableDateAndTimeInZone timeZone maintenance.plannedEnd
            , ", your local time."
            ]
        , sentence maintenance.name
        , detailsLink status
        ]


{-| A banner for one incident or maintenance. `itemId` names it and its state, so that a dismissed banner
comes back when the state changes, and not when only its wording does.
-}
banner : CloudStatus -> String -> BannerLevel -> List String -> Banner
banner status itemId level messageParts =
    { message = String.join " " messageParts
    , level = level
    , startsAt = Nothing
    , endsAt = Nothing
    , id = Just (bannerIdPrefix status.keystoneHostname ++ itemId)
    }


{-| Status page banner ids start with their cloud, so that their dismissals can be told apart from those of
the deployer's banners and pruned when that cloud's status page is read.
-}
bannerIdPrefix : KeystoneHostname -> String
bannerIdPrefix keystoneHostname =
    statusBannerIdPrefix ++ keystoneHostname ++ ":"


statusBannerIdPrefix : String
statusBannerIdPrefix =
    "statusPage:"


isStatusBannerId : String -> Bool
isStatusBannerId id =
    String.startsWith statusBannerIdPrefix id


{-| After a cloud's status page is read, forget dismissals of that cloud's banners that are no longer shown.
-}
pruneDismissedStatusBanners : KeystoneHostname -> List Banner -> Set String -> Set String
pruneDismissedStatusBanners keystoneHostname currentBanners dismissedBanners =
    let
        currentIds =
            currentBanners
                |> List.map bannerId
                |> Set.fromList
    in
    Set.filter
        (\id -> not (String.startsWith (bannerIdPrefix keystoneHostname) id) || Set.member id currentIds)
        dismissedBanners


bold : String -> String
bold markdown =
    "**" ++ markdown ++ "**"


detailsLink : CloudStatus -> String
detailsLink status =
    "[Details at " ++ statusPageHost status.url ++ "](" ++ status.url ++ ")"


{-| A name from the status page as a sentence, without doubling punctuation it already ends with.
-}
sentence : String -> String
sentence text =
    if List.any (\punctuation -> String.endsWith punctuation text) [ ".", "!", "?" ] then
        escapeMarkdown text

    else
        escapeMarkdown text ++ "."


{-| Names come from the status page, so characters that markdown would read as formatting are escaped.
-}
escapeMarkdown : String -> String
escapeMarkdown text =
    text
        |> String.toList
        |> List.concatMap
            (\char ->
                if String.contains (String.fromChar char) "\\`*_[]<>~" then
                    [ '\\', char ]

                else
                    [ char ]
            )
        |> String.fromList
