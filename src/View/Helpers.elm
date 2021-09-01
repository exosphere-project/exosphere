module View.Helpers exposing
    ( browserLink
    , compactKVRow
    , compactKVSubRow
    , contentContainer
    , createdAgoByFrom
    , edges
    , exoColumnAttributes
    , exoElementAttributes
    , exoPaddingSpacingAttributes
    , exoRowAttributes
    , featuredImageNamePrefixLookup
    , formContainer
    , friendlyProjectTitle
    , getExoSetupStatusStr
    , getServerUiStatus
    , getServerUiStatusBadgeState
    , getServerUiStatusStr
    , heading2
    , heading3
    , heading4
    , hint
    , imageExcludeFilterLookup
    , inputItemAttributes
    , possiblyUntitledResource
    , renderMarkdown
    , renderMessageAsElement
    , renderMessageAsString
    , renderRDPP
    , renderWebData
    , serverStatusBadge
    , sortProjects
    , titleFromHostname
    , toExoPalette
    , toViewContext
    , userAppProxyLookup
    )

import Color
import DateFormat.Relative
import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Element.Region as Region
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.Time exposing (humanReadableTime)
import Helpers.Url as UrlHelpers
import Html
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import OpenStack.Types as OSTypes
import Regex
import RemoteData
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.StatusBadge as StatusBadge
import Style.Widgets.ToggleTip
import Time
import Types.Error exposing (ErrorLevel(..), toFriendlyErrorLevel)
import Types.HelperTypes
import Types.Project exposing (Project)
import Types.Server exposing (ExoSetupStatus(..), Server, ServerOrigin(..), ServerUiStatus(..))
import Types.SharedModel exposing (LogMessage, SharedModel, Style)
import View.Types
import Widget


toViewContext : SharedModel -> View.Types.Context
toViewContext model =
    { palette = toExoPalette model.style
    , localization = model.style.localization
    , windowSize = model.windowSize
    , cloudSpecificConfigs = model.cloudSpecificConfigs
    , experimentalFeaturesEnabled = model.experimentalFeaturesEnabled
    , urlPathPrefix = model.urlPathPrefix
    , navigationKey = model.navigationKey
    }


toExoPalette : Style -> ExoPalette
toExoPalette style =
    SH.toExoPalette style.primaryColor style.secondaryColor style.styleMode



{- Elm UI Doodads -}


exoRowAttributes : List (Element.Attribute msg)
exoRowAttributes =
    exoElementAttributes


exoColumnAttributes : List (Element.Attribute msg)
exoColumnAttributes =
    exoElementAttributes


exoElementAttributes : List (Element.Attribute msg)
exoElementAttributes =
    exoPaddingSpacingAttributes


exoPaddingSpacingAttributes : List (Element.Attribute msg)
exoPaddingSpacingAttributes =
    [ Element.padding 10
    , Element.spacing 10
    ]


inputItemAttributes : Color.Color -> List (Element.Attribute msg)
inputItemAttributes backgroundColor =
    [ Element.width Element.fill
    , Element.spacing 12
    , Background.color <| SH.toElementColor <| backgroundColor
    ]


heading2 : ExoPalette -> List (Element.Attribute msg)
heading2 palette =
    [ Region.heading 2
    , Font.bold
    , Font.size 24
    , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
    , Border.color (palette.muted |> SH.toElementColor)
    , Element.width Element.fill
    , Element.paddingEach { bottom = 8, left = 0, right = 0, top = 0 }
    ]


heading3 : ExoPalette -> List (Element.Attribute msg)
heading3 palette =
    [ Region.heading 3
    , Font.bold
    , Font.size 20
    , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
    , Border.color (palette.muted |> SH.toElementColor)
    , Element.width Element.fill
    , Element.paddingEach { bottom = 8, left = 0, right = 0, top = 0 }
    ]


heading4 : List (Element.Attribute msg)
heading4 =
    [ Region.heading 4
    , Font.bold
    , Font.size 16
    , Element.width Element.fill
    ]


contentContainer : List (Element.Attribute msg)
contentContainer =
    -- Keeps the width from getting too wide for single column
    [ Element.width (Element.maximum 900 Element.fill)
    , Element.spacing 15
    , Element.paddingXY 0 10
    ]


formContainer : List (Element.Attribute msg)
formContainer =
    -- Keeps form fields from displaying too wide
    [ Element.width (Element.maximum 600 Element.fill)
    , Element.spacing 15
    , Element.paddingXY 0 10
    ]


compactKVRow : String -> Element.Element msg -> Element.Element msg
compactKVRow key value =
    Element.row
        (exoRowAttributes ++ [ Element.padding 0, Element.spacing 10 ])
        [ Element.paragraph [ Element.alignTop, Element.width (Element.px 200), Font.bold ] [ Element.text key ]
        , value
        ]


compactKVSubRow : String -> Element.Element msg -> Element.Element msg
compactKVSubRow key value =
    Element.row
        (exoRowAttributes ++ [ Element.padding 0, Element.spacing 10, Font.size 14 ])
        [ Element.paragraph [ Element.width (Element.px 175), Font.bold ] [ Element.text key ]
        , Element.el [] value
        ]


type alias Edges =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


edges : Edges
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


hint : View.Types.Context -> String -> Element.Attribute msg
hint context hintText =
    Element.below
        (Element.el
            [ Font.color (context.palette.error |> SH.toElementColor)
            , Font.size 14
            , Element.alignRight
            , Element.moveDown 6
            ]
            (Element.text hintText)
        )


renderMessageAsElement : View.Types.Context -> LogMessage -> Element.Element msg
renderMessageAsElement context message =
    let
        levelColor : ErrorLevel -> Element.Color
        levelColor errLevel =
            case errLevel of
                ErrorDebug ->
                    context.palette.readyGood |> SH.toElementColor

                ErrorInfo ->
                    context.palette.on.background |> SH.toElementColor

                ErrorWarn ->
                    context.palette.warn |> SH.toElementColor

                ErrorCrit ->
                    context.palette.error |> SH.toElementColor
    in
    Element.column (exoColumnAttributes ++ [ Element.spacing 13 ])
        [ Element.row [ Element.alignRight ]
            [ Element.el
                [ Font.color <| levelColor message.context.level
                , Font.bold
                ]
                (Element.text
                    (toFriendlyErrorLevel message.context.level)
                )
            , Element.el [ context.palette.muted |> SH.toElementColor |> Font.color ]
                (Element.text
                    (" at " ++ humanReadableTime message.timestamp)
                )
            ]
        , compactKVRow "We were trying to"
            (Element.paragraph [] [ Element.text message.context.actionContext ])
        , compactKVRow "Message"
            (Element.paragraph [] [ Element.text message.message ])
        , case message.context.recoveryHint of
            Just hint_ ->
                compactKVRow "Recovery hint" (Element.paragraph [] [ Element.text hint_ ])

            Nothing ->
                Element.none
        ]


renderMessageAsString : LogMessage -> String
renderMessageAsString message =
    let
        levelStr : ErrorLevel -> String
        levelStr errLevel =
            case errLevel of
                ErrorDebug ->
                    "DEBUG"

                ErrorInfo ->
                    "INFO"

                ErrorWarn ->
                    "WARN"

                ErrorCrit ->
                    "CRITICAL"
    in
    [ levelStr message.context.level
    , " at "
    , humanReadableTime message.timestamp
    , " -- while trying to "
    , message.context.actionContext
    , " -- "
    , message.message
    ]
        |> String.concat


browserLink : View.Types.Context -> Types.HelperTypes.Url -> View.Types.BrowserLinkLabel msg -> Element.Element msg
browserLink context url label =
    let
        linkAttribs =
            [ context.palette.primary |> SH.toElementColor |> Font.color
            , Font.underline
            , Element.pointer
            ]

        renderedLabel =
            case label of
                View.Types.BrowserLinkTextLabel str ->
                    { attribs = linkAttribs
                    , contents = Element.text str
                    }

                View.Types.BrowserLinkFancyLabel el ->
                    { attribs = []
                    , contents = el
                    }
    in
    Element.newTabLink
        renderedLabel.attribs
        { url = url
        , label = renderedLabel.contents
        }


possiblyUntitledResource : String -> String -> String
possiblyUntitledResource name resourceType =
    case name of
        "" ->
            "(Untitled " ++ resourceType ++ ")"

        _ ->
            name


titleFromHostname : String -> String
titleFromHostname hostname =
    let
        r =
            Helpers.alwaysRegex "^(.*?)\\..*"

        matches =
            Regex.findAtMost 1 r hostname

        maybeMaybeTitle =
            matches
                |> List.head
                |> Maybe.map (\x -> x.submatches)
                |> Maybe.andThen List.head
    in
    case maybeMaybeTitle of
        Just (Just title) ->
            title

        _ ->
            hostname


loadingStuff : View.Types.Context -> String -> Element.Element msg
loadingStuff context resourceWord =
    Element.row [ Element.spacing 15 ]
        [ Widget.circularProgressIndicator
            (SH.materialStyle context.palette).progressIndicator
            Nothing
        , Element.text <|
            String.concat
                [ "Loading "
                , resourceWord
                , "..."
                ]
        ]


renderWebData : View.Types.Context -> RemoteData.WebData a -> String -> (a -> Element.Element msg) -> Element.Element msg
renderWebData context remoteData resourceWord renderSuccessCase =
    case remoteData of
        RemoteData.NotAsked ->
            -- This is an ugly hack because some of our API calls don't set RemoteData to "Loading" when they should.
            loadingStuff context resourceWord

        RemoteData.Loading ->
            loadingStuff context resourceWord

        RemoteData.Failure error ->
            Element.text <|
                String.join " "
                    [ "Could not load"
                    , resourceWord
                    , "because:"
                    , Helpers.httpErrorToString error
                    ]

        RemoteData.Success resource ->
            renderSuccessCase resource


renderRDPP : View.Types.Context -> RDPP.RemoteDataPlusPlus Types.Error.HttpErrorWithBody a -> String -> (a -> Element.Element msg) -> Element.Element msg
renderRDPP context remoteData resourceWord renderSuccessCase =
    case remoteData.data of
        RDPP.DoHave data _ ->
            renderSuccessCase data

        RDPP.DontHave ->
            case remoteData.refreshStatus of
                RDPP.Loading ->
                    loadingStuff context resourceWord

                RDPP.NotLoading maybeErrorTuple ->
                    case maybeErrorTuple of
                        Just ( error, _ ) ->
                            Element.text <|
                                String.join " "
                                    [ "Could not load"
                                    , resourceWord
                                    , "because:"
                                    , Helpers.httpErrorWithBodyToString error
                                    ]

                        Nothing ->
                            loadingStuff context resourceWord


serverStatusBadge : Style.Types.ExoPalette -> Server -> Element.Element msg
serverStatusBadge palette server =
    let
        contents =
            server |> getServerUiStatus |> getServerUiStatusStr |> Element.text
    in
    StatusBadge.statusBadge
        palette
        (server |> getServerUiStatus |> getServerUiStatusBadgeState)
        contents


getServerUiStatus : Server -> ServerUiStatus
getServerUiStatus server =
    let
        maybeFirstTargetStatus =
            server.exoProps.targetOpenstackStatus
                |> Maybe.andThen List.head

        targetStatusActive =
            maybeFirstTargetStatus == Just OSTypes.ServerActive
    in
    case server.osProps.details.openstackStatus of
        OSTypes.ServerActive ->
            let
                whenNoTargetStatus =
                    case server.exoProps.serverOrigin of
                        ServerFromExo serverFromExoProps ->
                            if serverFromExoProps.exoServerVersion < 4 then
                                ServerUiStatusReady

                            else
                                case serverFromExoProps.exoSetupStatus.data of
                                    RDPP.DoHave status _ ->
                                        case status of
                                            ExoSetupWaiting ->
                                                ServerUiStatusBuilding

                                            ExoSetupRunning ->
                                                ServerUiStatusRunningSetup

                                            ExoSetupComplete ->
                                                ServerUiStatusReady

                                            ExoSetupError ->
                                                ServerUiStatusError

                                            ExoSetupTimeout ->
                                                ServerUiStatusError

                                            ExoSetupUnknown ->
                                                ServerUiStatusUnknown

                                    RDPP.DontHave ->
                                        ServerUiStatusUnknown

                        ServerNotFromExo ->
                            ServerUiStatusReady
            in
            case maybeFirstTargetStatus of
                Just OSTypes.ServerSuspended ->
                    ServerUiStatusSuspending

                Just OSTypes.ServerShelved ->
                    ServerUiStatusShelving

                Just OSTypes.ServerShelvedOffloaded ->
                    ServerUiStatusShelving

                Just OSTypes.ServerSoftDeleted ->
                    ServerUiStatusDeleting

                Just OSTypes.ServerDeleted ->
                    ServerUiStatusDeleting

                _ ->
                    whenNoTargetStatus

        OSTypes.ServerPaused ->
            if targetStatusActive then
                ServerUiStatusUnpausing

            else
                ServerUiStatusPaused

        OSTypes.ServerReboot ->
            ServerUiStatusRebooting

        OSTypes.ServerSuspended ->
            if targetStatusActive then
                ServerUiStatusResuming

            else
                ServerUiStatusSuspended

        OSTypes.ServerShutoff ->
            if targetStatusActive then
                ServerUiStatusStarting

            else
                ServerUiStatusShutoff

        OSTypes.ServerStopped ->
            if targetStatusActive then
                ServerUiStatusStarting

            else
                ServerUiStatusStopped

        OSTypes.ServerSoftDeleted ->
            ServerUiStatusSoftDeleted

        OSTypes.ServerError ->
            ServerUiStatusError

        OSTypes.ServerBuilding ->
            ServerUiStatusBuilding

        OSTypes.ServerRescued ->
            ServerUiStatusRescued

        OSTypes.ServerShelved ->
            if targetStatusActive then
                ServerUiStatusUnshelving

            else
                ServerUiStatusShelved

        OSTypes.ServerShelvedOffloaded ->
            if targetStatusActive then
                ServerUiStatusUnshelving

            else
                ServerUiStatusShelved

        OSTypes.ServerDeleted ->
            ServerUiStatusDeleted


getExoSetupStatusStr : Server -> Maybe String
getExoSetupStatusStr server =
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            Nothing

        ServerFromExo exoOriginProps ->
            case exoOriginProps.exoSetupStatus.data of
                RDPP.DoHave exoSetupStatus _ ->
                    case exoSetupStatus of
                        ExoSetupWaiting ->
                            Just "Waiting"

                        ExoSetupRunning ->
                            Just "Running"

                        ExoSetupComplete ->
                            Just "Complete"

                        ExoSetupError ->
                            Just "Error"

                        ExoSetupTimeout ->
                            Just "Timeout"

                        ExoSetupUnknown ->
                            Just "Unknown"

                RDPP.DontHave ->
                    Nothing


getServerUiStatusStr : ServerUiStatus -> String
getServerUiStatusStr status =
    case status of
        ServerUiStatusUnknown ->
            "Unknown"

        ServerUiStatusBuilding ->
            "Building"

        ServerUiStatusRunningSetup ->
            "Running Setup"

        ServerUiStatusReady ->
            "Ready"

        ServerUiStatusPaused ->
            "Paused"

        ServerUiStatusUnpausing ->
            "Unpausing"

        ServerUiStatusRebooting ->
            "Rebooting"

        ServerUiStatusSuspending ->
            "Suspending"

        ServerUiStatusSuspended ->
            "Suspended"

        ServerUiStatusResuming ->
            "Resuming"

        ServerUiStatusShutoff ->
            "Shut off"

        ServerUiStatusStopped ->
            "Stopped"

        ServerUiStatusStarting ->
            "Starting"

        ServerUiStatusDeleting ->
            "Deleting"

        ServerUiStatusSoftDeleted ->
            "Soft-deleted"

        ServerUiStatusError ->
            "Error"

        ServerUiStatusRescued ->
            "Rescued"

        ServerUiStatusShelving ->
            "Shelving"

        ServerUiStatusShelved ->
            "Shelved"

        ServerUiStatusUnshelving ->
            "Unshelving"

        ServerUiStatusDeleted ->
            "Deleted"


getServerUiStatusBadgeState : ServerUiStatus -> StatusBadge.StatusBadgeState
getServerUiStatusBadgeState status =
    case status of
        ServerUiStatusUnknown ->
            StatusBadge.Muted

        ServerUiStatusBuilding ->
            StatusBadge.Warning

        ServerUiStatusRunningSetup ->
            StatusBadge.Warning

        ServerUiStatusReady ->
            StatusBadge.ReadyGood

        ServerUiStatusRebooting ->
            StatusBadge.Warning

        ServerUiStatusPaused ->
            StatusBadge.Muted

        ServerUiStatusUnpausing ->
            StatusBadge.Warning

        ServerUiStatusSuspending ->
            StatusBadge.Muted

        ServerUiStatusSuspended ->
            StatusBadge.Muted

        ServerUiStatusResuming ->
            StatusBadge.Warning

        ServerUiStatusShutoff ->
            StatusBadge.Muted

        ServerUiStatusStopped ->
            StatusBadge.Muted

        ServerUiStatusStarting ->
            StatusBadge.Warning

        ServerUiStatusDeleting ->
            StatusBadge.Muted

        ServerUiStatusSoftDeleted ->
            StatusBadge.Muted

        ServerUiStatusError ->
            StatusBadge.Error

        ServerUiStatusRescued ->
            StatusBadge.Error

        ServerUiStatusShelving ->
            StatusBadge.Muted

        ServerUiStatusShelved ->
            StatusBadge.Muted

        ServerUiStatusUnshelving ->
            StatusBadge.Warning

        ServerUiStatusDeleted ->
            StatusBadge.Muted


renderMarkdown : View.Types.Context -> String -> List (Element.Element msg)
renderMarkdown context markdown =
    let
        deadEndsToString deadEnds =
            deadEnds
                |> List.map Markdown.Parser.deadEndToString
                |> String.join "\n"

        result =
            markdown
                |> Markdown.Parser.parse
                |> Result.mapError deadEndsToString
                |> Result.andThen
                    (\ast -> Markdown.Renderer.render (elmUiRenderer context) ast)
    in
    case result of
        Ok elements ->
            elements

        Err errors ->
            [ Element.text
                ("Error parsing markdown: \n" ++ errors)
            ]


elmUiRenderer : View.Types.Context -> Markdown.Renderer.Renderer (Element.Element msg)
elmUiRenderer context =
    -- Heavily borrowed and modified from https://ellie-app.com/bQLgjtbgdkZa1
    { heading = heading context.palette
    , paragraph =
        Element.paragraph
            []
    , thematicBreak = Element.none
    , text = Element.text
    , strong = \content -> Element.row [ Font.bold ] content
    , emphasis = \content -> Element.row [ Font.italic ] content
    , codeSpan =
        -- TODO implement this (show fixed-width font) once we need it
        Element.text
    , link =
        \{ destination } body ->
            browserLink
                context
                destination
                (View.Types.BrowserLinkFancyLabel
                    (Element.paragraph
                        [ context.palette.primary |> SH.toElementColor |> Font.color
                        , Font.underline
                        , Element.pointer
                        ]
                        body
                    )
                )
    , hardLineBreak = Html.br [] [] |> Element.html |> Element.el []
    , image =
        \image ->
            case image.title of
                Just _ ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.column
                [ Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Border.color (SH.toElementColor context.palette.on.background)
                , Background.color (SH.toElementColor context.palette.surface)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\(Markdown.Block.ListItem task children) ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row
                                    [ Element.alignTop ]
                                    ((case task of
                                        Markdown.Block.IncompleteTask ->
                                            Element.Input.defaultCheckbox False

                                        Markdown.Block.CompletedTask ->
                                            Element.Input.defaultCheckbox True

                                        Markdown.Block.NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock =
        -- TODO implement this (show fixed-width font) once we need it
        \{ body } ->
            Element.text body
    , html = Markdown.Html.oneOf []
    , table = Element.column []
    , tableHeader = Element.column []
    , tableBody = Element.column []
    , tableRow = Element.row []
    , tableHeaderCell =
        \_ children ->
            Element.paragraph [] children
    , tableCell =
        \_ children ->
            Element.paragraph [] children
    }


heading :
    ExoPalette
    ->
        { level : Markdown.Block.HeadingLevel
        , rawText : String
        , children : List (Element.Element msg)
        }
    -> Element.Element msg
heading exoPalette { level, children } =
    Element.paragraph
        (case level of
            Markdown.Block.H2 ->
                heading2 exoPalette

            Markdown.Block.H3 ->
                heading3 exoPalette

            Markdown.Block.H4 ->
                heading4

            _ ->
                heading2 exoPalette
        )
        children


sortProjects : List Types.HelperTypes.UnscopedProviderProject -> List Types.HelperTypes.UnscopedProviderProject
sortProjects projects =
    let
        projectComparator a b =
            compare a.project.name b.project.name
    in
    projects
        |> List.sortWith projectComparator


friendlyProjectTitle : SharedModel -> Project -> String
friendlyProjectTitle model project =
    -- If we have multiple projects on the same provider then append the project name to the provider name
    let
        providerTitle =
            project.endpoints.keystone
                |> UrlHelpers.hostnameFromUrl
                |> titleFromHostname

        multipleProjects =
            let
                projectCountOnSameProvider =
                    let
                        projectsOnSameProvider : Project -> Project -> Bool
                        projectsOnSameProvider proj1 proj2 =
                            UrlHelpers.hostnameFromUrl proj1.endpoints.keystone == UrlHelpers.hostnameFromUrl proj2.endpoints.keystone
                    in
                    List.filter (projectsOnSameProvider project) model.projects
                        |> List.length
            in
            projectCountOnSameProvider > 1
    in
    if multipleProjects then
        providerTitle ++ " (" ++ project.auth.project.name ++ ")"

    else
        providerTitle


createdAgoByFrom :
    View.Types.Context
    -> Time.Posix
    -> Time.Posix
    -> Maybe ( String, String )
    -> Maybe ( String, String )
    -> Bool
    -> msg
    -> Element.Element msg
createdAgoByFrom context currentTime createdTime maybeWhoCreatedTuple maybeFromTuple showToggleTip showHideTipMsg =
    let
        timeDistanceStr =
            DateFormat.Relative.relativeTime currentTime createdTime

        createdTimeFormatted =
            Helpers.Time.humanReadableTime createdTime

        muted =
            Font.color (context.palette.muted |> SH.toElementColor)

        separator =
            Element.el
                [ Element.paddingXY 10 0
                , muted
                ]
                (Element.text "/")
    in
    Element.wrappedRow [ Element.spacingXY 0 10 ] <|
        [ Element.row []
            [ Element.el [ muted ] (Element.text "Created ")
            , Element.text timeDistanceStr
            , Style.Widgets.ToggleTip.toggleTip
                context.palette
                (Element.text createdTimeFormatted)
                showToggleTip
                showHideTipMsg
            ]
        , case maybeWhoCreatedTuple of
            Just ( creatorAdjective, whoCreated ) ->
                Element.row []
                    [ separator
                    , Element.el [ muted ] (Element.text <| "by " ++ creatorAdjective ++ " ")
                    , Element.text whoCreated
                    ]

            Nothing ->
                Element.none
        , case maybeFromTuple of
            Just ( fromAdjective, whereFrom ) ->
                Element.row []
                    [ separator
                    , Element.el [ muted ] (Element.text <| "from " ++ fromAdjective ++ " ")
                    , Element.text whereFrom
                    ]

            Nothing ->
                Element.none
        ]


imageExcludeFilterLookup : View.Types.Context -> Project -> Maybe Types.HelperTypes.ExcludeFilter
imageExcludeFilterLookup context project =
    let
        projectKeystoneHostname =
            UrlHelpers.hostnameFromUrl project.endpoints.keystone
    in
    Dict.get projectKeystoneHostname context.cloudSpecificConfigs
        |> Maybe.andThen (\csc -> csc.imageExcludeFilter)


featuredImageNamePrefixLookup : View.Types.Context -> Project -> Maybe String
featuredImageNamePrefixLookup context project =
    let
        projectKeystoneHostname =
            UrlHelpers.hostnameFromUrl project.endpoints.keystone
    in
    Dict.get projectKeystoneHostname context.cloudSpecificConfigs
        |> Maybe.andThen (\csc -> csc.featuredImageNamePrefix)


userAppProxyLookup : View.Types.Context -> Project -> Maybe Types.HelperTypes.UserAppProxyHostname
userAppProxyLookup context project =
    let
        projectKeystoneHostname =
            UrlHelpers.hostnameFromUrl project.endpoints.keystone
    in
    Dict.get projectKeystoneHostname context.cloudSpecificConfigs
        |> Maybe.andThen (\csc -> csc.userAppProxy)
