module View.Helpers exposing
    ( Edges
    , compactKVRow
    , compactKVSubRow
    , contentContainer
    , createdAgoByFromSize
    , directionOptions
    , edges
    , ellipsizedText
    , etherTypeOptions
    , extendedResourceName
    , featuredImageNamePrefixLookup
    , flavorPicker
    , formContainer
    , friendlyCloudName
    , friendlyProjectTitle
    , getExoSetupStatusStr
    , getServerUiStatus
    , getServerUiStatusBadgeState
    , getServerUiStatusStr
    , headerHeadingAttributes
    , hint
    , inputItemAttributes
    , invalidInputAttributes
    , loginPickerButton
    , portRangeBoundsOptions
    , portRangeBoundsToString
    , protocolOptions
    , radioLabelAttributes
    , remoteOptions
    , remoteToRemoteType
    , remoteToStringInput
    , remoteTypeToString
    , renderMarkdown
    , renderMaybe
    , renderMessageAsElement
    , renderMessageAsString
    , renderRDPP
    , requiredLabel
    , resourceName
    , securityGroupTypeLabel
    , serverStatusBadge
    , serverStatusBadgeFromStatus
    , shareStatusBadge
    , sortProjects
    , stringToPortRangeBounds
    , stringToRemoteType
    , titleFromHostname
    , toExoPalette
    , validInputAttributes
    , volumeStatusBadgeFromStatus
    , warningInputAttributes
    )

import Color
import Css
import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Element.Region as Region
import FeatherIcons as Icons
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (humanCount)
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.Jetstream2
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String exposing (toTitleCase)
import Helpers.Time exposing (humanReadableDateAndTime)
import Helpers.Url as UrlHelpers
import Html
import Html.Styled
import Html.Styled.Attributes
import List.Extra
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import OpenStack.Quotas as OSQuotas
import OpenStack.SecurityGroupRule exposing (Remote(..), SecurityGroupRuleDirection(..), SecurityGroupRuleEthertype(..), SecurityGroupRuleProtocol(..), directionToString, etherTypeToString, protocolToString)
import OpenStack.Types as OSTypes exposing (ShareStatus(..), VolumeStatus(..))
import Regex
import Route
import String.Extra
import Style.Helpers as SH
import Style.Types as ST exposing (ExoPalette)
import Style.Widgets.Button as Button
import Style.Widgets.Code exposing (codeBlock, codeSpan)
import Style.Widgets.CopyableText exposing (copyableTextAccessory)
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Link as Link
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.StatusBadge as StatusBadge exposing (StatusBadgeSize)
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip as ToggleTip
import Types.Error exposing (ErrorLevel(..), toFriendlyErrorLevel)
import Types.HelperTypes exposing (Localization)
import Types.Project exposing (Project)
import Types.Server exposing (ExoSetupStatus(..), Server, ServerOrigin(..), ServerUiStatus(..))
import Types.SharedModel exposing (LogMessage, SharedModel, Style)
import Types.SharedMsg as SharedMsg
import View.Types exposing (PortRangeBounds(..), RemoteType(..))
import Widget


toExoPalette : Style -> ExoPalette
toExoPalette style =
    SH.toExoPalette style.deployerColors style.styleMode


inputItemAttributes : ExoPalette -> List (Element.Attribute msg)
inputItemAttributes palette =
    [ Element.width Element.fill
    , Element.spacing spacer.px12
    , Background.color <| SH.toElementColor palette.neutral.background.frontLayer
    , Border.color <| SH.toElementColor palette.neutral.border
    ]


headerHeadingAttributes : List (Element.Attribute msg)
headerHeadingAttributes =
    [ Border.width 0
    , Element.padding 0
    , Element.width Element.shrink
    ]


heading2 : ExoPalette -> List (Element.Attribute msg)
heading2 palette =
    Text.headingStyleAttrs palette
        ++ Text.typographyAttrs Text.ExtraLarge


heading3 : ExoPalette -> List (Element.Attribute msg)
heading3 palette =
    Text.subheadingStyleAttrs palette
        ++ Text.typographyAttrs Text.Large


heading4 : List (Element.Attribute msg)
heading4 =
    Text.typographyAttrs Text.Emphasized
        ++ [ Region.heading 4
           , Element.width Element.fill
           ]


contentContainer : List (Element.Attribute msg)
contentContainer =
    -- Keeps the width from getting too wide for single column
    [ Element.width (Element.maximum 900 Element.fill)
    , Element.spacing spacer.px24
    ]


formContainer : List (Element.Attribute msg)
formContainer =
    -- Keeps form fields from displaying too wide
    [ Element.width (Element.maximum 600 Element.fill)
    , Element.spacing spacer.px24
    ]


compactKVRow : String -> Element.Element msg -> Element.Element msg
compactKVRow key value =
    Element.row
        [ Element.padding 0, Element.spacing spacer.px12 ]
        [ Element.paragraph [ Element.alignTop, Element.width (Element.px 200) ] [ Text.strong key ]
        , value
        ]


compactKVSubRow : String -> Element.Element msg -> Element.Element msg
compactKVSubRow key value =
    Element.row
        [ Element.padding 0, Element.spacing spacer.px12, Text.fontSize Text.Body ]
        [ Element.paragraph [ Element.width (Element.px 175) ] [ Text.strong key ]
        , Element.el [ Element.width Element.fill ] value
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


ellipsizedText : String -> Element.Element msg
ellipsizedText text =
    -- from https://elmlang.slack.com/archives/C4F9NBLR1/p1635861051191700?thread_ts=1635806498.188700&cid=C4F9NBLR1
    -- If this still overflows its container, then the outer-most container that's overflowing its parent needs
    -- `Html.Attributes.style "min-width" "0"`
    -- per https://stackoverflow.com/a/53784508
    let
        elmUiElement_text_classes : String
        elmUiElement_text_classes =
            "s t wf hf"
    in
    Html.Styled.span
        [ [ Css.textOverflow Css.ellipsis
          , Css.overflow Css.hidden
          , Css.width (Css.pct 100)
          , Css.lineHeight (Css.num 1.3)
          ]
            |> Html.Styled.Attributes.css
        , Html.Styled.Attributes.class elmUiElement_text_classes
        ]
        [ Html.Styled.text text ]
        |> Html.Styled.toUnstyled
        |> Element.html


hint : View.Types.Context -> String -> Element.Attribute msg
hint context hintText =
    Element.below
        (Element.el
            [ Font.color (context.palette.danger.textOnNeutralBG |> SH.toElementColor)
            , Text.fontSize Text.Small
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
                    context.palette.success.textOnNeutralBG |> SH.toElementColor

                ErrorInfo ->
                    context.palette.info.textOnNeutralBG |> SH.toElementColor

                ErrorWarn ->
                    context.palette.warning.textOnNeutralBG |> SH.toElementColor

                ErrorCrit ->
                    context.palette.danger.textOnNeutralBG |> SH.toElementColor

        copyable =
            copyableTextAccessory context.palette message.message
    in
    Element.column [ Element.spacing spacer.px12, Element.width Element.fill ]
        [ Element.row [ Element.alignRight ]
            [ Element.el
                [ Font.color <| levelColor message.context.level
                ]
                (Text.strong
                    (toFriendlyErrorLevel message.context.level)
                )
            , Element.el [ context.palette.neutral.text.subdued |> SH.toElementColor |> Font.color ]
                (Element.text
                    (" at " ++ humanReadableDateAndTime message.timestamp)
                )
            ]
        , compactKVRow "We were trying to"
            (Element.paragraph [] [ Element.text message.context.actionContext ])
        , compactKVRow "Message"
            (Element.row
                [ Element.width Element.fill, Element.spacing spacer.px8 ]
                [ Element.paragraph [ copyable.id ] [ Element.text message.message ], copyable.accessory ]
            )
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
    , humanReadableDateAndTime message.timestamp
    , " -- while trying to "
    , message.context.actionContext
    , " -- "
    , message.message
    ]
        |> String.concat


resourceName : Maybe String -> String -> String
resourceName maybeName uuid =
    case maybeName of
        Nothing ->
            shortenUuid uuid

        Just "" ->
            shortenUuid uuid

        Just name ->
            name


extendedResourceName : Maybe String -> String -> String -> String
extendedResourceName maybeName uuid resourceType =
    case maybeName of
        Nothing ->
            shortenUuid uuid ++ " (" ++ resourceType ++ ")"

        Just "" ->
            shortenUuid uuid ++ " (" ++ resourceType ++ ")"

        Just name ->
            name


shortenUuid : String -> String
shortenUuid uuid =
    let
        parts =
            String.split "-" uuid
    in
    case ( List.head parts, List.head (List.reverse parts) ) of
        ( Just head, Just tail ) ->
            head ++ " ... " ++ tail

        _ ->
            uuid


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
    Element.row [ Element.spacing spacer.px16 ]
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


loginPickerButton : View.Types.Context -> Element.Element SharedMsg.SharedMsg
loginPickerButton context =
    Element.link []
        { url = Route.toUrl context.urlPathPrefix Route.LoginPicker
        , label =
            Button.default
                context.palette
                { text = "Other Login Methods"
                , onPress =
                    Just SharedMsg.NoOp
                }
        }


serverStatusBadge : ExoPalette -> StatusBadgeSize -> Server -> Element.Element msg
serverStatusBadge palette size server =
    serverStatusBadgeFromStatus palette size (getServerUiStatus server)


serverStatusBadgeFromStatus : ExoPalette -> StatusBadgeSize -> ServerUiStatus -> Element.Element msg
serverStatusBadgeFromStatus palette size status =
    let
        contents =
            status |> getServerUiStatusStr |> Element.text
    in
    StatusBadge.statusBadgeWithSize
        palette
        size
        (status |> getServerUiStatusBadgeState)
        contents


getServerUiStatus : Server -> ServerUiStatus
getServerUiStatus server =
    if server.exoProps.deletionAttempted then
        ServerUiStatusDeleting

    else
        let
            maybeFirstTargetStatus =
                server.exoProps.targetOpenstackStatus
                    |> Maybe.andThen List.head

            targetStatusActive =
                maybeFirstTargetStatus == Just OSTypes.ServerActive
        in
        case server.osProps.details.openstackStatus of
            OSTypes.ServerActive ->
                case ( maybeFirstTargetStatus, server.exoProps.serverOrigin ) of
                    ( Just OSTypes.ServerDeleted, _ ) ->
                        ServerUiStatusDeleting

                    ( Just OSTypes.ServerResize, _ ) ->
                        ServerUiStatusResizing

                    ( Just OSTypes.ServerShelved, _ ) ->
                        ServerUiStatusShelving

                    ( Just OSTypes.ServerShelvedOffloaded, _ ) ->
                        ServerUiStatusShelving

                    ( Just OSTypes.ServerSoftDeleted, _ ) ->
                        ServerUiStatusDeleting

                    ( Just OSTypes.ServerSuspended, _ ) ->
                        ServerUiStatusSuspending

                    ( _, ServerFromExo serverFromExoProps ) ->
                        if serverFromExoProps.exoServerVersion < 4 then
                            ServerUiStatusReady

                        else
                            case serverFromExoProps.exoSetupStatus.data of
                                RDPP.DoHave ( status, _ ) _ ->
                                    case status of
                                        ExoSetupWaiting ->
                                            ServerUiStatusBuilding

                                        ExoSetupStarting ->
                                            ServerUiStatusRunningSetup

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

                    ( _, ServerNotFromExo ) ->
                        ServerUiStatusReady

            OSTypes.ServerBuild ->
                ServerUiStatusBuilding

            OSTypes.ServerDeleted ->
                ServerUiStatusDeleted

            OSTypes.ServerError ->
                ServerUiStatusError

            OSTypes.ServerHardReboot ->
                ServerUiStatusRebooting

            OSTypes.ServerMigrating ->
                ServerUiStatusMigrating

            OSTypes.ServerPassword ->
                ServerUiStatusPassword

            OSTypes.ServerPaused ->
                if targetStatusActive then
                    ServerUiStatusUnpausing

                else
                    ServerUiStatusPaused

            OSTypes.ServerReboot ->
                ServerUiStatusRebooting

            OSTypes.ServerRebuild ->
                ServerUiStatusBuilding

            OSTypes.ServerRescue ->
                ServerUiStatusRescued

            OSTypes.ServerResize ->
                ServerUiStatusResizing

            OSTypes.ServerRevertResize ->
                ServerUiStatusRevertingResize

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

            OSTypes.ServerShutoff ->
                if targetStatusActive then
                    ServerUiStatusStarting

                else
                    ServerUiStatusShutoff

            OSTypes.ServerSoftDeleted ->
                ServerUiStatusSoftDeleted

            OSTypes.ServerStopped ->
                if targetStatusActive then
                    ServerUiStatusStarting

                else
                    ServerUiStatusStopped

            OSTypes.ServerSuspended ->
                if targetStatusActive then
                    ServerUiStatusResuming

                else
                    ServerUiStatusSuspended

            OSTypes.ServerUnknown ->
                ServerUiStatusUnknown

            OSTypes.ServerVerifyResize ->
                if targetStatusActive then
                    ServerUiStatusReady

                else
                    ServerUiStatusVerifyResize


shareStatusBadge : ExoPalette -> ShareStatus -> Element.Element msg
shareStatusBadge palette shareStatus =
    StatusBadge.statusBadge
        palette
        (shareStatus |> getShareUiStatusBadgeState)
        (shareStatus |> OSTypes.shareStatusToString |> Element.text)


getExoSetupStatusStr : Server -> Maybe String
getExoSetupStatusStr server =
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            Nothing

        ServerFromExo exoOriginProps ->
            case exoOriginProps.exoSetupStatus.data of
                RDPP.DoHave ( exoSetupStatus, _ ) _ ->
                    Types.Server.exoSetupStatusToString exoSetupStatus
                        |> Just

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

        ServerUiStatusResizing ->
            "Resizing"

        ServerUiStatusVerifyResize ->
            "Resized"

        ServerUiStatusRevertingResize ->
            "Reverting Resize"

        ServerUiStatusMigrating ->
            "Migrating"

        ServerUiStatusPassword ->
            "Setting Password"


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

        ServerUiStatusResizing ->
            StatusBadge.Warning

        ServerUiStatusVerifyResize ->
            StatusBadge.ReadyGood

        ServerUiStatusRevertingResize ->
            StatusBadge.Warning

        ServerUiStatusMigrating ->
            StatusBadge.Warning

        ServerUiStatusPassword ->
            StatusBadge.ReadyGood


getShareUiStatusBadgeState : ShareStatus -> StatusBadge.StatusBadgeState
getShareUiStatusBadgeState status =
    case status of
        ShareCreating ->
            StatusBadge.Warning

        ShareCreatingFromSnapshot ->
            StatusBadge.Warning

        ShareDeleted ->
            StatusBadge.Muted

        ShareError ->
            StatusBadge.Error

        ShareDeleting ->
            StatusBadge.Muted

        ShareErrorDeleting ->
            StatusBadge.Error

        ShareAvailable ->
            StatusBadge.ReadyGood

        ShareInactive ->
            StatusBadge.Muted

        ShareManageStarting ->
            StatusBadge.Warning

        ShareManageError ->
            StatusBadge.Error

        ShareUnmanageStarting ->
            StatusBadge.Warning

        ShareUnmanageError ->
            StatusBadge.Error

        ShareUnmanaged ->
            StatusBadge.Muted

        ShareAwaitingTransfer ->
            StatusBadge.Warning

        ShareExtending ->
            StatusBadge.Warning

        ShareExtendingError ->
            StatusBadge.Error

        ShareShrinking ->
            StatusBadge.Warning

        ShareShrinkingError ->
            StatusBadge.Error

        ShareShrinkingPossibleDataLossError ->
            StatusBadge.Error

        ShareMigrating ->
            StatusBadge.Warning

        ShareMigratingTo ->
            StatusBadge.Warning

        ShareReplicationChange ->
            StatusBadge.Warning

        ShareReverting ->
            StatusBadge.Warning

        ShareRevertingError ->
            StatusBadge.Error


renderMarkdown : ExoPalette -> String -> List (Element.Element msg)
renderMarkdown palette markdown =
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
                    (\ast -> Markdown.Renderer.render (elmUiRenderer palette) ast)
    in
    case result of
        Ok elements ->
            elements

        Err errors ->
            [ Element.text
                ("Error parsing markdown: \n" ++ errors)
            ]


elmUiRenderer : ExoPalette -> Markdown.Renderer.Renderer (Element.Element msg)
elmUiRenderer palette =
    -- Heavily borrowed and modified from https://ellie-app.com/bQLgjtbgdkZa1
    { heading = heading palette
    , paragraph =
        Element.paragraph
            []
    , thematicBreak = Element.none
    , text = Element.text
    , strong = \content -> Element.row [ Font.semiBold ] content
    , emphasis = \content -> Element.row [ Font.italic ] content
    , strikethrough = \content -> Element.row [ Font.strike ] content
    , codeSpan =
        codeSpan palette
    , link =
        \{ destination } body ->
            Element.newTabLink (Link.linkStyle palette)
                { url = destination
                , label =
                    Element.paragraph
                        [ palette.primary |> SH.toElementColor |> Font.color
                        , Element.pointer
                        ]
                        body
                }
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
                , Element.padding spacer.px12
                , Border.color (SH.toElementColor palette.neutral.border)
                , Background.color (SH.toElementColor palette.neutral.background.frontLayer)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing spacer.px12 ]
                (items
                    |> List.map
                        (\(Markdown.Block.ListItem task children) ->
                            Element.row
                                [ Element.alignTop, Element.spacing spacer.px12 ]
                            <|
                                List.concat
                                    [ [ case task of
                                            Markdown.Block.IncompleteTask ->
                                                Element.Input.defaultCheckbox False

                                            Markdown.Block.CompletedTask ->
                                                Element.Input.defaultCheckbox True

                                            Markdown.Block.NoTask ->
                                                Element.text "•"
                                      ]
                                    , children
                                    ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing spacer.px16 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing spacer.px4 ]
                                [ Element.row [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock =
        \{ body } ->
            codeBlock palette body
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


friendlyCloudName : View.Types.Context -> Project -> String
friendlyCloudName context project =
    let
        cloudPart =
            case GetterSetters.cloudSpecificConfigLookup context.cloudSpecificConfigs project of
                Nothing ->
                    UrlHelpers.hostnameFromUrl project.endpoints.keystone

                Just cloudSpecificConfig ->
                    cloudSpecificConfig.friendlyName

        regionPart =
            project.region |> Maybe.map .id
    in
    cloudPart
        ++ (case regionPart of
                Just regionId ->
                    " " ++ regionId

                Nothing ->
                    ""
           )


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


flavorPicker :
    View.Types.Context
    -> Project
    -> Maybe (List OSTypes.FlavorId)
    -> Maybe String
    -> OSTypes.ComputeQuota
    -> (PopoverId -> msg)
    -> PopoverId
    -> Maybe OSTypes.FlavorId
    -> Maybe OSTypes.FlavorId
    -> (OSTypes.FlavorId -> msg)
    -> Element.Element msg
flavorPicker context project restrictFlavorIds showDisabledFlavorsReason computeQuota flavorGroupToggleTipMsgMapper flavorGroupToggleTipId maybeCurrentFlavorId selectedFlavorId changeMsg =
    let
        { locale } =
            context

        flavorGroups =
            GetterSetters.cloudSpecificConfigLookup context.cloudSpecificConfigs project
                |> Maybe.map .flavorGroups
                |> Maybe.withDefault []

        isFlavorAllowed flavor =
            case restrictFlavorIds of
                Nothing ->
                    True

                Just restrictedFlavorIds ->
                    List.member flavor.id restrictedFlavorIds

        flavorsToShow =
            case ( restrictFlavorIds, showDisabledFlavorsReason ) of
                ( Just restrictedFlavorIds, Nothing ) ->
                    restrictedFlavorIds
                        |> List.filterMap (GetterSetters.flavorLookup project)

                _ ->
                    RDPP.withDefault [] project.flavors

        disabledFlavorTooltip flavor reason =
            ToggleTip.toggleTip context
                flavorGroupToggleTipMsgMapper
                (flavor.id ++ "--restricted")
                (reason
                    |> Maybe.withDefault "This flavor is restricted"
                    |> Element.text
                )
                ST.PositionRight

        -- This is a kludge. Input.radio is intended to display a group of multiple radio buttons,
        -- but we want to embed a button in each table row, so we define several Input.radios,
        -- each containing just a single option.
        -- https://elmlang.slack.com/archives/C4F9NBLR1/p1539909855000100
        radioButton flavor =
            let
                isCurrentFlavor =
                    case maybeCurrentFlavorId of
                        Just currentFlavorId ->
                            flavor.id == currentFlavorId

                        Nothing ->
                            False

                radio_ =
                    if isCurrentFlavor then
                        Element.el [ paddingRight ] <|
                            Element.text "Current"

                    else if isFlavorAllowed flavor then
                        Element.Input.radio
                            [ Element.centerX ]
                            { label = Element.Input.labelHidden flavor.name
                            , onChange = changeMsg
                            , options = [ Element.Input.option flavor.id (Element.text " ") ]
                            , selected = selectedFlavorId
                            }

                    else
                        disabledFlavorTooltip flavor showDisabledFlavorsReason
            in
            -- Only allow selection if there is enough available quota
            case OSQuotas.computeQuotaFlavorAvailServers computeQuota flavor of
                Nothing ->
                    radio_

                Just availServers ->
                    if availServers < 1 then
                        disabledFlavorTooltip flavor
                            (Just "This size would exceed your allocation's quota")

                    else
                        radio_

        paddingRight =
            Element.paddingEach { edges | right = spacer.px16 }

        headerAttribs =
            [ paddingRight
            , Font.semiBold
            , Font.center
            ]

        columns =
            [ { header = Element.none
              , width = Element.fill
              , view = \r -> radioButton r
              }
            , { header = Element.el (headerAttribs ++ [ Font.alignLeft ]) (Element.text "Name")
              , width = Element.fill
              , view = \r -> Element.el [ paddingRight ] (Element.text r.name)
              }
            , { header = Element.none
              , width = Element.fill |> Element.minimum 0
              , view =
                    \r ->
                        case r.description of
                            Nothing ->
                                Element.none

                            Just description ->
                                let
                                    toggleTipId =
                                        Helpers.String.hyphenate
                                            [ r.id
                                            , description
                                            ]
                                in
                                ToggleTip.toggleTip context
                                    flavorGroupToggleTipMsgMapper
                                    toggleTipId
                                    (Element.text description)
                                    ST.PositionRight
              }
            , { header = Element.el (headerAttribs ++ [ Font.alignRight ]) (Element.text "CPUs")
              , width = Element.fill
              , view = \r -> Element.el [ paddingRight, Font.alignRight ] (Element.text (humanCount locale r.vcpu))
              }
            , { header = Element.el (headerAttribs ++ [ Font.alignRight ]) (Element.text "RAM")
              , width = Element.fill
              , view =
                    \r ->
                        Element.el [ paddingRight, Font.alignRight ] (Element.text (FormatNumber.format { locale | decimals = Exact 0 } (toFloat r.ram_mb / 1024) ++ " GB"))
              }
            , { header = Element.el headerAttribs (Element.text "Root Disk")
              , width = Element.fill
              , view =
                    \r ->
                        Element.el
                            [ paddingRight, Font.alignRight ]
                            (if r.disk_root == 0 then
                                Element.text "- *"

                             else
                                Element.text (String.fromInt r.disk_root ++ " GB")
                            )
              }
            , { header = Element.el headerAttribs (Element.text "Ephemeral Disk")
              , width = Element.fill
              , view =
                    \r ->
                        Element.el
                            [ paddingRight, Font.alignRight ]
                            (if r.disk_ephemeral == 0 then
                                Element.text "none"

                             else
                                Element.text (String.fromInt r.disk_ephemeral ++ " GB")
                            )
              }
            ]

        zeroRootDiskExplainText =
            case List.Extra.find (\f -> f.disk_root == 0) flavorsToShow of
                Just _ ->
                    String.concat
                        [ "* No default root disk size is defined for this "
                        , context.localization.virtualComputer
                        , " "
                        , context.localization.virtualComputerHardwareConfig
                        ]

                Nothing ->
                    ""

        flavorEmptyHint =
            if selectedFlavorId == Nothing then
                [ hint context <|
                    String.join
                        " "
                        [ "Please pick"
                        , Helpers.String.indefiniteArticle context.localization.virtualComputerHardwareConfig
                        , context.localization.virtualComputerHardwareConfig
                        ]
                ]

            else
                []

        anyFlavorsTooLarge =
            flavorsToShow
                |> List.filterMap (OSQuotas.computeQuotaFlavorAvailServers computeQuota)
                |> List.filter (\x -> x < 1)
                |> List.isEmpty
                |> not

        renderFlavorGroup : List OSTypes.Flavor -> Types.HelperTypes.FlavorGroup -> Element.Element msg
        renderFlavorGroup flavors flavorGroup =
            let
                regex =
                    Regex.fromString flavorGroup.matchOn
                        |> Maybe.withDefault Regex.never

                groupFlavors =
                    flavors
                        |> List.filter (\f -> Regex.contains regex f.name)
            in
            if List.isEmpty groupFlavors then
                Element.none

            else
                Element.column
                    [ Element.spacing spacer.px8 ]
                    [ Element.row []
                        [ Element.el
                            [ context.palette.neutral.text.subdued
                                |> SH.toElementColor
                                |> Font.color
                            ]
                          <|
                            Element.text flavorGroup.title
                        , case flavorGroup.description of
                            Just description ->
                                let
                                    toggleTipId =
                                        Helpers.String.hyphenate
                                            [ flavorGroupToggleTipId
                                            , flavorGroup.title
                                            ]
                                in
                                ToggleTip.toggleTip context
                                    flavorGroupToggleTipMsgMapper
                                    toggleTipId
                                    (Element.text description)
                                    ST.PositionRight

                            Nothing ->
                                Element.none
                        ]
                    , renderFlavors
                        groupFlavors
                    ]

        renderFlavors flavors =
            Element.table
                [ Element.spacingXY 0 spacer.px4 ]
                { data = flavors
                , columns = columns
                }
    in
    Element.column
        [ Element.spacing spacer.px12 ]
        [ Text.strong <| Helpers.String.toTitleCase context.localization.virtualComputerHardwareConfig
        , Element.el flavorEmptyHint <|
            if List.isEmpty flavorGroups then
                renderFlavors (GetterSetters.sortedFlavors flavorsToShow)

            else
                Element.column
                    [ Element.spacing spacer.px12 ]
                    (flavorGroups |> List.map (renderFlavorGroup (GetterSetters.sortedFlavors flavorsToShow)))
        , if anyFlavorsTooLarge then
            Element.text <|
                String.join " "
                    [ context.localization.virtualComputerHardwareConfig
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    , "marked 'X' are too large for your available"
                    , context.localization.maxResourcesPerProject
                    ]

          else
            Element.none
        , Element.paragraph [ Text.fontSize Text.Tiny ] [ Element.text zeroRootDiskExplainText ]
        ]


createdAgoByFromSize :
    View.Types.Context
    -> ( String, Element.Element msg )
    -> Maybe ( String, String )
    -> Maybe ( String, String )
    -> Maybe ( String, Element.Element msg )
    -> OSTypes.Server
    -> Project
    -> Element.Element msg
createdAgoByFromSize context ( agoWord, agoContents ) maybeWhoCreatedTuple maybeFromTuple maybeSizeTuple server { flavors, endpoints } =
    let
        subduedText =
            Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)
    in
    Element.wrappedRow
        [ Element.width Element.fill, Element.spaceEvenly ]
    <|
        [ Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| agoWord ++ " ")
            , agoContents
            ]
        , case maybeWhoCreatedTuple of
            Just ( creatorAdjective, whoCreated ) ->
                Element.row [ Element.padding spacer.px8 ]
                    [ Element.el [ subduedText ] (Element.text <| "by " ++ creatorAdjective ++ " ")
                    , Element.text whoCreated
                    ]

            Nothing ->
                Element.none
        , case maybeFromTuple of
            Just ( fromAdjective, whereFrom ) ->
                Element.row [ Element.padding spacer.px8 ]
                    [ Element.el [ subduedText ] (Element.text <| "from " ++ fromAdjective ++ " ")
                    , Element.text whereFrom
                    ]

            Nothing ->
                Element.none
        , case maybeSizeTuple of
            Just ( sizeAdjective, size ) ->
                Element.row [ Element.padding spacer.px8 ]
                    [ Element.el [ subduedText ] (Element.text <| sizeAdjective ++ " ")
                    , size
                    ]

            Nothing ->
                Element.none
        , if "js2.jetstream-cloud.org" == UrlHelpers.hostnameFromUrl endpoints.keystone then
            Element.row [ Element.padding spacer.px8 ]
                [ Element.el [ subduedText ] (Element.text "Burn rate ")
                , String.concat
                    [ Helpers.Jetstream2.calculateAllocationBurnRate (RDPP.withDefault [] flavors) server
                        |> Maybe.map (Helpers.Formatting.humanRatio context.locale)
                        |> Maybe.withDefault "Unknown"
                    , " SUs/hour"
                    ]
                    |> Element.text
                ]

          else
            Element.none
        ]


featuredImageNamePrefixLookup : View.Types.Context -> Project -> Maybe String
featuredImageNamePrefixLookup context project =
    let
        projectKeystoneHostname =
            UrlHelpers.hostnameFromUrl project.endpoints.keystone
    in
    Dict.get projectKeystoneHostname context.cloudSpecificConfigs
        |> Maybe.andThen (\csc -> csc.featuredImageNamePrefix)


requiredLabel : ExoPalette -> Element.Element msg -> Element.Element msg
requiredLabel palette undecoratedLabelView =
    Element.row []
        [ undecoratedLabelView
        , Element.el
            [ Element.paddingXY spacer.px4 0
            , Font.color (SH.toElementColor palette.danger.textOnNeutralBG)
            ]
            (Element.text "*")
        ]


radioLabelAttributes : List (Element.Attribute msg)
radioLabelAttributes =
    [ Element.paddingEach { edges | bottom = spacer.px12 } ]


invalidInputAttributes : ExoPalette -> List (Element.Attribute msg)
invalidInputAttributes palette =
    validOrInvalidInputElementAttributes palette.danger.default Icons.alertCircle


warningInputAttributes : ExoPalette -> List (Element.Attribute msg)
warningInputAttributes palette =
    validOrInvalidInputElementAttributes palette.warning.default Icons.alertTriangle


validInputAttributes : ExoPalette -> List (Element.Attribute msg)
validInputAttributes palette =
    validOrInvalidInputElementAttributes palette.success.default Icons.checkCircle


validOrInvalidInputElementAttributes : Color.Color -> Icons.Icon -> List (Element.Attribute msg)
validOrInvalidInputElementAttributes color icon =
    [ Element.onRight
        (icon
            |> featherIcon
                [ Font.color (color |> SH.toElementColor)
                , Element.moveLeft 30
                , Element.centerY
                ]
        )
    , Element.paddingEach
        { top = 10
        , right = 35
        , bottom = 10
        , left = 10
        }
    , Element.below
        (Element.el
            [ Font.color (color |> SH.toElementColor)
            , Element.width Element.fill
            , Element.height (Element.px 3)
            , Background.color (color |> SH.toElementColor)
            ]
            Element.none
        )
    ]


renderMaybe : Maybe a -> (a -> Element.Element msg) -> Element.Element msg
renderMaybe condition component =
    case condition of
        Just value ->
            component value

        Nothing ->
            Element.none


allDirections : List SecurityGroupRuleDirection
allDirections =
    [ Ingress, Egress ]


directionOptions : List ( String, String )
directionOptions =
    List.map (\direction -> ( directionToString direction, directionToString direction |> toTitleCase )) allDirections


allEtherTypes : List SecurityGroupRuleEthertype
allEtherTypes =
    [ Ipv4, Ipv6 ]


etherTypeOptions : List ( String, String )
etherTypeOptions =
    List.map (\etherType -> ( etherTypeToString etherType, etherTypeToString etherType )) allEtherTypes


allProtocols : List SecurityGroupRuleProtocol
allProtocols =
    [ AnyProtocol
    , ProtocolIcmp
    , ProtocolIcmpv6
    , ProtocolTcp
    , ProtocolUdp
    , ProtocolAh
    , ProtocolDccp
    , ProtocolEgp
    , ProtocolEsp
    , ProtocolGre
    , ProtocolIgmp
    , ProtocolIpv6Encap
    , ProtocolIpv6Frag
    , ProtocolIpv6Nonxt
    , ProtocolIpv6Opts
    , ProtocolIpv6Route
    , ProtocolOspf
    , ProtocolPgm
    , ProtocolRsvp
    , ProtocolSctp
    , ProtocolUdpLite
    , ProtocolVrrp
    ]


protocolOptions : List ( String, String )
protocolOptions =
    List.map (\protocol -> ( protocolToString protocol, protocolToString protocol )) allProtocols


portRangeBoundsOptions : List ( String, String )
portRangeBoundsOptions =
    List.map
        (\bounds -> ( portRangeBoundsToString bounds, portRangeBoundsToString bounds ))
        allPortRangeBounds


allPortRangeBounds : List PortRangeBounds
allPortRangeBounds =
    [ PortRangeAny, PortRangeSingle, PortRangeMinMax ]


portRangeBoundsToString : PortRangeBounds -> String
portRangeBoundsToString bounds =
    case bounds of
        PortRangeAny ->
            "Any"

        PortRangeSingle ->
            "Single"

        PortRangeMinMax ->
            "Min - Max"


stringToPortRangeBounds : String -> PortRangeBounds
stringToPortRangeBounds bounds =
    case bounds of
        "Single" ->
            PortRangeSingle

        "Min - Max" ->
            PortRangeMinMax

        _ ->
            PortRangeAny


remoteOptions : Localization -> List ( String, String )
remoteOptions localization =
    List.map
        (\remoteType -> ( remoteTypeToString localization remoteType, remoteTypeToString localization remoteType |> toTitleCase ))
        allRemoteTypes


allRemoteTypes : List RemoteType
allRemoteTypes =
    [ Any, IpPrefix, SecurityGroup ]


securityGroupTypeLabel : Localization -> String
securityGroupTypeLabel localization =
    localization.securityGroup |> String.Extra.toTitleCase


stringToRemoteType : Localization -> String -> RemoteType
stringToRemoteType localization remoteType =
    case remoteType of
        "IP Prefix" ->
            IpPrefix

        _ ->
            if remoteType == securityGroupTypeLabel localization then
                SecurityGroup

            else
                Any


remoteTypeToString : Localization -> RemoteType -> String
remoteTypeToString localization remoteType =
    case remoteType of
        IpPrefix ->
            "IP Prefix"

        SecurityGroup ->
            securityGroupTypeLabel localization

        Any ->
            "Any"


remoteToRemoteType : Maybe Remote -> RemoteType
remoteToRemoteType remote =
    case remote of
        Just (RemoteIpPrefix _) ->
            IpPrefix

        Just (RemoteGroupUuid _) ->
            SecurityGroup

        _ ->
            Any


remoteToString : Maybe Remote -> String
remoteToString remote =
    case remote of
        Just (RemoteIpPrefix ip) ->
            ip

        Just (RemoteGroupUuid groupUuid) ->
            groupUuid

        Nothing ->
            "Any"


remoteToStringInput : Maybe Remote -> String
remoteToStringInput remote =
    remote
        |> remoteToString
        |> (\remoteString ->
                if remoteString == "Any" then
                    ""

                else
                    remoteString
           )


volumeStatusBadgeFromStatus : ExoPalette -> StatusBadgeSize -> VolumeStatus -> Element.Element msg
volumeStatusBadgeFromStatus palette size status =
    let
        contents =
            status |> OSTypes.volumeStatusToString |> String.Extra.humanize |> Helpers.String.toTitleCase |> Element.text
    in
    StatusBadge.statusBadgeWithSize
        palette
        size
        (status |> getVolumeStatusBadgeState)
        contents


getVolumeStatusBadgeState : VolumeStatus -> StatusBadge.StatusBadgeState
getVolumeStatusBadgeState status =
    case status of
        Creating ->
            StatusBadge.Warning

        Available ->
            StatusBadge.ReadyGood

        Reserved ->
            StatusBadge.Muted

        Attaching ->
            StatusBadge.Warning

        Detaching ->
            StatusBadge.Warning

        InUse ->
            StatusBadge.ReadyGood

        Maintenance ->
            StatusBadge.Warning

        Deleting ->
            StatusBadge.Muted

        AwaitingTransfer ->
            StatusBadge.Warning

        Error ->
            StatusBadge.Error

        ErrorDeleting ->
            StatusBadge.Error

        BackingUp ->
            StatusBadge.Warning

        RestoringBackup ->
            StatusBadge.Warning

        ErrorBackingUp ->
            StatusBadge.Error

        ErrorRestoring ->
            StatusBadge.Error

        ErrorExtending ->
            StatusBadge.Error

        Downloading ->
            StatusBadge.Warning

        Uploading ->
            StatusBadge.Warning

        Retyping ->
            StatusBadge.Warning

        Extending ->
            StatusBadge.Warning
