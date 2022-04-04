module View.Helpers exposing
    ( compactKVRow
    , compactKVSubRow
    , contentContainer
    , createdAgoByFromSize
    , edges
    , ellipsizedText
    , exoColumnAttributes
    , exoElementAttributes
    , exoPaddingSpacingAttributes
    , exoRowAttributes
    , externalLink
    , featuredImageNamePrefixLookup
    , flavorPicker
    , formContainer
    , friendlyCloudName
    , friendlyProjectTitle
    , getExoSetupStatusStr
    , getServerUiStatus
    , getServerUiStatusBadgeState
    , getServerUiStatusStr
    , hint
    , inputItemAttributes
    , invalidInputAttributes
    , invalidInputHelperText
    , linkAttribs
    , loginPickerButton
    , possiblyUntitledResource
    , renderIf
    , renderMarkdown
    , renderMaybe
    , renderMessageAsElement
    , renderMessageAsString
    , renderRDPP
    , renderWebData
    , requiredLabel
    , serverStatusBadge
    , sortProjects
    , titleFromHostname
    , toExoPalette
    , userAppProxyLookup
    , validInputAttributes
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
import FeatherIcons
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (humanCount)
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
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
import OpenStack.Types as OSTypes
import Regex
import RemoteData
import Route
import Style.Helpers as SH
import Style.Types as ST exposing (ExoPalette)
import Style.Widgets.Button as Button
import Style.Widgets.StatusBadge as StatusBadge
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip as ToggleTip
import Types.Error exposing (ErrorLevel(..), toFriendlyErrorLevel)
import Types.HelperTypes
import Types.Project exposing (Project)
import Types.Server exposing (ExoSetupStatus(..), Server, ServerOrigin(..), ServerUiStatus(..))
import Types.SharedModel exposing (LogMessage, SharedModel, Style)
import Types.SharedMsg as SharedMsg
import View.Types
import Widget


toExoPalette : Style -> ExoPalette
toExoPalette style =
    SH.toExoPalette style.deployerColors style.styleMode



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
    Text.headingStyleAttrs palette
        ++ Text.typographyAttrs Text.H2


heading3 : ExoPalette -> List (Element.Attribute msg)
heading3 palette =
    Text.subheadingStyleAttrs palette
        ++ Text.typographyAttrs Text.H3


heading4 : List (Element.Attribute msg)
heading4 =
    Text.typographyAttrs Text.H4
        ++ [ Region.heading 4
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
                    (" at " ++ humanReadableDateAndTime message.timestamp)
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
    , humanReadableDateAndTime message.timestamp
    , " -- while trying to "
    , message.context.actionContext
    , " -- "
    , message.message
    ]
        |> String.concat


linkAttribs : View.Types.Context -> List (Element.Attribute msg)
linkAttribs context =
    [ context.palette.primary |> SH.toElementColor |> Font.color
    , Element.pointer
    , Border.color (SH.toElementColor context.palette.background)
    , Border.widthEach
        { bottom = 1
        , left = 0
        , top = 0
        , right = 0
        }
    , Element.mouseOver [ Border.color (SH.toElementColor context.palette.primary) ]
    ]


externalLink : View.Types.Context -> Types.HelperTypes.Url -> String -> Element.Element msg
externalLink context url label =
    Element.newTabLink
        (linkAttribs context)
        { url = url
        , label = Element.text label
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


serverStatusBadge : ExoPalette -> Server -> Element.Element msg
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
    if server.exoProps.deletionAttempted then
        ServerUiStatusDeleting

    else
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
                                        RDPP.DoHave ( status, _ ) _ ->
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
                    Just OSTypes.ServerDeleted ->
                        ServerUiStatusDeleting

                    Just OSTypes.ServerResize ->
                        ServerUiStatusResizing

                    Just OSTypes.ServerShelved ->
                        ServerUiStatusShelving

                    Just OSTypes.ServerShelvedOffloaded ->
                        ServerUiStatusShelving

                    Just OSTypes.ServerSoftDeleted ->
                        ServerUiStatusDeleting

                    Just OSTypes.ServerSuspended ->
                        ServerUiStatusSuspending

                    _ ->
                        whenNoTargetStatus

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


getExoSetupStatusStr : Server -> Maybe String
getExoSetupStatusStr server =
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            Nothing

        ServerFromExo exoOriginProps ->
            case exoOriginProps.exoSetupStatus.data of
                RDPP.DoHave ( exoSetupStatus, _ ) _ ->
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
    , strikethrough = \content -> Element.row [ Font.strike ] content
    , codeSpan =
        -- TODO implement this (show fixed-width font) once we need it
        Element.text
    , link =
        \{ destination } body ->
            Element.newTabLink (linkAttribs context)
                { url = destination
                , label =
                    Element.paragraph
                        [ context.palette.primary |> SH.toElementColor |> Font.color
                        , Font.underline
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
                , Element.padding 10
                , Border.color (SH.toElementColor context.palette.on.background)
                , Background.color (SH.toElementColor context.palette.surface)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 10 ]
                (items
                    |> List.map
                        (\(Markdown.Block.ListItem task children) ->
                            Element.row
                                [ Element.alignTop, Element.spacing 10 ]
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


friendlyCloudName : View.Types.Context -> Project -> String
friendlyCloudName context project =
    let
        cloudPart =
            case GetterSetters.cloudSpecificConfigLookup context.cloudSpecificConfigs project of
                Nothing ->
                    UrlHelpers.hostnameFromUrl project.endpoints.keystone

                Just cloudSpecificConfig ->
                    cloudSpecificConfig.friendlyName
                        -- TODO deprecate friendlySubName after Jetstream1 is decommissioned
                        ++ (case cloudSpecificConfig.friendlySubName of
                                Nothing ->
                                    ""

                                Just friendlySubName ->
                                    " " ++ friendlySubName
                           )

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
    -> OSTypes.ComputeQuota
    -> Maybe Types.HelperTypes.FlavorGroupTitle
    -> (Maybe Types.HelperTypes.FlavorGroupTitle -> msg)
    -> Maybe OSTypes.FlavorId
    -> Maybe OSTypes.FlavorId
    -> (OSTypes.FlavorId -> msg)
    -> Element.Element msg
flavorPicker context project restrictFlavorIds computeQuota selectedFlavorGroupToggleTip selectFlavorGroupToggleTipMsg maybeCurrentFlavorId selectedFlavorId changeMsg =
    let
        { locale } =
            context

        flavorGroups =
            GetterSetters.cloudSpecificConfigLookup context.cloudSpecificConfigs project
                |> Maybe.map .flavorGroups
                |> Maybe.withDefault []

        allowedFlavors =
            case restrictFlavorIds of
                Nothing ->
                    project.flavors

                Just restrictedFlavorIds ->
                    restrictedFlavorIds
                        |> List.filterMap (GetterSetters.flavorLookup project)

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
                        Element.text "Current"

                    else
                        Element.Input.radio
                            []
                            { label = Element.Input.labelHidden flavor.name
                            , onChange = changeMsg
                            , options = [ Element.Input.option flavor.id (Element.text " ") ]
                            , selected =
                                selectedFlavorId
                                    |> Maybe.andThen
                                        (\flavorId ->
                                            if flavor.id == flavorId then
                                                Just flavor.id

                                            else
                                                Nothing
                                        )
                            }
            in
            -- Only allow selection if there is enough available quota
            case OSQuotas.computeQuotaFlavorAvailServers computeQuota flavor of
                Nothing ->
                    radio_

                Just availServers ->
                    if availServers < 1 then
                        Element.text "X"

                    else
                        radio_

        paddingRight =
            Element.paddingEach { edges | right = 15 }

        headerAttribs =
            [ paddingRight
            , Font.bold
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
            , { header = Element.el headerAttribs (Element.text "CPUs")
              , width = Element.fill
              , view = \r -> Element.el [ paddingRight, Font.alignRight ] (Element.text (humanCount locale r.vcpu))
              }
            , { header = Element.el headerAttribs (Element.text "RAM")
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
            case List.Extra.find (\f -> f.disk_root == 0) allowedFlavors of
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
                        [ "Please pick a"
                        , context.localization.virtualComputerHardwareConfig
                        ]
                ]

            else
                []

        anyFlavorsTooLarge =
            allowedFlavors
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
                    [ Element.spacing 5, Element.paddingXY 0 5 ]
                    [ Element.row []
                        [ Element.el
                            [ context.palette.muted
                                |> SH.toElementColor
                                |> Font.color
                            ]
                          <|
                            Element.text flavorGroup.title
                        , case flavorGroup.description of
                            Just description ->
                                let
                                    selected =
                                        selectedFlavorGroupToggleTip |> Maybe.map (\n -> flavorGroup.title == n) |> Maybe.withDefault False
                                in
                                ToggleTip.toggleTip context.palette
                                    (Element.text description)
                                    ST.PositionRight
                                    selected
                                    (selectFlavorGroupToggleTipMsg
                                        (if selected then
                                            Nothing

                                         else
                                            Just flavorGroup.title
                                        )
                                    )

                            Nothing ->
                                Element.none
                        ]
                    , renderFlavors
                        groupFlavors
                    ]

        renderFlavors flavors =
            Element.table
                []
                { data = flavors
                , columns = columns
                }
    in
    Element.column
        [ Element.spacing 10 ]
        [ Element.el
            [ Font.bold ]
            (Element.text <| Helpers.String.toTitleCase context.localization.virtualComputerHardwareConfig)
        , Element.el flavorEmptyHint <|
            if List.isEmpty flavorGroups then
                renderFlavors (GetterSetters.sortedFlavors allowedFlavors)

            else
                Element.column
                    []
                    (flavorGroups |> List.map (renderFlavorGroup (GetterSetters.sortedFlavors allowedFlavors)))
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
        , Element.paragraph [ Font.size 12 ] [ Element.text zeroRootDiskExplainText ]
        ]


createdAgoByFromSize :
    View.Types.Context
    -> ( String, Element.Element msg )
    -> Maybe ( String, String )
    -> Maybe ( String, String )
    -> Maybe ( String, Element.Element msg )
    -> Element.Element msg
createdAgoByFromSize context ( agoWord, agoContents ) maybeWhoCreatedTuple maybeFromTuple maybeSizeTuple =
    let
        muted =
            Font.color (context.palette.muted |> SH.toElementColor)
    in
    Element.wrappedRow
        [ Element.width Element.fill, Element.spaceEvenly ]
    <|
        [ Element.row [ Element.paddingXY 5 6 ]
            [ Element.el [ muted ] (Element.text <| agoWord ++ " ")
            , agoContents
            ]
        , case maybeWhoCreatedTuple of
            Just ( creatorAdjective, whoCreated ) ->
                Element.row [ Element.paddingXY 5 6 ]
                    [ Element.el [ muted ] (Element.text <| "by " ++ creatorAdjective ++ " ")
                    , Element.text whoCreated
                    ]

            Nothing ->
                Element.none
        , case maybeFromTuple of
            Just ( fromAdjective, whereFrom ) ->
                Element.row [ Element.paddingXY 5 6 ]
                    [ Element.el [ muted ] (Element.text <| "from " ++ fromAdjective ++ " ")
                    , Element.text whereFrom
                    ]

            Nothing ->
                Element.none
        , case maybeSizeTuple of
            Just ( sizeAdjective, size ) ->
                Element.row [ Element.paddingXY 5 6 ]
                    [ Element.el [ muted ] (Element.text <| sizeAdjective ++ " ")
                    , size
                    ]

            Nothing ->
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


userAppProxyLookup : View.Types.Context -> Project -> Maybe Types.HelperTypes.UserAppProxyHostname
userAppProxyLookup context project =
    let
        projectKeystoneHostname =
            UrlHelpers.hostnameFromUrl project.endpoints.keystone
    in
    Dict.get projectKeystoneHostname context.cloudSpecificConfigs
        |> Maybe.andThen (\csc -> csc.userAppProxy)


requiredLabel : ExoPalette -> Element.Element msg -> Element.Element msg
requiredLabel palette undecoratedLabelView =
    Element.row []
        [ undecoratedLabelView
        , Element.el
            [ Element.paddingXY 4 0
            , Font.color (SH.toElementColor palette.error)
            ]
            (Element.text "*")
        ]


invalidInputAttributes : ExoPalette -> List (Element.Attribute msg)
invalidInputAttributes palette =
    validOrInvalidInputElementAttributes palette.error FeatherIcons.alertCircle


validInputAttributes : ExoPalette -> List (Element.Attribute msg)
validInputAttributes palette =
    validOrInvalidInputElementAttributes palette.readyGood FeatherIcons.checkCircle


validOrInvalidInputElementAttributes : Color.Color -> FeatherIcons.Icon -> List (Element.Attribute msg)
validOrInvalidInputElementAttributes color icon =
    [ Element.onRight
        (Element.el
            [ Font.color (color |> SH.toElementColor)
            , Element.moveLeft 30
            , Element.centerY
            ]
            (icon
                |> FeatherIcons.toHtml []
                |> Element.html
            )
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


invalidInputHelperText : ExoPalette -> String -> Element.Element msg
invalidInputHelperText palette helperText =
    Element.row [ Element.spacingXY 10 0 ]
        [ Element.el
            [ Font.color (palette.error |> SH.toElementColor)
            ]
            (FeatherIcons.alertCircle
                |> FeatherIcons.toHtml []
                |> Element.html
            )
        , Element.el
            [ Font.color (SH.toElementColor palette.error)
            , Font.size 16
            ]
            (Element.text helperText)
        ]


renderIf : Bool -> Element.Element msg -> Element.Element msg
renderIf condition component =
    if condition then
        component

    else
        Element.none


renderMaybe : Maybe a -> (a -> Element.Element msg) -> Element.Element msg
renderMaybe condition component =
    case condition of
        Just value ->
            component value

        Nothing ->
            Element.none
