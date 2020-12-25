module View.Helpers exposing
    ( browserLink
    , compactKVRow
    , compactKVSubRow
    , edges
    , exoColumnAttributes
    , exoElementAttributes
    , exoPaddingSpacingAttributes
    , exoRowAttributes
    , friendlyProjectTitle
    , getServerUiStatus
    , getServerUiStatusColor
    , getServerUiStatusStr
    , heading2
    , heading3
    , heading4
    , hint
    , inputItemAttributes
    , possiblyUntitledResource
    , renderMarkdown
    , renderMessage
    , titleFromHostname
    , toExoPalette
    )

import Color
import Element
import Element.Background as Background
import Element.Border
import Element.Events
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
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Types.Error exposing (ErrorLevel(..), toFriendlyErrorLevel)
import Types.HelperTypes
import Types.Types
    exposing
        ( ExoSetupStatus(..)
        , LogMessage
        , Model
        , Msg(..)
        , Project
        , Server
        , ServerOrigin(..)
        , ServerUiStatus(..)
        , Style
        )
import View.Types


toExoPalette : Style -> ExoPalette
toExoPalette style =
    SH.toExoPalette style.primaryColor style.secondaryColor style.styleMode



{- Elm UI Doodads -}


exoRowAttributes : List (Element.Attribute Msg)
exoRowAttributes =
    exoElementAttributes


exoColumnAttributes : List (Element.Attribute Msg)
exoColumnAttributes =
    exoElementAttributes


exoElementAttributes : List (Element.Attribute Msg)
exoElementAttributes =
    exoPaddingSpacingAttributes


exoPaddingSpacingAttributes : List (Element.Attribute Msg)
exoPaddingSpacingAttributes =
    [ Element.padding 10
    , Element.spacing 10
    ]


inputItemAttributes : Color.Color -> List (Element.Attribute Msg)
inputItemAttributes backgroundColor =
    [ Element.spacing 12
    , Background.color <| SH.toElementColor <| backgroundColor
    ]


heading2 : List (Element.Attribute Msg)
heading2 =
    [ Region.heading 2
    , Font.bold
    , Font.size 24
    ]


heading3 : List (Element.Attribute Msg)
heading3 =
    [ Region.heading 3
    , Font.bold
    , Font.size 20
    ]


heading4 : List (Element.Attribute Msg)
heading4 =
    [ Region.heading 4
    , Font.bold
    , Font.size 16
    ]


compactKVRow : String -> Element.Element Msg -> Element.Element Msg
compactKVRow key value =
    Element.row
        (exoRowAttributes ++ [ Element.padding 0, Element.spacing 10 ])
        [ Element.paragraph [ Element.alignTop, Element.width (Element.px 200), Font.bold ] [ Element.text key ]
        , value
        ]


compactKVSubRow : String -> Element.Element Msg -> Element.Element Msg
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


hint : Style.Types.ExoPalette -> String -> Element.Attribute msg
hint palette hintText =
    Element.below
        (Element.el
            [ Font.color (palette.error |> SH.toElementColor)
            , Font.size 14
            , Element.alignRight
            , Element.moveDown 6
            ]
            (Element.text hintText)
        )


renderMessage : Style -> LogMessage -> Element.Element Msg
renderMessage style message =
    let
        levelColor : ErrorLevel -> Element.Color
        levelColor errLevel =
            case errLevel of
                ErrorDebug ->
                    style |> toExoPalette |> .readyGood |> SH.toElementColor

                ErrorInfo ->
                    style |> toExoPalette |> .on |> .background |> SH.toElementColor

                ErrorWarn ->
                    style |> toExoPalette |> .warn |> SH.toElementColor

                ErrorCrit ->
                    style |> toExoPalette |> .error |> SH.toElementColor
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
            , Element.el [ style |> toExoPalette |> .muted |> SH.toElementColor |> Font.color ]
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


browserLink : Style.Types.ExoPalette -> Bool -> Types.HelperTypes.Url -> View.Types.BrowserLinkLabel -> Element.Element Msg
browserLink palette isElectron url label =
    let
        linkAttribs =
            [ palette.primary |> SH.toElementColor |> Font.color
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
    if isElectron then
        Element.el
            (renderedLabel.attribs
                ++ [ Element.Events.onClick (OpenInBrowser url) ]
            )
            renderedLabel.contents

    else
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


getServerUiStatus : Server -> ServerUiStatus
getServerUiStatus server =
    case server.osProps.details.openstackStatus of
        OSTypes.ServerActive ->
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
                                        ServerUiStatusPartiallyActive

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

        OSTypes.ServerPaused ->
            ServerUiStatusPaused

        OSTypes.ServerReboot ->
            ServerUiStatusReboot

        OSTypes.ServerSuspended ->
            ServerUiStatusSuspended

        OSTypes.ServerShutoff ->
            ServerUiStatusShutoff

        OSTypes.ServerStopped ->
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
            ServerUiStatusShelved

        OSTypes.ServerShelvedOffloaded ->
            ServerUiStatusShelved

        OSTypes.ServerDeleted ->
            ServerUiStatusDeleted


getServerUiStatusStr : ServerUiStatus -> String
getServerUiStatusStr status =
    case status of
        ServerUiStatusUnknown ->
            "Unknown"

        ServerUiStatusBuilding ->
            "Building"

        ServerUiStatusPartiallyActive ->
            "Partially Active"

        ServerUiStatusReady ->
            "Ready"

        ServerUiStatusPaused ->
            "Paused"

        ServerUiStatusReboot ->
            "Reboot"

        ServerUiStatusSuspended ->
            "Suspended"

        ServerUiStatusShutoff ->
            "Shut off"

        ServerUiStatusStopped ->
            "Stopped"

        ServerUiStatusSoftDeleted ->
            "Soft-deleted"

        ServerUiStatusError ->
            "Error"

        ServerUiStatusRescued ->
            "Rescued"

        ServerUiStatusShelved ->
            "Shelved"

        ServerUiStatusDeleted ->
            "Deleted"


getServerUiStatusColor : ExoPalette -> ServerUiStatus -> Element.Color
getServerUiStatusColor palette status =
    case status of
        ServerUiStatusUnknown ->
            SH.toElementColor palette.muted

        ServerUiStatusBuilding ->
            SH.toElementColor palette.warn

        ServerUiStatusPartiallyActive ->
            SH.toElementColor palette.warn

        ServerUiStatusReady ->
            SH.toElementColor palette.readyGood

        ServerUiStatusReboot ->
            SH.toElementColor palette.warn

        ServerUiStatusPaused ->
            SH.toElementColor palette.muted

        ServerUiStatusSuspended ->
            SH.toElementColor palette.muted

        ServerUiStatusShutoff ->
            SH.toElementColor palette.muted

        ServerUiStatusStopped ->
            SH.toElementColor palette.muted

        ServerUiStatusSoftDeleted ->
            SH.toElementColor palette.muted

        ServerUiStatusError ->
            -- red
            SH.toElementColor palette.error

        ServerUiStatusRescued ->
            -- red
            SH.toElementColor palette.error

        ServerUiStatusShelved ->
            SH.toElementColor palette.muted

        ServerUiStatusDeleted ->
            SH.toElementColor palette.muted


renderMarkdown : Style.Types.ExoPalette -> Bool -> String -> Element.Element Msg
renderMarkdown palette isElectron markdown =
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
                    (\ast -> Markdown.Renderer.render (elmUiRenderer palette isElectron) ast)
    in
    case result of
        Ok elements ->
            Element.paragraph []
                elements

        Err errors ->
            Element.text
                ("Error parsing markdown: \n" ++ errors)


elmUiRenderer : Style.Types.ExoPalette -> Bool -> Markdown.Renderer.Renderer (Element.Element Msg)
elmUiRenderer palette isElectron =
    -- Heavily borrowed and modified from https://ellie-app.com/bQLgjtbgdkZa1
    { heading = heading
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
                palette
                isElectron
                destination
                (View.Types.BrowserLinkFancyLabel
                    (Element.paragraph
                        [ palette.primary |> SH.toElementColor |> Font.color
                        , Font.underline
                        , Element.pointer
                        ]
                        body
                    )
                )
    , hardLineBreak = Html.br [] [] |> Element.html
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
                [ Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Element.Border.color (SH.toElementColor palette.on.background)
                , Background.color (SH.toElementColor palette.surface)
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
    { level : Markdown.Block.HeadingLevel
    , rawText : String
    , children : List (Element.Element Msg)
    }
    -> Element.Element Msg
heading { level, children } =
    Element.paragraph
        (case level of
            Markdown.Block.H2 ->
                heading2

            Markdown.Block.H3 ->
                heading3

            Markdown.Block.H4 ->
                heading4

            _ ->
                heading2
        )
        children


friendlyProjectTitle : Model -> Project -> String
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
