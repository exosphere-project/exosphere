module DesignSystem.Explorer exposing (main)

import Element
import Element.Font as Font
import Element.Region
import FeatherIcons
import Html
import Html.Attributes exposing (src, style)
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Button as Button
import Style.Widgets.Card exposing (badge, clickableCardFixedSize, exoCard, exoCardWithTitleAndSubtitle, expandoCard)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon exposing (bell, console, copyToClipboard, history, ipAddress, lock, lockOpen, plusCircle, remove, roundRect, timesCircle)
import Style.Widgets.IconButton exposing (chip)
import Style.Widgets.Link as Link
import Style.Widgets.Meter exposing (meter)
import Style.Widgets.StatusBadge exposing (StatusBadgeState(..), statusBadge)
import Style.Widgets.Text as Text
import UIExplorer
    exposing
        ( Config
        , UIExplorerProgram
        , category
        , createCategories
        , exploreWithCategories
        , storiesOf
        )
import UIExplorer.ColorMode exposing (ColorMode(..))
import View.Helpers as VH



--- elm-ui helpers


{-| Convert Elements to HTML & apply global styling attributes.
-}
toHtml : Style.Types.ExoPalette -> Element.Element msg -> Html.Html msg
toHtml pal a =
    Element.layout
        [ Font.color <| SH.toElementColor <| pal.on.background ]
        a



--- theme


{-| Creates an ExoPalette based on the light/dark color mode.
-}
palette : Maybe ColorMode -> Style.Types.ExoPalette
palette maybeColorMode =
    let
        theme =
            case maybeColorMode of
                Just colorMode ->
                    case colorMode of
                        Light ->
                            Style.Types.Light

                        Dark ->
                            Style.Types.Dark

                Nothing ->
                    Style.Types.Light
    in
    SH.toExoPalette
        Style.Types.defaultColors
        { theme = Style.Types.Override theme, systemPreference = Nothing }


{-| Stub listener which is intended for use with ports.

    e.g. https://github.com/kalutheo/elm-ui-explorer/blob/master/examples/button/ExplorerWithNotes.elm#L22

-}
onColorModeChanged : ColorMode -> Cmd msg
onColorModeChanged _ =
    Cmd.none



--- component helpers


{-| Create an icon with standard size & color.
-}
defaultIcon : Style.Types.ExoPalette -> (Element.Color -> number -> icon) -> icon
defaultIcon pal icon =
    icon (pal.on.background |> SH.toElementColor) 25


veryLongCopy : String
veryLongCopy =
    """
    Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC,
    making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words,
    consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source.
    Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC.
    This book is a treatise on the theory of ethics, very popular during the Renaissance.
    """



--- MODEL


{-| Is the Expandable Card expanded or collapsed?
-}
type alias ExpandoCardState =
    { expanded : Bool }


{-| Is the Popover visible?
-}
type alias PopoverState =
    { isShown : Bool }


type alias Model =
    { expandoCard : ExpandoCardState
    , popover : PopoverState
    }


initialModel : Model
initialModel =
    { expandoCard = { expanded = False }
    , popover = { isShown = False }
    }


type alias PluginOptions =
    {}



--- UPDATE


type Msg
    = NoOp
    | ToggleExpandoCard Bool
    | ShowHidePopover



--- MAIN


config : Config Model Msg PluginOptions ()
config =
    { customModel = initialModel
    , customHeader =
        Just
            { title = "Exosphere Design System"
            , logo = UIExplorer.logoFromHtml (Html.img [ src "/assets/img/logo-alt.svg", style "padding-top" "10px", style "padding-left" "5px" ] [])
            , titleColor = Just "#FFFFFF"
            , bgColor = Just "#181725"
            }
    , init = \_ m -> m
    , enableDarkMode = True
    , subscriptions = \_ -> Sub.none
    , update =
        \msg m ->
            let
                model =
                    m.customModel
            in
            case msg of
                NoOp ->
                    ( m, Cmd.none )

                ToggleExpandoCard expanded ->
                    ( { m
                        | customModel =
                            { model
                                | expandoCard = { expanded = expanded }
                            }
                      }
                    , Cmd.none
                    )

                ShowHidePopover ->
                    ( { m
                        | customModel =
                            { model
                                | popover = { isShown = not model.popover.isShown }
                            }
                      }
                    , Cmd.none
                    )
    , menuViewEnhancer = \_ v -> v
    , viewEnhancer = \_ stories -> stories
    , onModeChanged = Just (Maybe.withDefault Light >> onColorModeChanged)
    , documentTitle = Just "Exosphere Design System"
    }


main : UIExplorerProgram Model Msg PluginOptions ()
main =
    exploreWithCategories
        config
        (createCategories
            |> category "Atoms"
                [ storiesOf
                    "Text"
                    [ ( "unstyled", \m -> toHtml (palette m.colorMode) <| Element.text "This is text rendered using `Element.text` and no styling. It will inherit attributes from the document layout.", {} )
                    , ( "p"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Text.p [ Font.justify ]
                                    [ Text.body veryLongCopy
                                    , Text.body "[ref. "
                                    , Link.externalLink (palette m.colorMode) "https://www.lipsum.com/" "www.lipsum.com"
                                    , Text.body "]"
                                    ]
                      , {}
                      )
                    , ( "bold", \m -> toHtml (palette m.colorMode) <| Text.p [] [ Text.body "Logged in as ", Text.bold "@Jimmy:3421", Text.body "." ], {} )
                    , ( "underline"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Text.p []
                                    [ Text.body "Exosphere is a "
                                    , Text.underline "user-friendly"
                                    , Text.body ", extensible client for cloud computing."
                                    ]
                      , {}
                      )
                    , ( "heading"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Text.heading (palette m.colorMode)
                                    []
                                    (FeatherIcons.helpCircle
                                        |> FeatherIcons.toHtml []
                                        |> Element.html
                                        |> Element.el []
                                    )
                                    "Get Support"
                      , {}
                      )
                    , ( "subheading"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Text.subheading (palette m.colorMode)
                                    []
                                    (FeatherIcons.hardDrive
                                        |> FeatherIcons.toHtml []
                                        |> Element.html
                                        |> Element.el []
                                    )
                                    "Volumes"
                      , {}
                      )
                    , ( "h1"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Text.text Text.H1
                                    [ Element.Region.heading 1 ]
                                    "App Config Info"
                      , {}
                      )
                    , ( "h2"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Text.text Text.H2
                                    [ Element.Region.heading 2 ]
                                    "App Config Info"
                      , {}
                      )
                    , ( "h3"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Text.text Text.H3
                                    [ Element.Region.heading 3 ]
                                    "App Config Info"
                      , {}
                      )
                    , ( "h4", \m -> toHtml (palette m.colorMode) <| Text.text Text.H4 [ Element.Region.heading 4 ] "App Config Info", {} )
                    ]
                , storiesOf
                    "Link"
                    [ ( "internal"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Text.p []
                                    [ Text.body "Compare this to plain, old "
                                    , Link.link
                                        (palette m.colorMode)
                                        "http://localhost:8002/#Atoms/Text/underline"
                                        "underlined text"
                                    , Text.body "."
                                    ]
                      , {}
                      )
                    , ( "external"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Text.p []
                                    [ Text.body "Exosphere is a user-friendly, extensible client for cloud computing. Check out our "
                                    , Link.externalLink
                                        (palette m.colorMode)
                                        "https://gitlab.com/exosphere/exosphere/blob/master/README.md"
                                        "README on GitLab"
                                    , Element.text "."
                                    ]
                      , {}
                      )
                    ]
                , storiesOf
                    "Icon"
                    (List.map
                        (\icon ->
                            ( Tuple.first icon, \m -> toHtml (palette m.colorMode) <| defaultIcon (palette m.colorMode) <| Tuple.second icon, {} )
                        )
                        [ ( "bell", bell )
                        , ( "console", console )
                        , ( "copyToClipboard", copyToClipboard )
                        , ( "history", history )
                        , ( "ipAddress", ipAddress )
                        , ( "lock", lock )
                        , ( "lockOpen", lockOpen )
                        , ( "plusCircle", plusCircle )
                        , ( "remove", remove )
                        , ( "roundRect", roundRect )
                        , ( "timesCircle", timesCircle )
                        ]
                    )
                , storiesOf
                    "Button"
                    [ ( "primary"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Button.primary (palette m.colorMode) { text = "Create", onPress = Just NoOp }
                      , {}
                      )
                    , ( "disabled"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Button.primary (palette m.colorMode) { text = "Next", onPress = Nothing }
                      , {}
                      )
                    , ( "secondary"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Button.default (palette m.colorMode) { text = "Next", onPress = Just NoOp }
                      , {}
                      )
                    , ( "warning"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Button.button Button.Warning (palette m.colorMode) { text = "Suspend", onPress = Just NoOp }
                      , {}
                      )
                    , ( "danger"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Button.button Button.Danger (palette m.colorMode) { text = "Delete All", onPress = Just NoOp }
                      , {}
                      )
                    , ( "danger secondary"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Button.button Button.DangerSecondary (palette m.colorMode) { text = "Delete All", onPress = Just NoOp }
                      , {}
                      )
                    ]
                , storiesOf
                    "Badge"
                    [ ( "default", \m -> toHtml (palette m.colorMode) <| badge "Experimental", {} )
                    ]
                , storiesOf
                    "Status Badge"
                    [ ( "good", \m -> toHtml (palette m.colorMode) <| statusBadge (palette m.colorMode) ReadyGood (Element.text "Ready"), {} )
                    , ( "muted", \m -> toHtml (palette m.colorMode) <| statusBadge (palette m.colorMode) Muted (Element.text "Unknown"), {} )
                    , ( "warning", \m -> toHtml (palette m.colorMode) <| statusBadge (palette m.colorMode) Style.Widgets.StatusBadge.Warning (Element.text "Building"), {} )
                    , ( "error", \m -> toHtml (palette m.colorMode) <| statusBadge (palette m.colorMode) Error (Element.text "Error"), {} )
                    ]
                ]
            |> category "Molecules"
                [ storiesOf
                    "Copyable Text"
                    [ ( "default"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                Style.Widgets.CopyableText.copyableText
                                    (palette m.colorMode)
                                    [ Font.family [ Font.monospace ] ]
                                    "192.168.1.1"
                      , {}
                      )
                    ]
                , storiesOf
                    "Card"
                    [ ( "default", \m -> toHtml (palette m.colorMode) <| exoCard (palette m.colorMode) (Element.text "192.168.1.1"), {} )
                    , -- TODO: Render a more complete version of this based on Page.Home.
                      ( "fixed size with hover", \m -> toHtml (palette m.colorMode) <| clickableCardFixedSize (palette m.colorMode) 300 300 [ Element.text "Lorem ipsum dolor sit amet." ], {} )
                    , ( "title & accessories with hover"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                exoCardWithTitleAndSubtitle (palette m.colorMode)
                                    (Style.Widgets.CopyableText.copyableText
                                        (palette m.colorMode)
                                        [ Font.family [ Font.monospace ] ]
                                        "192.168.1.1"
                                    )
                                    (Button.default
                                        (palette m.colorMode)
                                        { text = "Unassign"
                                        , onPress = Just NoOp
                                        }
                                    )
                                    (Element.text "Assigned to a resource that Exosphere cannot represent")
                      , {}
                      )
                    ]
                , storiesOf
                    "Meter"
                    [ ( "default", \m -> toHtml (palette m.colorMode) <| meter (palette m.colorMode) "Space used" "6 of 10 GB" 6 10, {} )
                    ]
                ]
            |> category "Organisms"
                [ storiesOf
                    "Expandable Card"
                    [ ( "default"
                      , \m ->
                            toHtml (palette m.colorMode) <|
                                expandoCard (palette m.colorMode)
                                    m.customModel.expandoCard.expanded
                                    (\next -> ToggleExpandoCard next)
                                    (Element.text "Backup SSD")
                                    (Element.text "25 GB")
                                    (Element.column
                                        VH.contentContainer
                                        [ VH.compactKVRow "Name:" <| Element.text "Backup SSD"
                                        , VH.compactKVRow "Status:" <| Element.text "Ready"
                                        , VH.compactKVRow "Description:" <|
                                            Element.paragraph [ Element.width Element.fill ] <|
                                                [ Element.text "Solid State Drive" ]
                                        , VH.compactKVRow "UUID:" <| copyableText (palette m.colorMode) [] "6205e1a8-9a5d-4325-bb0d-219f09a4d988"
                                        ]
                                    )
                      , {}
                      )
                    ]
                , storiesOf
                    "Chip"
                    [ ( "default", \m -> toHtml (palette m.colorMode) <| chip (palette m.colorMode) Nothing (Element.text "assigned"), {} )
                    , ( "with badge", \m -> toHtml (palette m.colorMode) <| chip (palette m.colorMode) Nothing (Element.row [ Element.spacing 5 ] [ Element.text "ubuntu", badge "10" ]), {} )
                    ]
                , storiesOf
                    "Popover"
                    (List.map
                        (\positionTuple ->
                            ( "position: " ++ Tuple.first positionTuple
                            , \m ->
                                let
                                    popover =
                                        Element.paragraph
                                            (SH.popoverStyleDefaults (palette m.colorMode)
                                                ++ [ Element.padding 20
                                                   , Element.width <| Element.px 300
                                                   , Font.size 16
                                                   ]
                                            )
                                            [ Element.text
                                                "I'm a popover that can be used as dropdown, toggle tip, etc."
                                            ]
                                in
                                toHtml (palette m.colorMode) <|
                                    Element.el [ Element.paddingXY 400 100 ]
                                        (Element.el
                                            (if m.customModel.popover.isShown then
                                                SH.popoverAttribs
                                                    popover
                                                    (Tuple.second positionTuple)
                                                    Nothing

                                             else
                                                []
                                            )
                                            (Button.primary (palette m.colorMode)
                                                { text = "Click me"
                                                , onPress = Just ShowHidePopover
                                                }
                                            )
                                        )
                            , {}
                            )
                        )
                        [ ( "TopLeft", Style.Types.PositionTopLeft )
                        , ( "Top", Style.Types.PositionTop )
                        , ( "TopRight", Style.Types.PositionTopRight )
                        , ( "RightTop", Style.Types.PositionRightTop )
                        , ( "Right", Style.Types.PositionRight )
                        , ( "RightBottom", Style.Types.PositionRightBottom )
                        , ( "BottomRight", Style.Types.PositionBottomRight )
                        , ( "Bottom", Style.Types.PositionBottom )
                        , ( "BottomLeft", Style.Types.PositionBottomLeft )
                        , ( "LeftBottom", Style.Types.PositionLeftBottom )
                        , ( "Left", Style.Types.PositionLeft )
                        , ( "LeftTop", Style.Types.PositionLeftTop )
                        ]
                    )
                ]
            |> category "Templates"
                [ storiesOf
                    "Login"
                    [ ( "default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                ]
            |> category "Pages"
                [ storiesOf
                    "Jetstream2"
                    [ ( "default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                ]
        )
