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
import View.Helpers as VH



--- elm-ui helpers


toHtml : Element.Element msg -> Html.Html msg
toHtml a =
    Element.layout [] <| a


veryLongCopy : String
veryLongCopy =
    """
    Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC,
    making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words,
    consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source.
    Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC.
    This book is a treatise on the theory of ethics, very popular during the Renaissance.
    """



--- theme


palette : Style.Types.ExoPalette
palette =
    SH.toExoPalette
        Style.Types.defaultColors
        { theme = Style.Types.Override Style.Types.Light, systemPreference = Nothing }



--- component helpers


text : String -> Element.Element msg
text msg =
    Element.text msg


defaultIcon : (Element.Color -> number -> icon) -> icon
defaultIcon icon =
    icon (palette.on.background |> SH.toElementColor) 25



--- MODEL


type alias ExpandoCardState =
    { expanded : Bool }


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
    , init =
        \_ m -> m
    , enableDarkMode = True
    , subscriptions =
        \_ -> Sub.none
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
    , onModeChanged = Nothing
    , documentTitle = Just "Exosphere Design System"
    }


main : UIExplorerProgram Model Msg PluginOptions ()
main =
    let
        popover =
            Element.paragraph
                (SH.popoverStyleDefaults palette
                    ++ [ Element.padding 20
                       , Element.width <| Element.px 300
                       , Font.size 16
                       ]
                )
                [ Element.text
                    "I'm a popover that can be used as dropdown, toggle tip, etc."
                ]
    in
    exploreWithCategories
        config
        (createCategories
            |> category "Atoms"
                [ storiesOf
                    "Text"
                    [ ( "unstyled", \_ -> toHtml <| text "This is text rendered using `Element.text` and no styling. It will inherit attributes from the document layout.", {} )
                    , ( "p"
                      , \_ ->
                            toHtml <|
                                Text.p [ Font.justify ]
                                    [ Text.body veryLongCopy
                                    , Text.body "[ref. "
                                    , Link.externalLink palette "https://www.lipsum.com/" "www.lipsum.com"
                                    , Text.body "]"
                                    ]
                      , {}
                      )
                    , ( "bold", \_ -> toHtml <| Text.p [] [ Text.body "Logged in as ", Text.bold "@Jimmy:3421", Text.body "." ], {} )
                    , ( "underline"
                      , \_ ->
                            toHtml <|
                                Text.p []
                                    [ Text.body "Exosphere is a "
                                    , Text.underline "user-friendly"
                                    , Text.body ", extensible client for cloud computing."
                                    ]
                      , {}
                      )
                    , ( "heading"
                      , \_ ->
                            toHtml <|
                                Text.heading palette
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
                      , \_ ->
                            toHtml <|
                                Text.subheading palette
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
                      , \_ ->
                            toHtml <|
                                Text.text Text.H1
                                    [ Element.Region.heading 1 ]
                                    "App Config Info"
                      , {}
                      )
                    , ( "h2"
                      , \_ ->
                            toHtml <|
                                Text.text Text.H2
                                    [ Element.Region.heading 2 ]
                                    "App Config Info"
                      , {}
                      )
                    , ( "h3"
                      , \_ ->
                            toHtml <|
                                Text.text Text.H3
                                    [ Element.Region.heading 3 ]
                                    "App Config Info"
                      , {}
                      )
                    , ( "h4", \_ -> toHtml <| Text.text Text.H4 [ Element.Region.heading 4 ] "App Config Info", {} )
                    ]
                , storiesOf
                    "Link"
                    [ ( "internal"
                      , \_ ->
                            toHtml <|
                                Text.p []
                                    [ Text.body "Compare this to plain, old "
                                    , Link.link
                                        palette
                                        "http://localhost:8002/#Atoms/Text/underline"
                                        "underlined text"
                                    , Text.body "."
                                    ]
                      , {}
                      )
                    , ( "external"
                      , \_ ->
                            toHtml <|
                                Text.p []
                                    [ Text.body "Exosphere is a user-friendly, extensible client for cloud computing. Check out our "
                                    , Link.externalLink
                                        palette
                                        "https://gitlab.com/exosphere/exosphere/blob/master/README.md"
                                        "README on GitLab"
                                    , Element.text "."
                                    ]
                      , {}
                      )
                    ]
                , storiesOf
                    "Icon"
                    [ ( "bell", \_ -> toHtml <| defaultIcon <| bell, {} )
                    , ( "console", \_ -> toHtml <| defaultIcon <| console, {} )
                    , ( "copyToClipboard", \_ -> toHtml <| defaultIcon <| copyToClipboard, {} )
                    , ( "history", \_ -> toHtml <| defaultIcon <| history, {} )
                    , ( "ipAddress", \_ -> toHtml <| defaultIcon <| ipAddress, {} )
                    , ( "lock", \_ -> toHtml <| defaultIcon <| lock, {} )
                    , ( "lockOpen", \_ -> toHtml <| defaultIcon <| lockOpen, {} )
                    , ( "plusCircle", \_ -> toHtml <| defaultIcon <| plusCircle, {} )
                    , ( "remove", \_ -> toHtml <| defaultIcon <| remove, {} )
                    , ( "roundRect", \_ -> toHtml <| defaultIcon <| roundRect, {} )
                    , ( "timesCircle", \_ -> toHtml <| defaultIcon <| timesCircle, {} )
                    ]
                , storiesOf
                    "Button"
                    [ ( "primary"
                      , \_ ->
                            toHtml <|
                                Button.primary palette { text = "Create", onPress = Just NoOp }
                      , {}
                      )
                    , ( "disabled"
                      , \_ ->
                            toHtml <|
                                Button.primary palette { text = "Next", onPress = Nothing }
                      , {}
                      )
                    , ( "secondary"
                      , \_ ->
                            toHtml <|
                                Button.default palette { text = "Next", onPress = Just NoOp }
                      , {}
                      )
                    , ( "warning"
                      , \_ ->
                            toHtml <|
                                Button.button Button.Warning palette { text = "Suspend", onPress = Just NoOp }
                      , {}
                      )
                    , ( "danger"
                      , \_ ->
                            toHtml <|
                                Button.button Button.Danger palette { text = "Delete All", onPress = Just NoOp }
                      , {}
                      )
                    , ( "danger secondary"
                      , \_ ->
                            toHtml <|
                                Button.button Button.DangerSecondary palette { text = "Delete All", onPress = Just NoOp }
                      , {}
                      )
                    ]
                , storiesOf
                    "Badge"
                    [ ( "default", \_ -> toHtml <| badge "Experimental", {} )
                    ]
                , storiesOf
                    "Status Badge"
                    [ ( "good", \_ -> toHtml <| statusBadge palette ReadyGood (text "Ready"), {} )
                    , ( "muted", \_ -> toHtml <| statusBadge palette Muted (text "Unknown"), {} )
                    , ( "warning", \_ -> toHtml <| statusBadge palette Style.Widgets.StatusBadge.Warning (text "Building"), {} )
                    , ( "error", \_ -> toHtml <| statusBadge palette Error (text "Error"), {} )
                    ]
                ]
            |> category "Molecules"
                [ storiesOf
                    "Copyable Text"
                    [ ( "default"
                      , \_ ->
                            toHtml <|
                                Style.Widgets.CopyableText.copyableText
                                    palette
                                    [ Font.family [ Font.monospace ] ]
                                    "192.168.1.1"
                      , {}
                      )
                    ]
                , storiesOf
                    "Card"
                    [ ( "default", \_ -> toHtml <| exoCard palette (text "192.168.1.1"), {} )
                    , -- TODO: Render a more complete version of this based on Page.Home.
                      ( "fixed size with hover", \_ -> toHtml <| clickableCardFixedSize palette 300 300 [ text "Lorem ipsum dolor sit amet." ], {} )
                    , ( "title & accessories with hover"
                      , \_ ->
                            toHtml <|
                                exoCardWithTitleAndSubtitle palette
                                    (Style.Widgets.CopyableText.copyableText
                                        palette
                                        [ Font.family [ Font.monospace ] ]
                                        "192.168.1.1"
                                    )
                                    (Button.default
                                        palette
                                        { text = "Unassign"
                                        , onPress = Just NoOp
                                        }
                                    )
                                    (text "Assigned to a resource that Exosphere cannot represent")
                      , {}
                      )
                    ]
                , storiesOf
                    "Meter"
                    [ ( "default", \_ -> toHtml <| meter palette "Space used" "6 of 10 GB" 6 10, {} )
                    ]
                , storiesOf
                    "Input"
                    [ ( "default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                ]
            |> category "Organisms"
                [ storiesOf
                    "Expandable Card"
                    [ ( "default"
                      , \m ->
                            toHtml <|
                                expandoCard palette
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
                                        , VH.compactKVRow "UUID:" <| copyableText palette [] "6205e1a8-9a5d-4325-bb0d-219f09a4d988"
                                        ]
                                    )
                      , {}
                      )
                    ]
                , storiesOf
                    "Chip"
                    [ ( "default", \_ -> toHtml <| chip palette Nothing (Element.text "assigned"), {} )
                    , ( "with badge", \_ -> toHtml <| chip palette Nothing (Element.row [ Element.spacing 5 ] [ Element.text "ubuntu", badge "10" ]), {} )
                    ]
                , storiesOf
                    "Popover"
                    (List.map
                        (\positionTuple ->
                            ( "position: " ++ Tuple.first positionTuple
                            , \m ->
                                toHtml <|
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
                                            (Button.primary palette
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
                , storiesOf
                    "Create"
                    [ ( "default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                , storiesOf
                    "List"
                    [ ( "default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                , storiesOf
                    "Detail"
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
