port module DesignSystem.Explorer exposing (main)

import Browser.Events
import Color
import DesignSystem.Helpers exposing (Plugins, palettize, toHtml)
import DesignSystem.Stories.ColorPalette as ColorPalette
import DesignSystem.Stories.Text as TextStories
import Element
import Element.Font as Font
import Html
import Html.Attributes exposing (src, style)
import Set
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Button as Button
import Style.Widgets.Card exposing (badge, clickableCardFixedSize, exoCard, exoCardWithTitleAndSubtitle, expandoCard)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon exposing (bell, console, copyToClipboard, history, ipAddress, lock, lockOpen, plusCircle, remove, roundRect, timesCircle)
import Style.Widgets.IconButton exposing (chip)
import Style.Widgets.Link as Link
import Style.Widgets.Meter exposing (meter)
import Style.Widgets.Popover.Popover exposing (popover, toggleIfTargetIsOutside)
import Style.Widgets.Popover.Types exposing (PopoverId)
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
import UIExplorer.ColorMode exposing (ColorMode(..), colorModeToString)
import UIExplorer.Plugins.Note as NotePlugin
import UIExplorer.Plugins.Tabs as TabsPlugin
import UIExplorer.Plugins.Tabs.Icons as TabsIconsPlugin
import View.Helpers as VH



--- theme


{-| Extracts brand colors from the config.js flags set for this application.
-}
deployerColors : Flags -> Style.Types.DeployerColorThemes
deployerColors flags =
    case flags.palette of
        Just pal ->
            { light =
                { primary = Color.rgb255 pal.light.primary.r pal.light.primary.g pal.light.primary.b
                , secondary = Color.rgb255 pal.light.secondary.r pal.light.secondary.g pal.light.secondary.b
                }
            , dark =
                { primary = Color.rgb255 pal.dark.primary.r pal.dark.primary.g pal.dark.primary.b
                , secondary = Color.rgb255 pal.dark.secondary.r pal.dark.secondary.g pal.dark.secondary.b
                }
            }

        Nothing ->
            Style.Types.defaultColors


{-| Port for signalling a color mode change to the html doc.

    ref. https://github.com/kalutheo/elm-ui-explorer/blob/master/examples/button/ExplorerWithNotes.elm#L22

-}
port onModeChanged : String -> Cmd msg



--- component helpers


{-| Create an icon with standard size & color.
-}
defaultIcon : Style.Types.ExoPalette -> (Element.Color -> number -> icon) -> icon
defaultIcon pal icon =
    icon (pal.neutral.icon |> SH.toElementColor) 25



--- MODEL


{-| Is the Expandable Card expanded or collapsed?
-}
type alias ExpandoCardState =
    { expanded : Bool }


{-| Which Popovers are visible?
-}
type alias PopoverState =
    { showPopovers : Set.Set PopoverId }


type alias Model =
    { expandoCard : ExpandoCardState
    , popover : PopoverState
    , deployerColors : Style.Types.DeployerColorThemes
    , tabs : TabsPlugin.Model
    }


initialModel : Model
initialModel =
    { expandoCard = { expanded = False }
    , deployerColors = Style.Types.defaultColors
    , popover = { showPopovers = Set.empty }
    , tabs = TabsPlugin.initialModel
    }



--- FLAGS


{-| Flags given to the Explorer on startup.

This is a pared down version of Exosphere's `Types.Flags`.

-}
type alias Flags =
    { palette :
        Maybe
            { light :
                { primary :
                    { r : Int
                    , g : Int
                    , b : Int
                    }
                , secondary :
                    { r : Int
                    , g : Int
                    , b : Int
                    }
                }
            , dark :
                { primary :
                    { r : Int
                    , g : Int
                    , b : Int
                    }
                , secondary :
                    { r : Int
                    , g : Int
                    , b : Int
                    }
                }
            }
    }



--- UPDATE


type Msg
    = NoOp
    | ToggleExpandoCard Bool
    | TogglePopover PopoverId
    | TabMsg TabsPlugin.Msg



--- MAIN


config : Config Model Msg Plugins Flags
config =
    { customModel = initialModel
    , customHeader =
        Just
            { title = "Exosphere Design System"
            , logo = UIExplorer.logoFromHtml (Html.img [ src "assets/img/logo-alt.svg", style "padding-top" "10px", style "padding-left" "5px" ] [])
            , titleColor = Just "#FFFFFF"
            , bgColor = Just "#181725"
            }
    , init = \f m -> { m | deployerColors = deployerColors f }
    , enableDarkMode = True
    , subscriptions =
        \m ->
            Sub.batch <|
                List.map
                    (\popoverId ->
                        Browser.Events.onMouseDown
                            (toggleIfTargetIsOutside popoverId TogglePopover)
                    )
                    (Set.toList m.customModel.popover.showPopovers)
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

                TogglePopover popoverId ->
                    ( { m
                        | customModel =
                            { model
                                | popover =
                                    { showPopovers =
                                        if Set.member popoverId model.popover.showPopovers then
                                            Set.remove popoverId model.popover.showPopovers

                                        else
                                            Set.insert popoverId model.popover.showPopovers
                                    }
                            }
                      }
                    , Cmd.none
                    )

                TabMsg submsg ->
                    let
                        cm =
                            m.customModel
                    in
                    ( { m | customModel = { cm | tabs = TabsPlugin.update submsg m.customModel.tabs } }, Cmd.none )
    , menuViewEnhancer = \_ v -> v
    , viewEnhancer =
        \m stories ->
            let
                colorMode =
                    m.colorMode |> Maybe.withDefault Light
            in
            Html.div []
                [ stories
                , TabsPlugin.view colorMode
                    m.customModel.tabs
                    [ ( "Notes", NotePlugin.viewEnhancer m, TabsIconsPlugin.note )
                    ]
                    TabMsg
                ]
    , onModeChanged = Just (onModeChanged << colorModeToString << Maybe.withDefault Light)
    , documentTitle = Just "Exosphere Design System"
    }


main : UIExplorerProgram Model Msg Plugins Flags
main =
    exploreWithCategories
        config
        (createCategories
            |> category "Atoms"
                [ ColorPalette.stories toHtml
                , TextStories.stories toHtml
                , storiesOf
                    "Link"
                    [ ( "internal"
                      , \m ->
                            toHtml (palettize m) <|
                                Text.p []
                                    [ Text.body "Compare this to plain, old "
                                    , Link.link
                                        (palettize m)
                                        "http://localhost:8002/#Atoms/Text/underline"
                                        "underlined text"
                                    , Text.body "."
                                    ]
                      , { note = "" }
                      )
                    , ( "external"
                      , \m ->
                            toHtml (palettize m) <|
                                Text.p []
                                    [ Text.body "Exosphere is a user-friendly, extensible client for cloud computing. Check out our "
                                    , Link.externalLink
                                        (palettize m)
                                        "https://gitlab.com/exosphere/exosphere/blob/master/README.md"
                                        "README on GitLab"
                                    , Element.text "."
                                    ]
                      , { note = "" }
                      )
                    ]
                , storiesOf
                    "Icon"
                    (List.map
                        (\icon ->
                            ( Tuple.first icon, \m -> toHtml (palettize m) <| defaultIcon (palettize m) <| Tuple.second icon, { note = "" } )
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
                            toHtml (palettize m) <|
                                Button.primary (palettize m) { text = "Create", onPress = Just NoOp }
                      , { note = "" }
                      )
                    , ( "disabled"
                      , \m ->
                            toHtml (palettize m) <|
                                Button.primary (palettize m) { text = "Next", onPress = Nothing }
                      , { note = "" }
                      )
                    , ( "secondary"
                      , \m ->
                            toHtml (palettize m) <|
                                Button.default (palettize m) { text = "Next", onPress = Just NoOp }
                      , { note = "" }
                      )
                    , ( "warning"
                      , \m ->
                            toHtml (palettize m) <|
                                Button.button Button.Warning (palettize m) { text = "Suspend", onPress = Just NoOp }
                      , { note = "" }
                      )
                    , ( "danger"
                      , \m ->
                            toHtml (palettize m) <|
                                Button.button Button.Danger (palettize m) { text = "Delete All", onPress = Just NoOp }
                      , { note = "" }
                      )
                    , ( "danger secondary"
                      , \m ->
                            toHtml (palettize m) <|
                                Button.button Button.DangerSecondary (palettize m) { text = "Delete All", onPress = Just NoOp }
                      , { note = "" }
                      )
                    ]
                , storiesOf
                    "Badge"
                    [ ( "default", \m -> toHtml (palettize m) <| badge "Experimental", { note = "" } )
                    ]
                , storiesOf
                    "Status Badge"
                    [ ( "good", \m -> toHtml (palettize m) <| statusBadge (palettize m) ReadyGood (Element.text "Ready"), { note = "" } )
                    , ( "muted", \m -> toHtml (palettize m) <| statusBadge (palettize m) Muted (Element.text "Unknown"), { note = "" } )
                    , ( "warning", \m -> toHtml (palettize m) <| statusBadge (palettize m) Style.Widgets.StatusBadge.Warning (Element.text "Building"), { note = "" } )
                    , ( "error", \m -> toHtml (palettize m) <| statusBadge (palettize m) Error (Element.text "Error"), { note = "" } )
                    ]
                ]
            |> category "Molecules"
                [ storiesOf
                    "Copyable Text"
                    [ ( "default"
                      , \m ->
                            toHtml (palettize m) <|
                                Style.Widgets.CopyableText.copyableText
                                    (palettize m)
                                    [ Font.family [ Font.monospace ] ]
                                    "192.168.1.1"
                      , { note = "" }
                      )
                    ]
                , storiesOf
                    "Card"
                    [ ( "default", \m -> toHtml (palettize m) <| exoCard (palettize m) (Element.text "192.168.1.1"), { note = "" } )
                    , -- TODO: Render a more complete version of this based on Page.Home.
                      ( "fixed size with hover", \m -> toHtml (palettize m) <| clickableCardFixedSize (palettize m) 300 300 [ Element.text "Lorem ipsum dolor sit amet." ], { note = "" } )
                    , ( "title & accessories with hover"
                      , \m ->
                            toHtml (palettize m) <|
                                exoCardWithTitleAndSubtitle (palettize m)
                                    (Style.Widgets.CopyableText.copyableText
                                        (palettize m)
                                        [ Font.family [ Font.monospace ] ]
                                        "192.168.1.1"
                                    )
                                    (Button.default
                                        (palettize m)
                                        { text = "Unassign"
                                        , onPress = Just NoOp
                                        }
                                    )
                                    (Element.text "Assigned to a resource that Exosphere cannot represent")
                      , { note = "" }
                      )
                    ]
                , storiesOf
                    "Meter"
                    [ ( "default", \m -> toHtml (palettize m) <| meter (palettize m) "Space used" "6 of 10 GB" 6 10, { note = "" } )
                    ]
                ]
            |> category "Organisms"
                [ storiesOf
                    "Expandable Card"
                    [ ( "default"
                      , \m ->
                            toHtml (palettize m) <|
                                expandoCard (palettize m)
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
                                        , VH.compactKVRow "UUID:" <| copyableText (palettize m) [] "6205e1a8-9a5d-4325-bb0d-219f09a4d988"
                                        ]
                                    )
                      , { note = "" }
                      )
                    ]
                , storiesOf
                    "Chip"
                    [ ( "default", \m -> toHtml (palettize m) <| chip (palettize m) Nothing (Element.text "assigned"), { note = "" } )
                    , ( "with badge", \m -> toHtml (palettize m) <| chip (palettize m) Nothing (Element.row [ Element.spacing 5 ] [ Element.text "ubuntu", badge "10" ]), { note = "" } )
                    ]
                , storiesOf
                    "Popover"
                    (List.map
                        (\positionTuple ->
                            ( "position: " ++ Tuple.first positionTuple
                            , \m ->
                                let
                                    demoPopoverContent _ =
                                        Element.paragraph
                                            [ Element.width <| Element.px 275
                                            , Font.size 16
                                            ]
                                            [ Element.text <|
                                                "I'm a popover that can be used as dropdown, toggle tip, etc. "
                                                    ++ "Clicking outside of me will close me."
                                            ]

                                    demoPopoverTarget togglePopoverMsg _ =
                                        Button.primary (palettize m)
                                            { text = "Click me"
                                            , onPress = Just togglePopoverMsg
                                            }

                                    demoPopover =
                                        popover
                                            { palette = palettize m
                                            , showPopovers = m.customModel.popover.showPopovers
                                            }
                                            TogglePopover
                                            { id = "explorerDemoPopover"
                                            , content = demoPopoverContent
                                            , contentStyleAttrs = [ Element.padding 20 ]
                                            , position = Tuple.second positionTuple
                                            , distanceToTarget = Nothing
                                            , target = demoPopoverTarget
                                            , targetStyleAttrs = []
                                            }
                                in
                                toHtml (palettize m) <|
                                    Element.el [ Element.paddingXY 400 100 ]
                                        demoPopover
                            , { note = "" }
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
        )
