port module DesignSystem.Explorer exposing (main)

import Browser.Events
import Color
import DesignSystem.Helpers exposing (Plugins, palettize, toHtml)
import DesignSystem.Stories.Card as CardStories
import DesignSystem.Stories.ColorPalette as ColorPalette
import DesignSystem.Stories.Link as LinkStories
import DesignSystem.Stories.Text as TextStories
import DesignSystem.Stories.Toast as ToastStories
import Element
import Element.Font as Font
import Html
import Html.Attributes exposing (src, style)
import Set
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Button as Button
import Style.Widgets.Card exposing (badge)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon exposing (bell, console, copyToClipboard, history, ipAddress, lock, lockOpen, plusCircle, remove, roundRect, timesCircle)
import Style.Widgets.Meter exposing (meter)
import Style.Widgets.Popover.Popover exposing (popover, toggleIfTargetIsOutside)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.StatusBadge exposing (StatusBadgeState(..), statusBadge)
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



--- MODEL


{-| Which Popovers are visible?
-}
type alias PopoverState =
    { showPopovers : Set.Set PopoverId }


type alias Model =
    { popover : PopoverState
    , deployerColors : Style.Types.DeployerColorThemes
    , tabs : TabsPlugin.Model
    }


initialModel : Model
initialModel =
    { deployerColors = Style.Types.defaultColors
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
                , LinkStories.stories toHtml
                , storiesOf
                    "Icon"
                    (List.map
                        (\widget ->
                            ( Tuple.first widget
                            , \m ->
                                let
                                    palette =
                                        palettize m

                                    icon =
                                        Tuple.second widget
                                in
                                toHtml palette <|
                                    icon (palette.neutral.icon |> SH.toElementColor) 25
                            , { note = """
## Usage

Exosphere has several **custom icons**.

For everything else, use [FeatherIcons](https://package.elm-lang.org/packages/1602/elm-feather/latest/FeatherIcons):

    FeatherIcons.logOut |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                            """ }
                            )
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
                    (List.map
                        (\button ->
                            ( button.name
                            , \m ->
                                toHtml (palettize m) <|
                                    button.widget (palettize m) { text = button.text, onPress = button.onPress }
                            , { note = """
## Usage

Exosphere uses buttons from [elm-ui-widgets](https://package.elm-lang.org/packages/Orasund/elm-ui-widgets/latest/Widget#buttons).

### Variants

- **Primary**: Used for the most important call-to-action on a page.

- **Secondary**: The most commonly used (or default) button.

- **Warning**: Used when an action has reversible consequences with a major impact.

- **Danger**: Used when an action is destructive and/or has irreversible consequences.

- **Danger Secondary**: For non-immediate but irreversible actions, such as those followed by a confirmation alert.
                            """ }
                            )
                        )
                        [ { name = "primary", widget = Button.primary, text = "Create", onPress = Just NoOp }
                        , { name = "primary: disabled", widget = Button.primary, text = "Next", onPress = Nothing }
                        , { name = "secondary", widget = Button.default, text = "Next", onPress = Just NoOp }
                        , { name = "warning", widget = Button.button Button.Warning, text = "Suspend", onPress = Just NoOp }
                        , { name = "danger", widget = Button.button Button.Danger, text = "Delete All", onPress = Just NoOp }
                        , { name = "danger secondary", widget = Button.button Button.DangerSecondary, text = "Delete All", onPress = Just NoOp }
                        ]
                    )
                , storiesOf
                    "Badge"
                    [ ( "default", \m -> toHtml (palettize m) <| badge "Experimental", { note = """
## Usage

To annotate additional but important information like marking features as "Experimental".

_This component is being deprecated._

### Alternatives

If you are looking for a way to display removable tags, consider a [chip](/#Organisms/Chip).

If you want to show a resource's current state or provide feedback on a process, consider using a [status badge](/#Atoms/Status%20Badge).
                        """ } )
                    ]
                , storiesOf
                    "Status Badge"
                    (List.map
                        (\status ->
                            ( status.name
                            , \m ->
                                toHtml (palettize m) <|
                                    statusBadge (palettize m) status.variant status.text
                            , { note = """
## Usage

To display a read-only label which clearly shows the current status of a resource (usually a server).
                        """ }
                            )
                        )
                        [ { name = "good", variant = ReadyGood, text = Element.text "Ready" }
                        , { name = "muted", variant = Muted, text = Element.text "Unknown" }
                        , { name = "warning", variant = Style.Widgets.StatusBadge.Warning, text = Element.text "Building" }
                        , { name = "error", variant = Error, text = Element.text "Error" }
                        ]
                    )
                ]
            |> category "Molecules"
                [ --TODO: Add `filterChipView` (inside DataList) since `chip` is not in use.
                  storiesOf
                    "Copyable Text"
                    [ ( "default"
                      , \m ->
                            toHtml (palettize m) <|
                                copyableText
                                    (palettize m)
                                    [ Font.family [ Font.monospace ]
                                    , Element.width Element.shrink
                                    ]
                                    "192.168.1.1"
                      , { note = """
## Usage

Shows stylable text with an accessory button for copying the text content to the user's clipboard.

It uses [clipboard.js](https://clipboardjs.com/) under the hood & relies on a [port for initialisation](https://gitlab.com/exosphere/exosphere/-/blob/master/ports.js#L101).
                        """ }
                      )
                    ]
                , storiesOf
                    "Meter"
                    [ ( "default", \m -> toHtml (palettize m) <| meter (palettize m) "Space used" "6 of 10 GB" 6 10, { note = """
## Usage

Shows a static horizontal progress bar chart which indicates the capacity of a resource.

- `title` indicates the resource.
- `subtitle` represents the value and maximum in words e.g. "<value> of <maximum> <units>".
                    """ } )
                    ]
                ]
            |> category "Organisms"
                [ CardStories.stories toHtml
                , ToastStories.stories toHtml { onPress = Just NoOp }
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
                            , { note = """
## Usage

Takes a **target** element that opens/closes the popover and a **content** element for the popover body.

### Special Cases

#### Tooltip

Use `toggleTip` to show an icon button that toggles a hint when clicked.

#### Dropdown

Use a list of dropdown items with `dropdownItemStyle` to create a dropdown.

#### Confirmation Popup

Use `deletePopconfirm` to show a confirmation popover after pressing a delete button.

### Advanced

By default, the popover manages its own toggle state.

For advanced usage, `popoverAttribs` returns the [nearby elements](https://package.elm-lang.org/packages/mdgriffith/elm-ui/1.1.8/Element#nearby-elements) required by the target element when the popover is shown.

Use `popoverStyleDefaults` so that the popover style is still consistent with others in the application.
                            """ }
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
