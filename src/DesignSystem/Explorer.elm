port module DesignSystem.Explorer exposing (main)

import Browser.Events
import Color
import DesignSystem.Helpers exposing (Plugins, palettize, toHtml)
import DesignSystem.Stories.Card as CardStories
import DesignSystem.Stories.ColorPalette as ColorPalette
import DesignSystem.Stories.Link as LinkStories
import DesignSystem.Stories.Text as TextStories
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
import Style.Widgets.IconButton exposing (chip)
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



--- component helpers


{-| Create an icon with standard size & color.
-}
defaultIcon : Style.Types.ExoPalette -> (Element.Color -> number -> icon) -> icon
defaultIcon pal icon =
    icon (pal.neutral.icon |> SH.toElementColor) 25



--- MODEL


{-| Which Popovers are visible?
-}
type alias PopoverState =
    { showPopovers : Set.Set PopoverId }


type alias Model =
    { expandoCard : CardStories.ExpandoCardState
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
                , LinkStories.stories toHtml
                , storiesOf
                    "Icon"
                    (List.map
                        (\icon ->
                            ( Tuple.first icon, \m -> toHtml (palettize m) <| defaultIcon (palettize m) <| Tuple.second icon, { note = """
## Usage

Exosphere has several **custom icons** in `Style.Widgets.Icon` (as shown above). They can be used as:

    Icon.lockOpen (SH.toElementColor context.palette.on.background) 28

For everything else, use `FeatherIcons` (learn more on their package [documentation](https://package.elm-lang.org/packages/1602/elm-feather/latest/FeatherIcons)):

    FeatherIcons.logOut |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                            """ } )
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

Exosphere uses [elm-ui-widgets buttons](https://package.elm-lang.org/packages/Orasund/elm-ui-widgets/latest/Widget#buttons): `Widget.button`, `Widget.textButton`, and `Widget.iconButton`. The styles passed to these functions (`Widget.Style.ButtonStyle msg` type) are usually obtained from "button" containing fields in the record returned by `Style.Helpers.materialStyle`.

We have abstracted a significant part of this by our home-made `Style.Widgets.Button` (see issue [#791](https://gitlab.com/exosphere/exosphere/-/issues/791) for more context). You can use `Style.Widgets.Button.button` function for the following variants of text buttons:

- **Primary**: Used for most important call-to-action button on a page which is normally at most 1 per page.

- **Secondary**: The most commonly used button for general actions on a page which need less emphasis than primary button. It is also available as `Style.Widgets.Button.default` for convenience.

- **Warning**: Used when an action has reversible consequences with a major impact.

- **Danger**: Used when an action is destructive and/or has irreversible consequences.

- **Danger Secondary**: Same as Danger but action doesn't have immediate irreversible consequences, mostly becuase it takes to a confirmation page/dialog.
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

Use `Style.Widgets.Card.badge` to annotate additional but important information like marking features as "Experimental".

It can also be combined within components to show extra details like counts.

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

Use `Style.Widgets.StatusBadge.statusBadge` to display a read-only label which clearly shows the current status of a resource (usually a server).

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
                [ storiesOf
                    "Chip"
                    [ --TODO: Replace this component with the `filterChipView` inside DataList since `chip` is not in use.
                      ( "default", \m -> toHtml (palettize m) <| chip (palettize m) Nothing (Element.text "assigned"), { note = "*Usage will be updated soon (see issue [#790](https://gitlab.com/exosphere/exosphere/-/issues/790))*" } )
                    , ( "with badge", \m -> toHtml (palettize m) <| chip (palettize m) Nothing (Element.row [ Element.spacing 5 ] [ Element.text "ubuntu", badge "10" ]), { note = "*Usage will be updated soon (see issue [#790](https://gitlab.com/exosphere/exosphere/-/issues/790))*" } )
                    ]
                , storiesOf
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

Use `Style.Widgets.CopyableText.copyableText` to show stylable text with an accessory button for copying the text content to the user's clipboard.

It uses [clipboard.js](https://clipboardjs.com/) under the hood & relies on a [port for initialisation](https://gitlab.com/exosphere/exosphere/-/blob/master/ports.js#L101). This is why copying doesn't work on this design-system page yet.
                        """ }
                      )
                    ]
                , storiesOf
                    "Meter"
                    [ ( "default", \m -> toHtml (palettize m) <| meter (palettize m) "Space used" "6 of 10 GB" 6 10, { note = """
## Usage

Use `Style.Widgets.Meter.meter` to show a static horizontal progress bar chart which indicates capacity used of a resource.

Besides the obvious `value` and `maximum`, it takes:
- `title` that is shown on the left side, often representing what meter indicates. 
- `subtitle` that is shown on the right side, often represnting value and maximum in words. For e.g. "<value> of <maximum> <units>".
                    """ } )
                    ]
                ]
            |> category "Organisms"
                [ CardStories.stories toHtml { onPress = Just NoOp, onExpand = \next -> ToggleExpandoCard next }

                -- TODO: also add stories for special popovers
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
Use `Style.Widgets.Popover.Popover.popover` for creating a popover. Most importantly, it takes a **target** element that opens/closes the popover and a **content** element that is the popover body. To learn about all the parameters it takes, check out the docstring of this function.

`popover` function makes your life easier by controlling opening/closing. But if you want to control that yourself for a specific case (for e.g. when message emitted by a target should not toggle popover visiblity), you can use `Style.Widgets.Popover.Popover.popoverAttribs`. It returns the [nearby element](https://package.elm-lang.org/packages/mdgriffith/elm-ui/1.1.8/Element#nearby-elements) attributes that need to be passed to the target element when popover is shown.

By taking control in your hands, you must remember to style the popover same as all other popover elements in Exosphere. This can be achieved by using `Style.Widgets.Popover.Popover.popoverStyleDefaults` that are the default style attributes for popover body as the name suggests.

### Special Popovers

#### Toggle Tip
Use `Style.Widgets.ToggleTip.toggleTip` for showing an icon-button that toggles the tip (a popover) on clicking.

#### Dropdown
We don't have a dedicated function for dropdown yet. But it can be achieved by creating a list of dropdown items as buttons with `Style.Helpers.dropdownItemStyle` applied, and then passing that list as a column to the `content` of `popover` function.

#### Delete Popconfirm
You can use `Style.Widgets.DeleteButton.deletePopconfirm` to show a confirmation popover on pressing a delete button (since it's an irreversible action).

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
