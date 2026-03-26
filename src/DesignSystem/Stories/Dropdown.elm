module DesignSystem.Stories.Dropdown exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, palettize)
import Element
import FeatherIcons as Icons
import Set
import Style.Types
import Style.Widgets.Button as Button
import Style.Widgets.Dropdown exposing (dropdown)
import Style.Widgets.Icon exposing (sizedFeatherIcon)
import Style.Widgets.Popover.Popover exposing (dropdownItemStyle)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import UIExplorer exposing (storiesOf)
import Widget


notes : String
notes =
    """
## Usage

A dropdown menu built on top of [Popover](/#Organisms/Popover/position:%20TopLeft).

It provides a standard target button with a chevron indicator and positions the content below-right.

    Dropdown.dropdown context
        msgMapper
        { id = "myDropdown"
        , label = "Actions"
        , content =
            \\closeDropdown ->
                Element.column [ Element.spacing spacer.px8 ]
                    [ Element.el [ closeDropdown ] (Element.text "Item 1")
                    , Element.el [ closeDropdown ] (Element.text "Item 2")
                    ]
        }

### Content

The `content` function receives a `closeDropdown` attribute. Attach it to any element that should close the dropdown when clicked (e.g. action buttons).

Use `dropdownItemStyle` from `Style.Widgets.Popover.Popover` for consistent item styling.
"""


stories :
    Renderer msg
    -> (PopoverId -> msg)
    -> Maybe msg
    ->
        UIExplorer.UI
            { model
                | deployerColors : Style.Types.DeployerColorThemes
                , popover : { showPopovers : Set.Set PopoverId }
            }
            msg
            Plugins
stories renderer tagger onPress =
    storiesOf
        "Dropdown"
        [ ( "actions"
          , \m ->
                let
                    palette =
                        palettize m
                in
                renderer palette <|
                    dropdown
                        { palette = palette, showPopovers = m.customModel.popover.showPopovers }
                        tagger
                        { id = "explorerDemoDropdownActions"
                        , label = "Actions"
                        , content =
                            \closeDropdown ->
                                Element.column [ Element.spacing spacer.px8 ]
                                    [ Element.el [ closeDropdown ] <|
                                        Button.button Button.Secondary
                                            palette
                                            { text = "Lock"
                                            , onPress = onPress
                                            }
                                    , Element.el [ closeDropdown ] <|
                                        Button.button Button.Primary
                                            palette
                                            { text = "Unshelve"
                                            , onPress = onPress
                                            }
                                    , Element.el [ closeDropdown ] <|
                                        Button.button Button.Danger
                                            palette
                                            { text = "Delete"
                                            , onPress = onPress
                                            }
                                    ]
                        }
          , { note = notes }
          )
        , ( "create"
          , \m ->
                let
                    palette =
                        palettize m
                in
                renderer palette <|
                    dropdown
                        { palette = palette, showPopovers = m.customModel.popover.showPopovers }
                        tagger
                        { id = "explorerDemoDropdownCreate"
                        , label = "Create"
                        , content =
                            \closeDropdown ->
                                Element.column []
                                    [ Element.el [ Element.width Element.fill, closeDropdown ] <|
                                        Widget.button
                                            (dropdownItemStyle palette)
                                            { icon = sizedFeatherIcon 18 Icons.server
                                            , text = "Instance"
                                            , onPress = onPress
                                            }
                                    , Element.el [ Element.width Element.fill, closeDropdown ] <|
                                        Widget.button
                                            (dropdownItemStyle palette)
                                            { icon = sizedFeatherIcon 18 Icons.hardDrive
                                            , text = "Volume"
                                            , onPress = onPress
                                            }
                                    , Element.el [ Element.width Element.fill, closeDropdown ] <|
                                        Widget.button
                                            (dropdownItemStyle palette)
                                            { icon = sizedFeatherIcon 18 Icons.share2
                                            , text = "Share"
                                            , onPress = onPress
                                            }
                                    ]
                        }
          , { note = notes }
          )
        ]
