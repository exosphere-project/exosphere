module DesignSystem.Stories.IconButton exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Element.Background
import Element.Font as Font
import FeatherIcons exposing (edit2, logOut)
import Style.Helpers as SH exposing (toElementColor)
import Style.Widgets.Icon as Icon exposing (sizedFeatherIcon)
import Style.Widgets.IconButton exposing (FlowOrder(..), clickableIcon, goToButton, navIconButton, notes)
import Style.Widgets.Spacer exposing (spacer)
import UIExplorer exposing (storiesOf)
import Widget


stories :
    Renderer msg
    -> Maybe msg
    -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer onPress =
    storiesOf "Icon Buttons" <|
        [ ( "icon button"
          , \m ->
                renderer (palettize m) <|
                    -- TODO: Refactor this commonly-used icon button with label into a component.
                    Widget.iconButton
                        (SH.materialStyle (palettize m)).button
                        { icon =
                            Element.row [ Element.spacing spacer.px8 ]
                                [ Element.text "Remove Project"
                                , sizedFeatherIcon 18 logOut
                                ]
                        , text = "Remove Project"
                        , onPress = onPress
                        }
          , { note = notes }
          )
        , ( "go to button", \m -> renderer (palettize m) <| goToButton (palettize m) Nothing, { note = notes } )
        ]
            ++ List.map
                (\( enabled, text ) ->
                    ( "clickable icon: " ++ text
                    , \m ->
                        let
                            palette =
                                palettize m
                        in
                        renderer palette <|
                            clickableIcon []
                                { icon = edit2
                                , accessibilityLabel = "edit: " ++ text
                                , onClick =
                                    if enabled then
                                        onPress

                                    else
                                        Nothing
                                , color = palette.neutral.icon |> SH.toElementColor
                                , hoverColor = palette.neutral.text.default |> SH.toElementColor
                                }
                    , { note = notes }
                    )
                )
                [ ( True, "enabled" ), ( False, "disabled" ) ]
            ++ List.map
                (\( placement, text ) ->
                    ( "nav button: " ++ text
                    , \m ->
                        let
                            palette =
                                palettize m
                        in
                        renderer palette <|
                            navIconButton palette
                                [ Font.color (toElementColor palette.menu.textOrIcon)
                                , Element.Background.color <| toElementColor palette.menu.background
                                ]
                                { icon = Icon.HelpCircle
                                , iconPlacement = placement
                                , label = text
                                , onClick = Nothing
                                }
                    , { note = notes }
                    )
                )
                [ ( Before, "icon before label" ), ( After, "icon after label" ) ]
