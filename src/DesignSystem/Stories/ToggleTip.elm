module DesignSystem.Stories.ToggleTip exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, palettize)
import Element
import Element.Font as Font
import Set
import Style.Types
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.ToggleTip exposing (toggleTip, warningToggleTip)
import UIExplorer exposing (storiesOf)


notes : String
notes =
    """
## Usage

Shows an icon button that toggles a popover area when clicked.

The content of the popover could be a hint, or something interactive like a set of suggested actions.

Powered by the [Popover](/#Organisms/Popover/position:%20TopLeft) widget.
"""


stories :
    Renderer msg
    -> (PopoverId -> msg)
    ->
        UIExplorer.UI
            { model
                | deployerColors : Style.Types.DeployerColorThemes
                , popover : { showPopovers : Set.Set PopoverId }
            }
            msg
            Plugins
stories renderer tagger =
    storiesOf
        "Toggletip"
        (List.map
            (\tip ->
                ( tip
                , \m ->
                    let
                        renderToggleTip =
                            case tip of
                                "warning" ->
                                    warningToggleTip

                                _ ->
                                    toggleTip
                    in
                    renderer (palettize m) <|
                        Element.el [ Element.paddingXY spacer.px8 0 ] <|
                            renderToggleTip
                                { palette = palettize m, showPopovers = m.customModel.popover.showPopovers }
                                tagger
                                "experimentalFeaturesToggleTip"
                                (Element.paragraph
                                    [ Element.width (Element.fill |> Element.minimum 300)
                                    , Element.spacing spacer.px8
                                    , Font.regular
                                    ]
                                    [ Element.text "New features in development." ]
                                )
                                Style.Types.PositionRight
                , { note = notes }
                )
            )
            [ "info", "warning" ]
        )
