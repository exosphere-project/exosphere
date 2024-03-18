module DesignSystem.Stories.CopyableText exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Element.Font as Font
import Element.Input as Input
import Style.Widgets.CopyableText as CopyableText exposing (copyableScript, copyableText, copyableTextAccessory)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import UIExplorer
    exposing
        ( storiesOf
        )
import View.Helpers as VH


stories :
    Renderer msg
    -> (String -> msg)
    -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer onChange =
    storiesOf
        "Copyable Text"
        [ ( "default"
          , \m ->
                renderer (palettize m) <|
                    copyableText
                        (palettize m)
                        [ Font.family [ Font.monospace ]
                        , Element.width Element.shrink
                        ]
                        "192.168.1.1"
          , { note = CopyableText.notes }
          )
        , ( "copyable scripts"
          , \m ->
                renderer (palettize m) <|
                    copyableScript (palettize m) """
export OS_PROJECT_NAME="cloud-riders"
export OS_USERNAME="enfysnest"
export OS_IDENTITY_API_VERSION=3
export OS_USER_DOMAIN_NAME="default"
export OS_TENANT_NAME="enfysnest"
export OS_AUTH_URL="https://cell.alliance.rebel:35357/v3"
export OS_PROJECT_DOMAIN_NAME="default"
export OS_REGION_NAME="CellOne"
export OS_PASSWORD=$OS_PASSWORD_INPUT
"""
          , { note = CopyableText.notes }
          )
        , ( "separate accessory"
          , \m ->
                let
                    text =
                        """# Support Request From Exosphere

## Applicable Resource
share with UUID 632033bd-9121-49fd-a064-f1d5eedb024f

## Request Description
It doesn't mount.

## Recent Log Messages
(none)"""

                    copyable =
                        copyableTextAccessory (palettize m) text
                in
                renderer (palettize m) <|
                    Element.row [ Element.spacing spacer.px8 ]
                        [ Input.multiline
                            (VH.inputItemAttributes (palettize m)
                                ++ [ Element.height <| Element.px 200
                                   , Element.width <| Element.px 300
                                   , Element.spacing spacer.px8
                                   , Text.fontFamily Text.Mono
                                   ]
                                ++ Text.typographyAttrs Text.Tiny
                                ++ [ copyable.id ]
                            )
                            { onChange = onChange
                            , text = text
                            , placeholder = Nothing
                            , label = Input.labelHidden "Support request"
                            , spellcheck = False
                            }
                        , copyable.accessory
                        ]
          , { note = CopyableText.notes }
          )
        ]
