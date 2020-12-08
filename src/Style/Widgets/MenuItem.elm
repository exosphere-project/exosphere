module Style.Widgets.MenuItem exposing (MenuItemState(..), menuItem)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Style.Types
import View.Helpers as VH


type MenuItemState
    = Active
    | Inactive


{-| Generate an Input.button element suitable use as a menu item

    menuItem Active "Add Provider" Nothing

-}
menuItem : Style.Types.ExoPalette -> MenuItemState -> String -> Maybe msg -> Element msg
menuItem palette state itemLabel onPress =
    let
        ( fontWeight, fontColor, backgroundColor ) =
            case state of
                Active ->
                    ( Font.bold
                    , palette.menu.on.surface
                    , palette.menu.surface
                    )

                Inactive ->
                    ( Font.regular
                    , palette.menu.on.background
                    , palette.menu.background
                    )

        menuItemButtonAttrs =
            [ Element.width Element.fill
            , Border.color (VH.toElementColor palette.on.background)
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            , Element.spacing 15
            , Element.paddingXY 15 24
            , Background.color (VH.toElementColor backgroundColor)
            , Font.color (VH.toElementColor fontColor)
            ]

        menuItemElementAttrs =
            let
                borderProps =
                    case state of
                        Active ->
                            [ Border.color (VH.toElementColor palette.primary)
                            , Border.widthEach { bottom = 0, left = 3, right = 0, top = 0 }
                            ]

                        Inactive ->
                            []

                otherProps =
                    [ Element.width Element.fill ]
            in
            List.concat [ borderProps, otherProps ]

        label =
            Element.column
                []
                [ Element.row
                    []
                    [ Element.paragraph
                        [ Font.size 15
                        , fontWeight
                        ]
                        [ Element.text itemLabel ]
                    ]
                ]
    in
    Element.el
        menuItemElementAttrs
        (Input.button
            menuItemButtonAttrs
            { onPress = onPress
            , label = label
            }
        )
