module Style.Widgets.MenuItem exposing (MenuItemState(..), menuItem)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Style.Helpers as SH
import Style.Types


type MenuItemState
    = Active
    | Inactive


{-| Generate an Input.button element suitable use as a menu item

    menuItem Active "Add Provider" Nothing

-}
menuItem : Style.Types.ExoPalette -> MenuItemState -> Maybe (Element msg) -> String -> Maybe msg -> Element msg
menuItem palette state icon itemLabel onPress =
    let
        ( fontWeight, backgroundColor ) =
            case state of
                Active ->
                    ( Font.bold
                    , palette.menu.surface
                    )

                Inactive ->
                    ( Font.regular
                    , palette.menu.background
                    )

        menuItemButtonAttrs =
            [ Element.width Element.fill
            , Border.color (SH.toElementColor palette.on.background)
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            , Element.spacing 15
            , Element.paddingXY 15 24
            , Background.color (SH.toElementColor backgroundColor)
            , Font.color (SH.toElementColor palette.menu.on.surface)
            ]

        menuItemElementAttrs =
            let
                borderProps =
                    case state of
                        Active ->
                            [ Border.color (SH.toElementColor palette.primary)
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
                    [ Element.spacing 8 ]
                    [ icon
                        |> Maybe.map
                            (Element.el
                                [ Font.color (SH.toElementColor palette.menu.on.background)
                                ]
                            )
                        |> Maybe.withDefault Element.none
                    , Element.paragraph
                        [ Font.size 15
                        , fontWeight
                        ]
                        [ Element.text itemLabel
                        ]
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
