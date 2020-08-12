module Style.Widgets.MenuItem exposing (MenuItemState(..), menuItem)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


type MenuItemState
    = Active
    | Inactive


{-| Generate an Input.button element suitable use as a menu item

    menuItem Active "Add Provider" Nothing

-}
menuItem : MenuItemState -> String -> Maybe msg -> Element msg
menuItem state itemLabel onPress =
    let
        ( fontWeight, fontColor, backgroundColor ) =
            case state of
                Active ->
                    ( Font.bold
                    , Element.rgb255 255 255 255
                    , Element.rgb255 51 51 51
                    )

                Inactive ->
                    ( Font.regular
                    , Element.rgb255 181 181 181
                    , Element.rgb255 36 36 36
                    )

        menuItemButtonAttrs =
            [ Element.width Element.fill
            , Border.color (Element.rgb255 10 10 10)
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            , Element.spacing 15
            , Element.paddingXY 15 24
            , Background.color backgroundColor
            , Font.color fontColor
            ]

        menuItemElementAttrs =
            let
                borderProps =
                    case state of
                        Active ->
                            -- Turquoise??
                            [ Border.color (Element.rgb255 0 209 178)
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
