module Style.Widgets.Select exposing (Label, Value, select)

import Color
import Element
import Element.Background as Background
import Element.Border as Border
import FeatherIcons as Icons
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Spacer exposing (spacer)


select :
    List (Element.Attribute msg)
    -> ExoPalette
    ->
        { onChange : Maybe Value -> msg
        , options : List ( Value, Label )
        , selected : Maybe Value
        , label : String
        }
    -> Element.Element msg
select attributes palette { onChange, options, selected, label } =
    let
        fontColor =
            Color.toCssString palette.neutral.text.default

        backgroundColor =
            Color.toCssString palette.neutral.background.frontLayer

        select_ : Html msg
        select_ =
            Html.select
                [ HtmlE.onInput
                    (\input ->
                        if String.isEmpty input then
                            onChange Nothing

                        else
                            onChange (Just input)
                    )
                , HtmlA.style "appearance" "none"
                , HtmlA.style "-webkit-appearance" "none"
                , HtmlA.style "-moz-appearance" "none"
                , HtmlA.style "padding" "5px"
                , HtmlA.style "border-width" "0"
                , HtmlA.style "height" "48px"
                , HtmlA.style "font-size" "18px"
                , HtmlA.style "color" fontColor
                , HtmlA.style "background-color" backgroundColor
                ]
                (Html.option
                    [ HtmlA.value ""
                    , HtmlA.style "background-color" backgroundColor
                    ]
                    [ Html.text label ]
                    :: List.map (option selected) options
                )

        option : Maybe Value -> ( Value, Label ) -> Html msg
        option maybeSelectedVal item =
            Html.option
                ([ HtmlA.value (Tuple.first item)
                 , HtmlA.style "background-color" backgroundColor
                 ]
                    ++ (case maybeSelectedVal of
                            Nothing ->
                                []

                            Just selectedVal ->
                                [ HtmlA.selected (selectedVal == Tuple.first item) ]
                       )
                )
                [ Html.text (Tuple.second item) ]
    in
    Element.row
        ([ Border.width 1
         , Border.rounded 4
         , Border.color <| SH.toElementColor palette.neutral.border
         , Element.paddingXY spacer.px4 0
         , Element.width Element.fill
         , Background.color <| SH.toElementColor palette.neutral.background.frontLayer
         ]
            ++ attributes
        )
        [ Element.el [ Element.width Element.fill ] <| Element.html select_
        , featherIcon [] Icons.chevronDown
        ]


type alias Value =
    String


type alias Label =
    String
