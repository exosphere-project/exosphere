module Style.Widgets.Select exposing (Label, Value, select)

import Color
import Element
import Element.Border as Border
import FeatherIcons
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Style.Types exposing (ExoPalette)


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
            Color.toCssString palette.on.secondary

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
                , HtmlA.style "background-color" "transparent"
                ]
                (Html.option [ HtmlA.value "" ] [ Html.text label ]
                    :: List.map (option selected) options
                )

        option : Maybe Value -> ( Value, Label ) -> Html msg
        option maybeSelectedVal item =
            Html.option
                (HtmlA.value (Tuple.first item)
                    :: (case maybeSelectedVal of
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
         , Border.rounded 8
         , Element.paddingXY 5 0
         , Element.width Element.fill
         ]
            ++ attributes
        )
        [ Element.el [ Element.width Element.fill ] <| Element.html select_
        , FeatherIcons.chevronDown |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
        ]


type alias Value =
    String


type alias Label =
    String
