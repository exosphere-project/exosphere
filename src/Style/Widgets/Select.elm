module Style.Widgets.Select exposing
    ( Label
    , Value
    , select
    , selectNoLabel
    )

import Element
import Element.Border as Border
import FeatherIcons
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE


selectNoLabel :
    List (Element.Attribute msg)
    ->
        { onChange : Maybe Value -> msg
        , options : List ( Value, Label )
        , selected : Maybe Value
        }
    -> Element.Element msg
selectNoLabel attributes { onChange, options, selected } =
    selectMaybeLabel attributes
        { onChange = onChange
        , options = options
        , selected = selected
        , label = Nothing
        }


select :
    List (Element.Attribute msg)
    ->
        { onChange : Maybe Value -> msg
        , options : List ( Value, Label )
        , selected : Maybe Value
        , label : String
        }
    -> Element.Element msg
select attributes { onChange, options, selected, label } =
    selectMaybeLabel attributes
        { onChange = onChange
        , options = options
        , selected = selected
        , label = Just label
        }


selectMaybeLabel :
    List (Element.Attribute msg)
    ->
        { onChange : Maybe Value -> msg
        , options : List ( Value, Label )
        , selected : Maybe Value
        , label : Maybe String
        }
    -> Element.Element msg
selectMaybeLabel attributes { onChange, options, selected, label } =
    let
        htmlOptions =
            case label of
                Just labelString ->
                    Html.option [ HtmlA.value "" ] [ Html.text labelString ]
                        :: List.map (option selected) options

                Nothing ->
                    List.map (option selected) options

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
                , HtmlA.style "background-color" "transparent"
                ]
                htmlOptions

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
