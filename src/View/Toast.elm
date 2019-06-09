module View.Toast exposing (toast)

import Element
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Toasty.Defaults
import Types.Types exposing (Msg)


toast : Toasty.Defaults.Toast -> Html Msg
toast t =
    let
        toastElement =
            case t of
                Toasty.Defaults.Success title message ->
                    genericToast "toasty-success" title message

                Toasty.Defaults.Warning title message ->
                    genericToast "toasty-warning" title message

                Toasty.Defaults.Error title message ->
                    genericToast "toasty-error" title message
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] toastElement


genericToast : String -> String -> String -> Element.Element Msg
genericToast variantClass title message =
    Element.column
        [ Element.htmlAttribute (Html.Attributes.class "toasty-container")
        , Element.htmlAttribute (Html.Attributes.class variantClass)
        , Element.padding 10
        , Element.spacing 10
        , Font.color (Element.rgb 1 1 1)
        ]
        [ Element.el
            [ Region.heading 1
            , Font.bold
            , Font.size 14
            ]
            (Element.text title)
        , if String.isEmpty message then
            Element.text ""

          else
            Element.paragraph
                [ Element.htmlAttribute (Html.Attributes.class "toasty-message")
                , Font.size 12
                ]
                [ Element.text message
                ]
        ]
