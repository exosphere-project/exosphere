module View.Toast exposing (toast)

import Element
import Element.Font as Font
import Element.Region as Region
import Error exposing (ErrorLevel(..))
import Html exposing (Html)
import Html.Attributes
import Types.Types exposing (Msg, Toast)


toast : Toast -> Html Msg
toast t =
    let
        ( class, title ) =
            case t.context.level of
                ErrorDebug ->
                    ( "toasty-success", "Debug Message" )

                ErrorInfo ->
                    ( "toasty-success", "Info" )

                ErrorWarn ->
                    ( "toasty-warning", "Warning" )

                ErrorCrit ->
                    ( "toasty-error", "Error" )

        toastElement =
            genericToast
                class
                title
                t.context.actionContext
                t.error
                t.context.recoveryHint
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] toastElement


genericToast : String -> String -> String -> a -> Maybe String -> Element.Element Msg
genericToast variantClass title actionContext error maybeRecoveryHint =
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
        , Element.column
            [ Element.htmlAttribute (Html.Attributes.class "toasty-message")
            , Font.size 12
            , Element.spacing 10
            ]
            [ Element.paragraph []
                [ Element.text "While Exosphere was trying to "
                , Element.text actionContext
                , Element.text ", this happened:"
                ]
            , Element.paragraph []
                [ Element.text <| Debug.toString error ]
            , case maybeRecoveryHint of
                Just recoveryHint ->
                    Element.paragraph []
                        [ Element.text "Hint: "
                        , Element.text recoveryHint
                        ]

                Nothing ->
                    Element.none
            ]
        ]
