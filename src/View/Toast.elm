module View.Toast exposing (toast)

import Element
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Style.Helpers as SH
import Types.Error exposing (ErrorLevel(..))
import Types.Types exposing (Msg, Toast)
import View.Types


toast : View.Types.Context -> Bool -> Toast -> Html Msg
toast context showDebugMsgs t =
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
                context
                class
                title
                t.context.actionContext
                t.error
                t.context.recoveryHint

        show =
            case t.context.level of
                ErrorDebug ->
                    showDebugMsgs

                _ ->
                    True

        layoutWith =
            Element.layoutWith { options = [ Element.noStaticStyleSheet ] } []
    in
    if show then
        layoutWith toastElement

    else
        layoutWith Element.none


genericToast : View.Types.Context -> String -> String -> String -> a -> Maybe String -> Element.Element Msg
genericToast context variantClass title actionContext error maybeRecoveryHint =
    Element.column
        [ Element.htmlAttribute (Html.Attributes.class "toasty-container")
        , Element.htmlAttribute (Html.Attributes.class variantClass)
        , Element.padding 10
        , Element.spacing 10
        , Font.color (SH.toElementColor context.palette.on.error)
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
                [ Element.text "While trying to "
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
