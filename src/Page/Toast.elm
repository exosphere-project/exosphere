module Page.Toast exposing (view)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Style.Helpers as SH
import Style.Types as ST
import Types.Error exposing (ErrorLevel(..), Toast)
import Types.SharedModel exposing (SharedModel)
import View.Types



-- No state or Msgs to keep track of, so there is no Model, Msg, init, or update here


view : View.Types.Context -> SharedModel -> Toast -> Html msg
view context sharedModel t =
    let
        ( stateColor, title ) =
            case t.context.level of
                ErrorDebug ->
                    ( context.palette.success, "Debug Message" )

                ErrorInfo ->
                    ( context.palette.info, "Info" )

                ErrorWarn ->
                    ( context.palette.warning, "Warning" )

                ErrorCrit ->
                    ( context.palette.danger, "Error" )

        toastElement =
            genericToast
                stateColor
                title
                t.context.actionContext
                t.error
                t.context.recoveryHint

        show =
            case t.context.level of
                ErrorDebug ->
                    sharedModel.showDebugMsgs

                _ ->
                    True

        layoutWith =
            Element.layoutWith { options = [ Element.noStaticStyleSheet ] } []
    in
    if show then
        layoutWith toastElement

    else
        layoutWith Element.none


genericToast : ST.UIStateColors -> String -> String -> String -> Maybe String -> Element.Element msg
genericToast stateColor title actionContext error maybeRecoveryHint =
    Element.column
        [ Element.pointer
        , Element.padding 10
        , Element.spacing 10
        , Background.color (SH.toElementColor stateColor.background)
        , Font.color (SH.toElementColor stateColor.textOnColoredBG)
        , Font.size 14
        , Border.width 1
        , Border.color (SH.toElementColor stateColor.border)
        , Border.rounded 4
        , Border.shadow SH.shadowDefaults
        ]
        [ Element.el
            [ Region.heading 1
            , Font.semiBold
            , Font.size 14
            ]
            (Element.text title)
        , Element.column
            [ Font.size 13
            , Element.spacing 8
            ]
            [ Element.paragraph []
                [ Element.text "While trying to "
                , Element.text actionContext
                , Element.text ", this happened:"
                ]
            , Element.paragraph []
                [ Element.text error ]
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
