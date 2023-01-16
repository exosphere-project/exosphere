module Style.Widgets.Toast exposing (ToastState, config, initialModel, notes, showToast, update, view)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Helpers.Helpers exposing (hiddenActionContexts)
import Html exposing (Html)
import Html.Attributes
import Route exposing (Route(..))
import Style.Helpers as SH exposing (spacer)
import Style.Types as ST
import Style.Widgets.Icon as Icon
import Style.Widgets.Link as Link
import Style.Widgets.Text as Text
import Toasty
import Toasty.Defaults
import Types.Error exposing (ErrorLevel(..), Toast)


notes : String
notes =
    """
## Usage

Exosphere uses [Toasty](https://package.elm-lang.org/packages/pablen/toasty/latest/Toasty) to display toast messages.

Toasts are shown to inform the user of an error. A toast's style is determined by error level.

A toast is dismissed:

- When it is clicked, or
- After its configured delay is elapsed.

Toasts accumulate under each other in a list. New toasts with the same underlying error are ignored.
"""


{-| Toasts need to be accumulated in a stack & have their own identifiers.
-}
type alias ToastState model =
    { model | toasties : Toasty.Stack Toast }


initialModel : Toasty.Stack a
initialModel =
    Toasty.initialState


config : Toasty.Config msg
config =
    let
        containerAttrs : List (Html.Attribute msg)
        containerAttrs =
            -- copied from Toasty.Defaults.containerAttrs (because it isn't exposed)
            -- with "position" changed from "fixed" to "absolute"
            -- and "top" changed from 0 to -1em
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "-1em"
            , Html.Attributes.style "right" "0"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "max-width" "300px"
            , Html.Attributes.style "list-style-type" "none"
            , Html.Attributes.style "padding" "0"
            , Html.Attributes.style "margin" "0"
            ]

        itemAttrs =
            -- copied from Toasty.Defaults.itemAttrs (because it isn't exposed)
            -- with margin-right decreased from 1em to 0.5em
            -- and "max-height" increased from 100px to 500 px
            -- (and its transition duration decreased to 0.3s)
            -- since content overflows in 100 px
            [ Html.Attributes.style "margin" "1em 0.5em 0 1em"
            , Html.Attributes.style "max-height" "500px"
            , Html.Attributes.style "transition" "max-height 0.3s, margin-top 0.6s"
            ]
    in
    -- Toasty.Defaults.config uses classes defined in assets/css/toasty.css
    Toasty.Defaults.config
        |> Toasty.delay 60000
        |> Toasty.containerAttrs containerAttrs
        |> Toasty.itemAttrs itemAttrs


view :
    { context | palette : ST.ExoPalette, urlPathPrefix : Maybe String }
    -> { sharedModel | showDebugMsgs : Bool }
    -> Toast
    -> Html msg
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
            if List.member t.context.actionContext hiddenActionContexts then
                minimalToast
                    context
                    stateColor
                    { title = title
                    , error = t.error
                    , showDebugMsgs = sharedModel.showDebugMsgs
                    }

            else
                genericToast
                    context
                    stateColor
                    { title = title
                    , maybeActionContext = Just t.context.actionContext
                    , error = t.error
                    , maybeRecoveryHint = t.context.recoveryHint
                    , showDebugMsgs = sharedModel.showDebugMsgs
                    }

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


minimalToast :
    { context | palette : ST.ExoPalette, urlPathPrefix : Maybe String }
    -> ST.UIStateColors
    ->
        { title : String
        , error : String
        , showDebugMsgs : Bool
        }
    -> Element.Element msg
minimalToast context stateColor { title, error, showDebugMsgs } =
    genericToast context stateColor { title = title, maybeActionContext = Nothing, error = error, maybeRecoveryHint = Nothing, showDebugMsgs = showDebugMsgs }


genericToast :
    { context | palette : ST.ExoPalette, urlPathPrefix : Maybe String }
    -> ST.UIStateColors
    ->
        { title : String
        , maybeActionContext : Maybe String
        , error : String
        , maybeRecoveryHint : Maybe String
        , showDebugMsgs : Bool
        }
    -> Element.Element msg
genericToast context stateColor { title, maybeActionContext, error, maybeRecoveryHint, showDebugMsgs } =
    let
        description =
            case maybeActionContext of
                Just actionContext ->
                    Element.paragraph []
                        [ Element.text "While trying to "
                        , Element.text actionContext
                        , Element.text ", this happened:"
                        ]

                Nothing ->
                    Element.none

        message =
            Element.paragraph []
                [ Element.text error ]

        hint =
            case maybeRecoveryHint of
                Just recoveryHint ->
                    Element.paragraph []
                        [ Element.text "Hint: "
                        , Element.text recoveryHint
                        ]

                Nothing ->
                    Element.none

        readMore =
            Element.paragraph []
                [ Element.link
                    (Link.linkStyle context.palette
                        ++ [ Element.alignRight ]
                    )
                    { url = Route.toUrl context.urlPathPrefix (MessageLog showDebugMsgs)
                    , label = Text.text Text.Small [] "Read more"
                    }
                ]
    in
    Element.column
        [ Element.pointer
        , Element.padding spacer.px12
        , Element.spacing spacer.px12
        , Element.width Element.fill
        , Background.color (SH.toElementColor stateColor.background)
        , Font.color (SH.toElementColor stateColor.textOnColoredBG)
        , Text.fontSize Text.Small
        , Border.width 1
        , Border.color (SH.toElementColor stateColor.border)
        , Border.rounded 4
        , Border.shadow SH.shadowDefaults
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.el
                [ Region.heading 1
                , Font.semiBold
                , Text.fontSize Text.Small
                ]
                (Element.text title)
            , Input.button
                [ Element.mouseOver
                    [ Border.color <| SH.toElementColor context.palette.neutral.icon
                    ]
                , Element.alignRight
                ]
                { onPress = Nothing
                , label = Icon.timesCircle (SH.toElementColor context.palette.neutral.icon) 12
                }
            ]
        , Element.column
            [ Text.fontSize Text.Small
            , Element.spacing spacer.px8
            , Element.width Element.fill
            ]
            [ description
            , message
            , hint
            , readMore
            ]
        ]


deduplicate : Toast -> List Toast -> Bool
deduplicate toast =
    not << List.any (\t -> t.error == toast.error)


showToast :
    Toast
    -> (Toasty.Msg Toast -> msg)
    -> ( ToastState model, Cmd msg )
    -> ( ToastState model, Cmd msg )
showToast toast tagger ( model, cmd ) =
    Toasty.addToastIf config tagger (deduplicate toast) toast ( model, cmd )


update : (Toasty.Msg Toast -> msg) -> Toasty.Msg Toast -> ToastState model -> ( ToastState model, Cmd msg )
update tagger msg model =
    Toasty.update config tagger msg model
