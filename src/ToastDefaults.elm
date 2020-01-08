module ToastDefaults exposing (Toast)

import Error.Error exposing (ErrorContext, ErrorLevel)
import Toasty.Defaults


type alias Toast =
    { level : ErrorLevel
    , context : ErrorContext
    }


{-| Default theme view handling the three toast variants.
-}
view : Toast -> Html msg
view toast =
    case toast of
        Success title message ->
            genericToast "toasty-success" title message

        Warning title message ->
            genericToast "toasty-warning" title message

        Error title message ->
            genericToast "toasty-error" title message


genericToast : String -> String -> String -> Html msg
genericToast variantClass title message =
    div
        [ class "toasty-container", class variantClass ]
        [ h1 [ class "toasty-title" ] [ text title ]
        , if String.isEmpty message then
            text ""

          else
            p [ class "toasty-message" ] [ text message ]
        ]
