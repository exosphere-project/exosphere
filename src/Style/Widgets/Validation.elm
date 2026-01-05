module Style.Widgets.Validation exposing
    ( FormErrorLevel(..)
    , FormInteraction(..)
    , invalidIcon
    , invalidMessage
    , invalidText
    , notes
    , validIcon
    , validMessage
    , warningAlreadyExists
    , warningIcon
    , warningMessage
    , warningText
    )

import Color
import Element exposing (Element)
import Element.Font as Font
import FeatherIcons as Icons
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Button as Button
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text


notes : String
notes =
    """
## Usage

Validation widgets show errors, warnings & suggested remedies in a consistent way.

Use them in your forms together with your business logic.
"""


type FormInteraction
    = Pristine
    | Touched


type FormErrorLevel
    = Warning
    | Error



--- components


validIcon : ExoPalette -> Element msg
validIcon palette =
    Icons.checkCircle
        |> featherIcon [ Font.color (palette.success.textOnNeutralBG |> SH.toElementColor) ]


{-| Shows a message for a form validation reassurance.
-}
validMessage : ExoPalette -> String -> Element.Element msg
validMessage palette helperText =
    Element.row [ Element.spacingXY spacer.px8 0 ]
        [ validIcon palette
        , -- let text wrap if it exceeds container's width
          Element.paragraph
            [ Font.color (SH.toElementColor palette.success.textOnNeutralBG)
            , Text.fontSize Text.Small
            ]
            [ Element.text helperText ]
        ]


invalidIcon : ExoPalette -> Element msg
invalidIcon palette =
    Icons.alertCircle
        |> featherIcon [ Font.color (palette.danger.textOnNeutralBG |> SH.toElementColor) ]


warningIcon : ExoPalette -> Element msg
warningIcon palette =
    Icons.alertTriangle
        |> featherIcon [ Font.color (palette.warning.textOnNeutralBG |> SH.toElementColor) ]


{-| Shows an error icon & message for a form validation error.
-}
invalidMessage : ExoPalette -> String -> Element.Element msg
invalidMessage palette helperText =
    Element.row [ Element.spacingXY spacer.px8 0 ]
        [ invalidIcon palette
        , invalidText palette helperText
        ]


{-| Renders text for blocking, invalid form field input.
-}
invalidText : ExoPalette -> String -> Element.Element msg
invalidText palette =
    feedbackText palette.danger.textOnNeutralBG


{-| Renders text for a non-blocking but potentially problematic form field input.
-}
warningText : ExoPalette -> String -> Element.Element msg
warningText palette =
    feedbackText palette.warning.textOnNeutralBG


feedbackText : Color.Color -> String -> Element msg
feedbackText color helperText =
    Text.p
        [ Font.color (SH.toElementColor color)
        , Text.fontSize Text.Small
        ]
        [ Element.text helperText ]


{-| Shows a warning icon & message for a non-blocking but potentially problematic form field input.
-}
warningMessage : ExoPalette -> String -> Element.Element msg
warningMessage palette helperText =
    Element.row [ Element.spacingXY spacer.px8 0 ]
        [ warningIcon palette
        , warningText palette helperText
        ]


{-| When creating something, if the item already exists, warn the user & show them some new input suggestions.
-}
warningAlreadyExists :
    { context | palette : ExoPalette }
    -> { alreadyExists : Bool, message : String, suggestions : List String, onSuggestionPressed : String -> msg, errorLevel : FormErrorLevel }
    -> List (Element msg)
warningAlreadyExists context { alreadyExists, message, suggestions, onSuggestionPressed, errorLevel } =
    let
        renderAlreadyExists =
            if alreadyExists then
                let
                    feedbackFunction =
                        case errorLevel of
                            Warning ->
                                warningMessage

                            Error ->
                                invalidMessage
                in
                [ feedbackFunction context.palette message ]

            else
                []

        suggestionButtons =
            if alreadyExists then
                [ Element.row [ Element.spacing spacer.px8 ]
                    (suggestions
                        |> List.map
                            (\suggestion ->
                                Button.default
                                    context.palette
                                    { text = suggestion
                                    , onPress = Just (onSuggestionPressed suggestion)
                                    }
                            )
                    )
                ]

            else
                [ Element.none ]
    in
    renderAlreadyExists
        ++ suggestionButtons
