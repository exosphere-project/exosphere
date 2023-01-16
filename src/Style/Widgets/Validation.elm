module Style.Widgets.Validation exposing
    ( invalidMessage
    , notes
    , warningAlreadyExists
    , warningMessage
    )

import Element exposing (Element)
import Element.Font as Font
import FeatherIcons
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Button as Button
import Style.Widgets.Spacer exposing (spacer)


notes : String
notes =
    """
## Usage

Validation widgets show errors, warnings & suggested remedies in a consistent way.

Use them in your forms together with your business logic.
"""



--- components


{-| Shows a message for a form validation error.
-}
invalidMessage : ExoPalette -> String -> Element.Element msg
invalidMessage palette helperText =
    Element.row [ Element.spacingXY spacer.px8 0 ]
        [ Element.el
            [ Font.color (palette.danger.textOnNeutralBG |> SH.toElementColor)
            ]
            (FeatherIcons.alertCircle
                |> FeatherIcons.toHtml []
                |> Element.html
            )
        , -- let text wrap if it exceeds container's width
          Element.paragraph
            [ Font.color (SH.toElementColor palette.danger.textOnNeutralBG)
            , Font.size 16
            ]
            [ Element.text helperText ]
        ]


{-| Shows a message for a non-blocking but potentially problematic form field input.
-}
warningMessage : ExoPalette -> String -> Element.Element msg
warningMessage palette helperText =
    Element.row [ Element.spacingXY spacer.px8 0 ]
        [ Element.el
            [ Font.color (palette.warning.textOnNeutralBG |> SH.toElementColor)
            ]
            (FeatherIcons.alertTriangle
                |> FeatherIcons.toHtml []
                |> Element.html
            )
        , Element.el
            [ Font.color (SH.toElementColor palette.warning.textOnNeutralBG)
            , Font.size 16
            ]
            (Element.text helperText)
        ]


{-| When creating something, if the item already exists, warn the user & show them some new input suggestions.
-}
warningAlreadyExists :
    { context | palette : ExoPalette }
    -> { alreadyExists : Bool, message : String, suggestions : List String, onSuggestionPressed : String -> msg }
    -> List (Element msg)
warningAlreadyExists context { alreadyExists, message, suggestions, onSuggestionPressed } =
    let
        renderAlreadyExists =
            if alreadyExists then
                [ warningMessage context.palette message ]

            else
                []

        suggestionButtons =
            let
                buttons =
                    suggestions
                        |> List.map
                            (\suggestion ->
                                Button.default
                                    context.palette
                                    { text = suggestion
                                    , onPress = Just (onSuggestionPressed suggestion)
                                    }
                            )
            in
            if alreadyExists then
                [ Element.row
                    [ Element.spacing spacer.px8 ]
                    buttons
                ]

            else
                [ Element.none ]
    in
    renderAlreadyExists
        ++ suggestionButtons
