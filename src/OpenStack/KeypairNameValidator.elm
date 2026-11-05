module OpenStack.KeypairNameValidator exposing (keypairNameValidator)

import List.Extra



{- Nova's API schema (`keypair_name_special_chars` in nova/api/validation/parameter_types.py) accepts
   keypair names matching `^[_\- a-zA-Z0-9]+$` with a length of 1 to 255. Microversion 2.92 also allows `@` and `.`,
   but Exosphere creates keypairs without requesting a microversion, so the stricter set applies.
-}


maxLength : Int
maxLength =
    255


isAllowedChar : Char -> Bool
isAllowedChar char =
    -- Elm's Char.isAlphaNum only matches ASCII letters & digits, as Nova's pattern does.
    Char.isAlphaNum char || List.member char [ '_', '-', ' ' ]


{-| Human-readable reasons why Nova would reject this keypair name, or an empty list if it is acceptable.

Validates the name as submitted, i.e. after trimming surrounding whitespace.

-}
keypairNameValidator : String -> List String
keypairNameValidator name =
    let
        trimmedName =
            String.trim name
    in
    if String.isEmpty trimmedName then
        [ "Name is required" ]

    else
        let
            disallowedChars =
                trimmedName
                    |> String.toList
                    |> List.filter (not << isAllowedChar)
                    |> List.map describeChar
                    |> List.Extra.unique
        in
        List.filterMap identity
            [ if String.length trimmedName > maxLength then
                Just ("Name must be " ++ String.fromInt maxLength ++ " characters or fewer")

              else
                Nothing
            , if List.isEmpty disallowedChars then
                Nothing

              else
                Just ("Name cannot contain " ++ String.join ", " disallowedChars)
            ]


describeChar : Char -> String
describeChar char =
    case char of
        '\t' ->
            "tabs"

        _ ->
            -- Quoted, a pasted non-breaking or other Unicode space would look like an allowed space.
            if String.isEmpty (String.trim (String.fromChar char)) then
                "non-standard spaces"

            else
                "\"" ++ String.fromChar char ++ "\""
