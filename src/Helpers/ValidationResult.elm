module Helpers.ValidationResult exposing (ValidationResult(..), isInvalid)


type ValidationResult comparable
    = Accepted { acceptable : comparable, actual : comparable }
    | Rejected { acceptable : comparable, actual : comparable }
    | Unknown


isInvalid : ValidationResult comparable -> Bool
isInvalid result =
    case result of
        Accepted _ ->
            False

        Rejected _ ->
            True

        Unknown ->
            False
