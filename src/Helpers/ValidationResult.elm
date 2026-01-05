module Helpers.ValidationResult exposing (ValidationResult(..), isInvalid)


type ValidationResult comparable
    = Accepted
    | Rejected { acceptable : comparable, actual : comparable }
    | Unknown


isInvalid : ValidationResult comparable -> Bool
isInvalid result =
    case result of
        Accepted ->
            False

        Rejected _ ->
            True

        Unknown ->
            False
