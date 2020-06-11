module OpenStack.ServerNameValidator exposing (serverNameValidator)

import Regex


serverNameValidator : String -> Maybe (List String)
serverNameValidator name =
    -- If server name is valid, returns nothing.
    -- If server name is invalid, returns a list of human-readable reasons why.
    let
        validators =
            [ ( name == ""
              , "not be empty"
              )
            , ( String.length name >= 255
              , "be less than 255 characters long"
              )
            , ( let
                    badChars =
                        Maybe.withDefault Regex.never <| Regex.fromString "[^A-Za-z0-9-_ ]"
                in
                Regex.contains badChars name
              , "only include alphanumeric characters, hyphen, underscore and space"
              )
            , ( String.right 1 name == "-"
              , "not end with a hyphen"
              )
            ]

        runValidator ( failCondition, reason ) =
            if failCondition then
                Just ("Server name must " ++ reason)

            else
                Nothing

        failures =
            List.filterMap runValidator validators
    in
    if List.isEmpty failures then
        Nothing

    else
        Just failures
