module OpenStack.ServerNameValidator exposing (serverNameValidator)

import Helpers.String
import Regex


serverNameValidator : Maybe String -> String -> Maybe (List String)
serverNameValidator maybeWordForServer name =
    -- If server name is valid, returns nothing.
    -- If server name is invalid, returns a list of human-readable reasons why.
    let
        wordForServer =
            maybeWordForServer |> Maybe.withDefault "server"

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
            , ( String.left 1 name == " "
              , "not start with a space"
              )
            , ( String.right 1 name == " "
              , "not end with a space"
              )
            , ( String.right 1 name == "-"
              , "not end with a hyphen"
              )
            ]

        runValidator ( failCondition, reason ) =
            if failCondition then
                Just <|
                    String.join " "
                        [ wordForServer
                            |> Helpers.String.toTitleCase
                        , "name must"
                        , reason
                        ]

            else
                Nothing

        failures =
            List.filterMap runValidator validators
    in
    if List.isEmpty failures then
        Nothing

    else
        Just failures
