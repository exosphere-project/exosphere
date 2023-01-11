module Helpers.Validation exposing
    ( resourceNameExistsMessage
    , resourceNameSuggestions
    , serverNameExists
    , sshKeyNameExists
    , volumeNameExists
    )

import DateFormat
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.ServerNameValidator as OSServerNameValidator
import Regex
import RemoteData
import String exposing (isEmpty)
import Time
import Types.Project exposing (Project)



--- helpers


{-| Does this server name already exist on the project?
-}
serverNameExists : Project -> String -> Bool
serverNameExists project serverName =
    case project.servers.data of
        RDPP.DoHave servers _ ->
            servers
                |> List.map .osProps
                |> List.map .name
                |> List.member serverName

        _ ->
            False


{-| Does this volume name already exist on the project?
-}
volumeNameExists : Project -> String -> Bool
volumeNameExists project volumeName_ =
    let
        name =
            String.trim volumeName_
    in
    if isEmpty name then
        False

    else
        RemoteData.withDefault [] project.volumes
            |> List.map .name
            |> List.member (Just name)


{-| Does this SSH public key name already exist on the project?
-}
sshKeyNameExists : Project -> String -> Bool
sshKeyNameExists project sshKeyName =
    let
        name =
            String.trim sshKeyName
    in
    RemoteData.withDefault [] project.keypairs
        |> List.map .name
        |> List.member name


{-| A warning message that this resource type already exists.
-}
resourceNameExistsMessage : String -> String -> String
resourceNameExistsMessage resourceName_ unitOfTenancy =
    "This " ++ resourceName_ ++ " name already exists for this " ++ unitOfTenancy ++ ". You can select any of our name suggestions or modify the current name to avoid duplication."


{-| Create a list of resource name suggestions based on a current resource name, project username & time.
-}
resourceNameSuggestions : Time.Posix -> Project -> String -> List String
resourceNameSuggestions currentTime project name =
    let
        currentDate =
            DateFormat.format
                [ DateFormat.yearNumber
                , DateFormat.text "-"
                , DateFormat.monthFixed
                , DateFormat.text "-"
                , DateFormat.dayOfMonthFixed
                ]
                Time.utc
                currentTime

        username =
            Maybe.withDefault "" <| List.head <| Regex.splitAtMost 1 OSServerNameValidator.badChars project.auth.user.name

        suggestedNameWithUsername =
            if not (String.contains username name) then
                [ name
                    ++ " "
                    ++ username
                ]

            else
                []

        suggestedNameWithDate =
            if not (String.contains currentDate name) then
                [ name
                    ++ " "
                    ++ currentDate
                ]

            else
                []

        suggestedNameWithUsernameAndDate =
            if not (String.contains username name) && not (String.contains currentDate name) then
                [ name
                    ++ " "
                    ++ username
                    ++ " "
                    ++ currentDate
                ]

            else
                []
    in
    suggestedNameWithUsername ++ suggestedNameWithDate ++ suggestedNameWithUsernameAndDate
