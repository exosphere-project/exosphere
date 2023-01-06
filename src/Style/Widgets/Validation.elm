module Style.Widgets.Validation exposing
    ( invalidMessage
    , resourceNameSuggestions
    , serverNameExists
    , serverNameExistsMessage
    , sshKeyNameExists
    , sshKeyNameExistsMessage
    , volumeNameExists
    , volumeNameExistsMessage
    , warningMessage
    )

import DateFormat
import Element
import Element.Font as Font
import FeatherIcons
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.ServerNameValidator as OSServerNameValidator
import Regex
import RemoteData
import String exposing (isEmpty)
import Style.Helpers as SH exposing (spacer)
import Style.Types exposing (ExoPalette)
import Time
import Types.HelperTypes
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


{-| Localized warning message for when a server name already exists on a project.
-}
serverNameExistsMessage : { context | localization : Types.HelperTypes.Localization } -> String
serverNameExistsMessage context =
    resourceNameExistsMessage context.localization.virtualComputer context.localization.unitOfTenancy


{-| Localized warning message for when a volume name already exists on a project.
-}
volumeNameExistsMessage : { context | localization : Types.HelperTypes.Localization } -> String
volumeNameExistsMessage context =
    resourceNameExistsMessage context.localization.blockDevice context.localization.unitOfTenancy


{-| Localized warning message for when an SSH key name already exists on a project.
-}
sshKeyNameExistsMessage : { context | localization : Types.HelperTypes.Localization } -> String
sshKeyNameExistsMessage context =
    resourceNameExistsMessage context.localization.pkiPublicKeyForSsh context.localization.unitOfTenancy


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



--- components


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
