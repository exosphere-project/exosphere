module Style.Widgets.Validation exposing
    ( Resource(..)
    , invalidMessage
    , resourceNameAlreadyExists
    , resourceNameExistsMessage
    , resourceNameSuggestions
    , serverNameExists
    , sshKeyNameExists
    , volumeNameExists
    , warningMessage
    )

import DateFormat
import Element exposing (Element)
import Element.Font as Font
import FeatherIcons
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.ServerNameValidator as OSServerNameValidator
import Regex
import RemoteData
import String exposing (isEmpty)
import Style.Helpers as SH exposing (spacer)
import Style.Types exposing (ExoPalette)
import Style.Widgets.Button as Button
import Time
import Types.HelperTypes
import Types.Project exposing (Project)



--- model


type Resource
    = Compute String
    | Volume String
    | Keypair String



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


{-| If a resource name already exists, warn the user & show them some name suggestions.
-}
resourceNameAlreadyExists :
    { context | localization : Types.HelperTypes.Localization, palette : ExoPalette }
    -> Project
    -> Time.Posix
    -> { resource : Resource, onSuggestionPressed : String -> msg }
    -> List (Element msg)
resourceNameAlreadyExists context project currentTime { resource, onSuggestionPressed } =
    let
        ( name, checkNameExists, localizedResourceType ) =
            case resource of
                Compute n ->
                    ( n, serverNameExists project, context.localization.virtualComputer )

                Volume n ->
                    ( n, volumeNameExists project, context.localization.blockDevice )

                Keypair n ->
                    ( n, sshKeyNameExists project, context.localization.pkiPublicKeyForSsh )

        nameExists =
            checkNameExists name

        renderNameExists =
            if nameExists then
                [ warningMessage context.palette (resourceNameExistsMessage localizedResourceType context.localization.unitOfTenancy) ]

            else
                []

        nameSuggestionButtons =
            let
                suggestedNames =
                    resourceNameSuggestions currentTime project name
                        |> List.filter (\n -> not (checkNameExists n))

                suggestionButtons =
                    suggestedNames
                        |> List.map
                            (\suggestion ->
                                Button.default
                                    context.palette
                                    { text = suggestion
                                    , onPress = Just (onSuggestionPressed suggestion)
                                    }
                            )
            in
            if nameExists then
                [ Element.row
                    [ Element.spacing spacer.px8 ]
                    suggestionButtons
                ]

            else
                [ Element.none ]
    in
    renderNameExists
        ++ nameSuggestionButtons
