module View.Forms exposing
    ( Resource(..)
    , resourceNameAlreadyExists
    )

import Element exposing (Element)
import Helpers.Validation exposing (resourceNameExistsMessage, resourceNameSuggestions, serverNameExists, sshKeyNameExists, volumeNameExists)
import Style.Helpers exposing (spacer)
import Style.Types exposing (ExoPalette)
import Style.Widgets.Button as Button
import Style.Widgets.Validation exposing (warningAlreadyExists)
import Time
import Types.HelperTypes
import Types.Project exposing (Project)



--- model


type Resource
    = Compute String
    | Volume String
    | Keypair String



--- components


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

        suggestedNames =
            resourceNameSuggestions currentTime project name
                |> List.filter (\n -> not (checkNameExists n))
    in
    warningAlreadyExists context
        { alreadyExists = nameExists
        , message = resourceNameExistsMessage localizedResourceType context.localization.unitOfTenancy
        , suggestions = suggestedNames
        , onSuggestionPressed = onSuggestionPressed
        }
