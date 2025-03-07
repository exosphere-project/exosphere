module View.Forms exposing
    ( Resource(..)
    , resourceNameAlreadyExists
    , securityGroupAffectsServersWarning
    )

import Element exposing (Element)
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (humanCount)
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Helpers.Validation exposing (resourceNameExistsMessage, resourceNameSuggestions, securityGroupNameExists, serverNameExists, shareNameExists, sshKeyNameExists, volumeNameExists)
import OpenStack.Types as OSTypes
import Style.Types exposing (ExoPalette)
import Style.Widgets.Validation exposing (warningAlreadyExists)
import Time
import Types.HelperTypes
import Types.Project exposing (Project)
import View.Types



--- model


type Resource
    = Compute String
    | Keypair String
    | SecurityGroup String
    | Share String
    | Volume String



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

                Keypair n ->
                    ( n, sshKeyNameExists project, context.localization.pkiPublicKeyForSsh )

                SecurityGroup n ->
                    ( n, securityGroupNameExists project, context.localization.securityGroup )

                Share n ->
                    ( n, shareNameExists project, context.localization.share )

                Volume n ->
                    ( n, volumeNameExists project, context.localization.blockDevice )

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


securityGroupAffectsServersWarning :
    View.Types.Context
    -> Project
    -> OSTypes.SecurityGroupUuid
    -> Maybe OSTypes.ServerUuid
    -> String
    -> Maybe String
securityGroupAffectsServersWarning context project securityGroupUuid exceptServerUuid doing =
    let
        serversAffected =
            GetterSetters.serversForSecurityGroup project securityGroupUuid
                |> .servers

        otherServersAffected =
            case exceptServerUuid of
                Just serverUuid ->
                    List.filter (\s -> s.osProps.uuid /= serverUuid) serversAffected

                Nothing ->
                    serversAffected

        numberOfServers =
            List.length otherServersAffected
    in
    if numberOfServers == 0 then
        Nothing

    else
        let
            { locale } =
                context
        in
        Just <|
            String.join " "
                ([ doing |> Helpers.String.capitalizeWord
                 , "this"
                 , context.localization.securityGroup
                 , "will affect"
                 , numberOfServers
                    |> humanCount { locale | decimals = Exact 0 }
                 ]
                    ++ (case exceptServerUuid of
                            Just _ ->
                                [ "other" ]

                            Nothing ->
                                []
                       )
                    ++ [ (context.localization.virtualComputer |> Helpers.String.pluralizeCount numberOfServers) ++ "."
                       ]
                )
