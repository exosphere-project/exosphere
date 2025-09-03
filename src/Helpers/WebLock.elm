module Helpers.WebLock exposing (WebLock(..), resourceIdToWebLock, webLockToResourceId)

import Types.HelperTypes exposing (ProjectIdentifier)


type WebLock
    = EnsureDefaultSecurityGroup ProjectIdentifier


webLockToResourceId : WebLock -> String
webLockToResourceId webLock =
    case webLock of
        EnsureDefaultSecurityGroup { projectUuid, regionId } ->
            [ Just "ensureDefaultSecurityGroup", Just projectUuid, regionId ] |> List.filterMap identity |> String.join "::"


resourceIdToWebLock : String -> Maybe WebLock
resourceIdToWebLock resourceId =
    let
        ( resource, rest ) =
            case String.split "::" resourceId of
                r :: rs ->
                    ( r, rs )

                [] ->
                    ( "", [] )
    in
    case resource of
        "ensureDefaultSecurityGroup" ->
            case rest of
                projectUuid :: regionParts ->
                    Just (EnsureDefaultSecurityGroup { projectUuid = projectUuid, regionId = List.head regionParts })

                [] ->
                    Nothing

        _ ->
            Nothing
