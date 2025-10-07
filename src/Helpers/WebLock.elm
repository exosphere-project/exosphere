module Helpers.WebLock exposing (WebLock(..), resourceIdToWebLock, webLockToResourceId)

import Helpers.String exposing (removeEmptiness)
import Types.HelperTypes exposing (ProjectIdentifier)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..))


type WebLock
    = ProjectMsg ProjectIdentifier ProjectSpecificMsgConstructor


webLockToResourceId : WebLock -> String
webLockToResourceId webLock =
    case webLock of
        ProjectMsg { projectUuid, regionId } EnsureDefaultSecurityGroup ->
            -- "projectId::<project-uuid>::<region-id>::ensureDefaultSecurityGroup"
            [ Just "projectId", Just projectUuid, regionId, Just "ensureDefaultSecurityGroup" ] |> List.map (Maybe.withDefault "") |> String.join "::"

        _ ->
            "unknown"


resourceIdToWebLock : String -> Maybe WebLock
resourceIdToWebLock resourceId =
    case String.split "::" resourceId of
        r :: rs ->
            case r of
                "projectId" ->
                    case rs of
                        [ projectUuid, regionIdOrBlank, message ] ->
                            case message of
                                "ensureDefaultSecurityGroup" ->
                                    let
                                        regionId =
                                            removeEmptiness <| Just regionIdOrBlank
                                    in
                                    Just <| ProjectMsg { projectUuid = projectUuid, regionId = regionId } EnsureDefaultSecurityGroup

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing
