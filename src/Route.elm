module Route exposing (ProjectRouteConstructor(..), Route(..), routeToUrl)

import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes
import Url.Builder as UB


type Route
    = GetSupport (Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid ))
    | HelpAbout
    | LoadingUnscopedProjects OSTypes.AuthTokenString
    | LoginJetstream (Maybe HelperTypes.JetstreamCreds)
    | LoginOpenstack (Maybe OSTypes.OpenstackLogin)
    | LoginPicker
    | MessageLog Bool
    | PageNotFound
    | ProjectRoute HelperTypes.ProjectIdentifier ProjectRouteConstructor
    | SelectProjects OSTypes.KeystoneUrl
    | Settings


type ProjectRouteConstructor
    = AllResourcesList
    | FloatingIpAssign (Maybe OSTypes.IpAddressUuid) (Maybe OSTypes.ServerUuid)
    | FloatingIpList
    | ImageList
    | KeypairCreate
    | KeypairList
    | ServerCreate OSTypes.ImageUuid String (Maybe Bool)
    | ServerCreateImage OSTypes.ServerUuid (Maybe String)
    | ServerDetail OSTypes.ServerUuid
    | ServerList
    | VolumeAttach (Maybe OSTypes.ServerUuid) (Maybe OSTypes.VolumeUuid)
    | VolumeCreate
    | VolumeDetail OSTypes.VolumeUuid
    | VolumeList
    | VolumeMountInstructions OSTypes.VolumeAttachment


routeToUrl : Maybe String -> Route -> String
routeToUrl maybePathPrefix route =
    let
        buildUrlFunc =
            buildPrefixedUrl maybePathPrefix
    in
    case route of
        GetSupport _ ->
            buildUrlFunc
                [ "getsupport" ]
                []

        HelpAbout ->
            buildUrlFunc
                [ "helpabout" ]
                []

        LoadingUnscopedProjects _ ->
            buildUrlFunc
                [ "loadingprojs"
                ]
                []

        LoginJetstream maybeJsLogin ->
            let
                jsProvider =
                    case maybeJsLogin of
                        Just jsLogin ->
                            case jsLogin.jetstreamProviderChoice of
                                HelperTypes.IUCloud ->
                                    "iu"

                                HelperTypes.TACCCloud ->
                                    "tacc"

                                HelperTypes.BothJetstreamClouds ->
                                    "both"

                        Nothing ->
                            "both"
            in
            buildUrlFunc
                [ "login"
                , "jetstream"
                ]
                [ UB.string "provider" jsProvider
                , UB.string "taccuname" (maybeJsLogin |> Maybe.map .taccUsername |> Maybe.withDefault "")

                -- Not encoding password!
                ]

        LoginOpenstack maybeOsLogin ->
            buildUrlFunc
                [ "login"
                , "openstack"
                ]
                [ UB.string "authurl" (maybeOsLogin |> Maybe.map .authUrl |> Maybe.withDefault "")
                , UB.string "udomain" (maybeOsLogin |> Maybe.map .userDomain |> Maybe.withDefault "")
                , UB.string "uname" (maybeOsLogin |> Maybe.map .username |> Maybe.withDefault "")

                -- Not encoding password!
                ]

        LoginPicker ->
            buildUrlFunc
                [ "loginpicker" ]
                []

        MessageLog showDebugMsgs ->
            buildUrlFunc
                [ "msglog" ]
                [ UB.string "showdebug"
                    (if showDebugMsgs then
                        "true"

                     else
                        "false"
                    )
                ]

        PageNotFound ->
            buildUrlFunc
                [ "pagenotfound" ]
                []

        ProjectRoute projectIdentifier projectRouteConstructor ->
            let
                projectIdentifierPath =
                    [ "projects"
                    , projectIdentifier
                    ]

                ( projectSpecificPath, projectSpecificQuery ) =
                    case projectRouteConstructor of
                        AllResourcesList ->
                            ( [ "resources" ]
                            , []
                            )

                        FloatingIpAssign _ _ ->
                            ( [ "assignfloatingip" ]
                            , []
                            )

                        FloatingIpList ->
                            ( [ "floatingips" ]
                            , []
                            )

                        ImageList ->
                            ( [ "images" ]
                            , []
                            )

                        KeypairCreate ->
                            ( [ "uploadkeypair" ]
                            , []
                            )

                        KeypairList ->
                            ( [ "keypairs" ]
                            , []
                            )

                        ServerCreate imageUuid imageName maybeDeployGuac ->
                            ( [ "createserver"
                              ]
                            , [ UB.string "imageuuid" imageUuid
                              , UB.string "imagename" imageName
                              , UB.string "deployguac"
                                    (case maybeDeployGuac of
                                        Just bool ->
                                            if bool then
                                                "justtrue"

                                            else
                                                "justfalse"

                                        Nothing ->
                                            "nothing"
                                    )
                              ]
                            )

                        ServerCreateImage serverUuid maybeImageName ->
                            ( [ "servers"
                              , serverUuid
                              , "image"
                              ]
                            , case maybeImageName of
                                Just imageName ->
                                    [ UB.string "name" imageName
                                    ]

                                Nothing ->
                                    []
                            )

                        ServerDetail serverUuid ->
                            ( [ "servers"
                              , serverUuid
                              ]
                            , []
                            )

                        ServerList ->
                            ( [ "servers" ]
                            , []
                            )

                        VolumeAttach maybeServerUuid maybeVolumeUuid ->
                            let
                                volUuidQP =
                                    case maybeVolumeUuid of
                                        Just volUuid ->
                                            [ UB.string "voluuid" volUuid ]

                                        Nothing ->
                                            []

                                serverUuidQP =
                                    case maybeServerUuid of
                                        Just serverUuid ->
                                            [ UB.string "serveruuid" serverUuid ]

                                        Nothing ->
                                            []
                            in
                            ( [ "attachvol"
                              ]
                            , List.concat [ volUuidQP, serverUuidQP ]
                            )

                        VolumeCreate ->
                            ( [ "createvolume"
                              ]
                            , []
                            )

                        VolumeDetail volumeUuid ->
                            ( [ "volumes"
                              , volumeUuid
                              ]
                            , []
                            )

                        VolumeList ->
                            ( [ "volumes" ]
                            , []
                            )

                        VolumeMountInstructions attachment ->
                            ( [ "attachvolinstructions" ]
                            , [ UB.string "serveruuid" attachment.serverUuid
                              , UB.string "attachmentuuid" attachment.attachmentUuid
                              , UB.string "device" attachment.device
                              ]
                            )
            in
            buildUrlFunc (projectIdentifierPath ++ projectSpecificPath) projectSpecificQuery

        SelectProjects keystoneUrl ->
            buildUrlFunc
                [ "selectprojs"
                ]
                [ UB.string "keystoneurl" keystoneUrl
                ]

        Settings ->
            buildUrlFunc
                [ "settings" ]
                []


buildPrefixedUrl : Maybe String -> List String -> List UB.QueryParameter -> String
buildPrefixedUrl maybePathPrefix pathParts queryParams =
    let
        prefixedPathParts =
            case maybePathPrefix of
                Just pathPrefix ->
                    pathPrefix :: pathParts

                Nothing ->
                    pathParts
    in
    UB.absolute prefixedPathParts queryParams
