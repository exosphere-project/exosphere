module AppUrl.Builder exposing (viewStateToUrl)

import Types.HelperTypes exposing (JetstreamProvider(..))
import Types.View
    exposing
        ( LoginView(..)
        , NonProjectViewConstructor(..)
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import Url.Builder as UB


viewStateToUrl : Maybe String -> ViewState -> String
viewStateToUrl maybePathPrefix viewState =
    case viewState of
        NonProjectView nonProjectViewConstructor ->
            projectNonspecificUrlPart (buildPrefixedUrl maybePathPrefix) nonProjectViewConstructor

        ProjectView projectIdentifier _ projectViewConstructor ->
            let
                projectIdentifierUrl =
                    buildPrefixedUrl
                        maybePathPrefix
                        [ "projects"
                        , projectIdentifier
                        ]
                        []

                projectSpecificUrlPart_ =
                    projectSpecificUrlPart UB.absolute projectViewConstructor
            in
            projectIdentifierUrl ++ projectSpecificUrlPart_


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


projectNonspecificUrlPart : (List String -> List UB.QueryParameter -> String) -> NonProjectViewConstructor -> String
projectNonspecificUrlPart buildUrlFunc viewConstructor =
    case viewConstructor of
        GetSupport _ ->
            buildUrlFunc
                [ "getsupport" ]
                []

        HelpAbout ->
            buildUrlFunc
                [ "helpabout" ]
                []

        Login loginView ->
            case loginView of
                LoginOpenstack pageModel ->
                    buildUrlFunc
                        [ "login"
                        , "openstack"
                        ]
                        [ UB.string "authurl" pageModel.creds.authUrl
                        , UB.string "udomain" pageModel.creds.userDomain
                        , UB.string "uname" pageModel.creds.username

                        -- Not encoding password!
                        ]

                LoginJetstream jsLogin ->
                    let
                        jsProvider =
                            case jsLogin.jetstreamProviderChoice of
                                IUCloud ->
                                    "iu"

                                TACCCloud ->
                                    "tacc"

                                BothJetstreamClouds ->
                                    "both"
                    in
                    buildUrlFunc
                        [ "login"
                        , "jetstream"
                        ]
                        [ UB.string "provider" jsProvider
                        , UB.string "taccuname" jsLogin.taccUsername

                        -- Not encoding password!
                        ]

        LoginPicker ->
            buildUrlFunc
                [ "loginpicker" ]
                []

        LoadingUnscopedProjects _ ->
            buildUrlFunc
                [ "loadingprojs"
                ]
                []

        MessageLog pageModel ->
            buildUrlFunc
                [ "msglog" ]
                [ UB.string "showdebug"
                    (if pageModel.showDebugMsgs then
                        "true"

                     else
                        "false"
                    )
                ]

        PageNotFound ->
            buildUrlFunc
                [ "pagenotfound" ]
                []

        SelectProjects pageModel ->
            buildUrlFunc
                [ "selectprojs"
                ]
                [ UB.string "keystoneurl" pageModel.providerKeystoneUrl
                ]

        Settings ->
            buildUrlFunc
                [ "settings" ]
                []


projectSpecificUrlPart : (List String -> List UB.QueryParameter -> String) -> ProjectViewConstructor -> String
projectSpecificUrlPart buildUrlFunc viewConstructor =
    case viewConstructor of
        AllResourcesList _ ->
            buildUrlFunc
                [ "resources" ]
                []

        FloatingIpAssign _ ->
            buildUrlFunc
                [ "assignfloatingip" ]
                []

        FloatingIpList _ ->
            buildUrlFunc
                [ "floatingips" ]
                []

        ImageList _ ->
            buildUrlFunc
                [ "images" ]
                []

        KeypairCreate _ ->
            buildUrlFunc
                [ "uploadkeypair" ]
                []

        KeypairList _ ->
            buildUrlFunc
                [ "keypairs" ]
                []

        ServerCreate pageModel ->
            buildUrlFunc
                [ "createserver"
                ]
                [ UB.string "imageuuid" pageModel.imageUuid
                , UB.string "imagename" pageModel.imageName
                , UB.string "deployguac"
                    (pageModel.deployGuacamole
                        |> (\maybeDeployGuac ->
                                case maybeDeployGuac of
                                    Just bool ->
                                        if bool then
                                            "justtrue"

                                        else
                                            "justfalse"

                                    Nothing ->
                                        "nothing"
                           )
                    )
                ]

        ServerCreateImage pageModel ->
            buildUrlFunc
                [ "servers"
                , pageModel.serverUuid
                , "image"
                ]
                [ UB.string "name" pageModel.imageName
                ]

        ServerDetail pageModel ->
            buildUrlFunc
                [ "servers"
                , pageModel.serverUuid
                ]
                []

        ServerList _ ->
            buildUrlFunc
                [ "servers" ]
                []

        VolumeAttach pageModel ->
            let
                volUuidQP =
                    case pageModel.maybeVolumeUuid of
                        Just volUuid ->
                            [ UB.string "voluuid" volUuid ]

                        Nothing ->
                            []

                serverUuidQP =
                    case pageModel.maybeServerUuid of
                        Just serverUuid ->
                            [ UB.string "serveruuid" serverUuid ]

                        Nothing ->
                            []
            in
            buildUrlFunc
                [ "attachvol"
                ]
                (List.concat [ volUuidQP, serverUuidQP ])

        VolumeCreate _ ->
            buildUrlFunc
                [ "createvolume"
                ]
                []

        VolumeDetail pageModel ->
            buildUrlFunc
                [ "volumes"
                , pageModel.volumeUuid
                ]
                []

        VolumeList _ ->
            buildUrlFunc
                [ "volumes" ]
                []

        VolumeMountInstructions pageModel ->
            buildUrlFunc
                [ "attachvolinstructions" ]
                [ UB.string "serveruuid" pageModel.serverUuid
                , UB.string "attachmentuuid" pageModel.attachmentUuid
                , UB.string "device" pageModel.device
                ]
