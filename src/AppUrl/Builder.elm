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
                LoginOpenstack openstackLogin ->
                    buildUrlFunc
                        [ "login"
                        , "openstack"
                        ]
                        [ UB.string "authurl" openstackLogin.creds.authUrl
                        , UB.string "udomain" openstackLogin.creds.userDomain
                        , UB.string "uname" openstackLogin.creds.username

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

        MessageLog model ->
            buildUrlFunc
                [ "msglog" ]
                [ UB.string "showdebug"
                    (if model.showDebugMsgs then
                        "true"

                     else
                        "false"
                    )
                ]

        PageNotFound ->
            buildUrlFunc
                [ "pagenotfound" ]
                []

        SelectProjects model ->
            buildUrlFunc
                [ "selectprojs"
                ]
                [ UB.string "keystoneurl" model.providerKeystoneUrl
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

        ServerCreate viewParams_ ->
            buildUrlFunc
                [ "createserver"
                ]
                [ UB.string "imageuuid" viewParams_.imageUuid
                , UB.string "imagename" viewParams_.imageName
                , UB.string "deployguac"
                    (viewParams_.deployGuacamole
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

        ServerCreateImage model ->
            buildUrlFunc
                [ "servers"
                , model.serverUuid
                , "image"
                ]
                [ UB.string "name" model.imageName
                ]

        ServerDetail model ->
            buildUrlFunc
                [ "servers"
                , model.serverUuid
                ]
                []

        ServerList _ ->
            buildUrlFunc
                [ "servers" ]
                []

        VolumeAttach model ->
            let
                volUuidQP =
                    case model.maybeVolumeUuid of
                        Just volUuid ->
                            [ UB.string "voluuid" volUuid ]

                        Nothing ->
                            []

                serverUuidQP =
                    case model.maybeServerUuid of
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

        VolumeDetail model ->
            buildUrlFunc
                [ "volumes"
                , model.volumeUuid
                ]
                []

        VolumeList _ ->
            buildUrlFunc
                [ "volumes" ]
                []

        VolumeMountInstructions attachment ->
            buildUrlFunc
                [ "attachvolinstructions" ]
                [ UB.string "serveruuid" attachment.serverUuid
                , UB.string "attachmentuuid" attachment.attachmentUuid
                , UB.string "device" attachment.device
                ]
