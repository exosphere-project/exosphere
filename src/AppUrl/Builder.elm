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
        LoginPicker ->
            buildUrlFunc
                [ "loginpicker" ]
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

        LoadingUnscopedProjects _ ->
            buildUrlFunc
                [ "loadingprojs"
                ]
                []

        SelectProjects model ->
            buildUrlFunc
                [ "selectprojs"
                ]
                [ UB.string "keystoneurl" model.providerKeystoneUrl
                ]

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

        Settings ->
            buildUrlFunc
                [ "settings" ]
                []

        GetSupport _ ->
            buildUrlFunc
                [ "getsupport" ]
                []

        HelpAbout ->
            buildUrlFunc
                [ "helpabout" ]
                []

        PageNotFound ->
            buildUrlFunc
                [ "pagenotfound" ]
                []


projectSpecificUrlPart : (List String -> List UB.QueryParameter -> String) -> ProjectViewConstructor -> String
projectSpecificUrlPart buildUrlFunc viewConstructor =
    case viewConstructor of
        AllResourcesList _ ->
            buildUrlFunc
                [ "resources" ]
                []

        ImageList _ ->
            buildUrlFunc
                [ "images" ]
                []

        ServerList _ ->
            buildUrlFunc
                [ "servers" ]
                []

        VolumeList _ ->
            buildUrlFunc
                [ "volumes" ]
                []

        FloatingIpList _ ->
            buildUrlFunc
                [ "floatingips" ]
                []

        FloatingIpAssign _ ->
            buildUrlFunc
                [ "assignfloatingip" ]
                []

        KeypairList _ ->
            buildUrlFunc
                [ "keypairs" ]
                []

        KeypairCreate _ ->
            buildUrlFunc
                [ "uploadkeypair" ]
                []

        ServerDetail model ->
            buildUrlFunc
                [ "servers"
                , model.serverUuid
                ]
                []

        ServerCreateImage model ->
            buildUrlFunc
                [ "servers"
                , model.serverUuid
                , "image"
                ]
                [ UB.string "name" model.imageName
                ]

        VolumeDetail model ->
            buildUrlFunc
                [ "volumes"
                , model.volumeUuid
                ]
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

        VolumeCreate _ ->
            buildUrlFunc
                [ "createvolume"
                ]
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

        VolumeMountInstructions attachment ->
            buildUrlFunc
                [ "attachvolinstructions" ]
                [ UB.string "serveruuid" attachment.serverUuid
                , UB.string "attachmentuuid" attachment.attachmentUuid
                , UB.string "device" attachment.device
                ]
