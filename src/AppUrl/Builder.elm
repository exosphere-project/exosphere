module AppUrl.Builder exposing (viewStateToUrl)

import Types.Types
    exposing
        ( JetstreamProvider(..)
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
                [ "login" ]
                []

        LoginOpenstack openstackLogin ->
            buildUrlFunc
                [ "login"
                , "openstack"
                ]
                [ UB.string "authurl" openstackLogin.authUrl
                , UB.string "pdomain" openstackLogin.projectDomain
                , UB.string "pname" openstackLogin.projectName
                , UB.string "udomain" openstackLogin.userDomain
                , UB.string "uname" openstackLogin.username

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
                , UB.string "pname" jsLogin.jetstreamProjectName
                , UB.string "taccuname" jsLogin.taccUsername

                -- Not encoding password!
                ]

        SelectProjects keystoneUrl _ ->
            buildUrlFunc
                [ "selectprojs"
                ]
                [ UB.string "keystoneurl" keystoneUrl
                ]

        MessageLog ->
            buildUrlFunc
                [ "msglog" ]
                []

        HelpAbout ->
            buildUrlFunc
                [ "helpabout" ]
                []


projectSpecificUrlPart : (List String -> List UB.QueryParameter -> String) -> ProjectViewConstructor -> String
projectSpecificUrlPart buildUrlFunc viewConstructor =
    case viewConstructor of
        ListImages _ _ ->
            buildUrlFunc
                [ "images" ]
                []

        ListProjectServers _ ->
            buildUrlFunc
                [ "servers" ]
                []

        ListProjectVolumes _ ->
            buildUrlFunc
                [ "volumes" ]
                []

        ListQuotaUsage ->
            UB.absolute
                [ "quotausage" ]
                []

        ServerDetail serverUuid _ ->
            buildUrlFunc
                [ "servers"
                , serverUuid
                ]
                []

        CreateServerImage serverUuid imageName ->
            buildUrlFunc
                [ "servers"
                , serverUuid
                , "image"
                ]
                [ UB.string "name" imageName
                ]

        VolumeDetail volumeUuid _ ->
            buildUrlFunc
                [ "volumes"
                , volumeUuid
                ]
                []

        CreateServer viewParams_ ->
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

        CreateVolume _ _ ->
            buildUrlFunc
                [ "createvolume"
                ]
                []

        AttachVolumeModal maybeServerUuid maybeVolUuid ->
            let
                volUuidQP =
                    case maybeVolUuid of
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
            buildUrlFunc
                [ "attachvol"
                ]
                (List.concat [ volUuidQP, serverUuidQP ])

        MountVolInstructions attachment ->
            buildUrlFunc
                [ "attachvolinstructions" ]
                [ UB.string "serveruuid" attachment.serverUuid
                , UB.string "attachmentuuid" attachment.attachmentUuid
                , UB.string "device" attachment.device
                ]
