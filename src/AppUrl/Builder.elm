module AppUrl.Builder exposing (viewStateToUrl)

import Types.Types
    exposing
        ( JetstreamProvider(..)
        , Model
        , NonProjectViewConstructor(..)
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import Url.Builder as UB


viewStateToUrl : ViewState -> String
viewStateToUrl viewState =
    case viewState of
        NonProjectView nonProjectViewConstructor ->
            case nonProjectViewConstructor of
                LoginPicker ->
                    UB.absolute
                        [ "login" ]
                        []

                LoginOpenstack openstackLogin ->
                    UB.absolute
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
                    UB.absolute
                        [ "login"
                        , "jetstream"
                        ]
                        [ UB.string "provider" jsProvider
                        , UB.string "pname" jsLogin.jetstreamProjectName
                        , UB.string "taccuname" jsLogin.taccUsername

                        -- Not encoding password!
                        ]

                SelectProjects keystoneUrl _ ->
                    UB.absolute
                        [ "selectprojs"
                        ]
                        [ UB.string "keystoneurl" keystoneUrl
                        ]

                MessageLog ->
                    UB.absolute
                        [ "msglog" ]
                        []

                HelpAbout ->
                    UB.absolute
                        [ "helpabout" ]
                        []

        ProjectView projectIdentifier _ projectViewConstructor ->
            let
                projectIdentifierUrl =
                    UB.absolute
                        [ "projects"
                        , projectIdentifier
                        ]
                        []

                projectSpecificUrlPart_ =
                    projectSpecificUrlPart projectViewConstructor
            in
            projectIdentifierUrl ++ projectSpecificUrlPart_


projectSpecificUrlPart : ProjectViewConstructor -> String
projectSpecificUrlPart viewConstructor =
    case viewConstructor of
        ListImages _ _ ->
            UB.absolute
                [ "images" ]
                []

        ListProjectServers _ ->
            UB.absolute
                [ "servers" ]
                []

        ListProjectVolumes _ ->
            UB.absolute
                [ "volumes" ]
                []

        ListQuotaUsage ->
            UB.absolute
                [ "quotausage" ]
                []

        ServerDetail serverUuid _ ->
            UB.absolute
                [ "servers"
                , serverUuid
                ]
                []

        CreateServerImage serverUuid imageName ->
            UB.absolute
                [ "servers"
                , serverUuid
                , "image"
                ]
                [ UB.string "name" imageName
                ]

        VolumeDetail volumeUuid _ ->
            UB.absolute
                [ "volumes"
                , volumeUuid
                ]
                []

        CreateServer viewParams_ ->
            UB.absolute
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
            UB.absolute
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
            UB.absolute
                [ "attachvol"
                ]
                (List.concat [ volUuidQP, serverUuidQP ])

        MountVolInstructions attachment ->
            UB.absolute
                [ "attachvolinstructions" ]
                [ UB.string "serveruuid" attachment.serverUuid
                , UB.string "attachmentuuid" attachment.attachmentUuid
                , UB.string "device" attachment.device
                ]
