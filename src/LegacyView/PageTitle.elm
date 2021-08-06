module LegacyView.PageTitle exposing (pageTitle)

import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Helpers.Url as UrlHelpers
import OpenStack.Types as OSTypes
import Types.OuterModel exposing (OuterModel)
import Types.Project exposing (Project)
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import View.Helpers as VH
import View.Types


pageTitle : OuterModel -> View.Types.Context -> String
pageTitle outerModel context =
    case outerModel.viewState of
        NonProjectView nonProjectViewConstructor ->
            case nonProjectViewConstructor of
                LoginPicker ->
                    "Log in"

                Login loginView ->
                    case loginView of
                        LoginOpenstack _ ->
                            "OpenStack Login"

                        LoginJetstream _ ->
                            "Jetstream Cloud Login"

                LoadingUnscopedProjects _ ->
                    String.join " "
                        [ "Loading"
                        , Helpers.String.pluralize context.localization.unitOfTenancy
                        ]

                SelectProjects keystoneUrl _ ->
                    let
                        providerTitle =
                            keystoneUrl
                                |> UrlHelpers.hostnameFromUrl
                                |> VH.titleFromHostname
                    in
                    String.join " "
                        [ "Select"
                        , context.localization.unitOfTenancy
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , providerTitle
                        ]

                MessageLog _ ->
                    "Message Log"

                Settings ->
                    "Settings"

                GetSupport _ _ _ ->
                    "Get Support"

                HelpAbout ->
                    "About " ++ outerModel.sharedModel.style.appTitle

                ExamplePage _ ->
                    "Example Page"

                PageNotFound ->
                    "Error: page not found"

        ProjectView projectIdentifier _ projectViewConstructor ->
            let
                maybeProject =
                    GetterSetters.projectLookup outerModel.sharedModel projectIdentifier

                projectName =
                    maybeProject
                        |> Maybe.map (\p -> p.auth.project.name)
                        |> Maybe.withDefault
                            (String.join " "
                                [ "could not find"
                                , context.localization.unitOfTenancy
                                , "name"
                                ]
                            )
            in
            case projectViewConstructor of
                AllResources _ ->
                    String.join " "
                        [ "All resources for"
                        , projectName
                        ]

                ListImages _ _ ->
                    String.join " "
                        [ context.localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                ListProjectServers _ ->
                    String.join " "
                        [ context.localization.virtualComputer
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                ListProjectVolumes _ ->
                    String.join " "
                        [ context.localization.blockDevice
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                ListFloatingIps _ ->
                    String.join " "
                        [ context.localization.floatingIpAddress
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                AssignFloatingIp _ ->
                    String.join " "
                        [ "Assign"
                        , context.localization.floatingIpAddress
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                ListKeypairs _ ->
                    String.join " "
                        [ context.localization.pkiPublicKeyForSsh
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                CreateKeypair _ _ ->
                    String.join " "
                        [ "Upload"
                        , context.localization.pkiPublicKeyForSsh
                        , "for"
                        , projectName
                        ]

                ServerDetail serverUuid _ ->
                    String.join " "
                        [ context.localization.virtualComputer
                            |> Helpers.String.toTitleCase
                        , serverName maybeProject serverUuid
                        ]

                CreateServerImage serverUuid _ ->
                    String.join " "
                        [ "Create"
                        , context.localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.toTitleCase
                        , "for"
                        , serverName maybeProject serverUuid
                        ]

                VolumeDetail volumeUuid _ ->
                    String.join " "
                        [ context.localization.blockDevice
                            |> Helpers.String.toTitleCase
                        , volumeName maybeProject volumeUuid
                        ]

                CreateServer _ ->
                    String.join " "
                        [ "Create"
                        , context.localization.virtualComputer
                            |> Helpers.String.toTitleCase
                        ]

                CreateVolume _ _ ->
                    String.join " "
                        [ "Create"
                        , context.localization.blockDevice
                            |> Helpers.String.toTitleCase
                        ]

                AttachVolumeModal _ _ ->
                    String.join " "
                        [ "Attach"
                        , context.localization.blockDevice
                            |> Helpers.String.toTitleCase
                        ]

                MountVolInstructions _ ->
                    String.join " "
                        [ "Mount"
                        , context.localization.blockDevice
                            |> Helpers.String.toTitleCase
                        ]


serverName : Maybe Project -> OSTypes.ServerUuid -> String
serverName maybeProject serverUuid =
    maybeProject
        |> Maybe.andThen (\proj -> GetterSetters.serverLookup proj serverUuid)
        |> Maybe.map (\server -> server.osProps.name)
        |> Maybe.withDefault serverUuid


volumeName : Maybe Project -> OSTypes.VolumeUuid -> String
volumeName maybeProject volumeUuid =
    maybeProject
        |> Maybe.andThen (\proj -> GetterSetters.volumeLookup proj volumeUuid)
        |> Maybe.map (\vol -> vol.name)
        |> Maybe.withDefault volumeUuid
