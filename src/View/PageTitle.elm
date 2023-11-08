module View.PageTitle exposing (pageTitle, serverName, shareName, volumeName)

import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Helpers.Url as UrlHelpers
import OpenStack.Types as OSTypes
import Types.OuterModel exposing (OuterModel)
import Types.Project exposing (Project)
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import View.Helpers as VH


pageTitle : OuterModel -> String
pageTitle outerModel =
    let
        { localization } =
            outerModel.sharedModel.viewContext
    in
    case outerModel.viewState of
        NonProjectView nonProjectViewConstructor ->
            case nonProjectViewConstructor of
                GetSupport _ ->
                    "Get Support"

                HelpAbout ->
                    "About " ++ outerModel.sharedModel.style.appTitle

                Home _ ->
                    "Exosphere"

                LoadingUnscopedProjects _ ->
                    String.join " "
                        [ "Loading"
                        , Helpers.String.pluralize localization.unitOfTenancy
                        ]

                Login loginView ->
                    case loginView of
                        LoginOpenstack _ ->
                            "OpenStack Login"

                        LoginOpenIdConnect pageModel ->
                            pageModel.oidcLoginButtonLabel

                LoginPicker ->
                    "Log in"

                MessageLog _ ->
                    "Message Log"

                PageNotFound ->
                    "Error: page not found"

                SelectProjectRegions _ ->
                    String.join " "
                        [ "Select"
                        , localization.openstackSharingKeystoneWithAnother
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , localization.unitOfTenancy
                            |> Helpers.String.toTitleCase
                        ]

                SelectProjects pageModel ->
                    let
                        providerTitle =
                            pageModel.providerKeystoneUrl
                                |> UrlHelpers.hostnameFromUrl
                                |> VH.titleFromHostname
                    in
                    String.join " "
                        [ "Select"
                        , localization.unitOfTenancy
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , providerTitle
                        ]

                Settings _ ->
                    "Settings"

        ProjectView projectIdentifier projectViewConstructor ->
            let
                maybeProject =
                    GetterSetters.projectLookup outerModel.sharedModel projectIdentifier

                projectName =
                    maybeProject
                        |> Maybe.map (\p -> p.auth.project.name)
                        |> Maybe.withDefault
                            (String.join " "
                                [ "could not find"
                                , localization.unitOfTenancy
                                , "name"
                                ]
                            )
            in
            case projectViewConstructor of
                ProjectOverview _ ->
                    String.join " "
                        [ projectName
                        , "overview"
                        ]

                FloatingIpAssign _ ->
                    String.join " "
                        [ "Assign"
                        , localization.floatingIpAddress
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                FloatingIpList _ ->
                    String.join " "
                        [ localization.floatingIpAddress
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                ImageList _ ->
                    String.join " "
                        [ localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                InstanceSourcePicker _ ->
                    String.join " "
                        [ localization.virtualComputer
                            |> Helpers.String.toTitleCase
                        , "sources"
                        , "for"
                        , projectName
                        ]

                FloatingIpCreate _ ->
                    String.join " "
                        [ "Create"
                        , localization.floatingIpAddress
                            |> Helpers.String.toTitleCase
                        ]

                KeypairCreate _ ->
                    String.join " "
                        [ "Upload"
                        , localization.pkiPublicKeyForSsh
                        , "for"
                        , projectName
                        ]

                KeypairList _ ->
                    String.join " "
                        [ localization.pkiPublicKeyForSsh
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                ServerCreate _ ->
                    String.join " "
                        [ "Create"
                        , localization.virtualComputer
                            |> Helpers.String.toTitleCase
                        ]

                ServerCreateImage pageModel ->
                    String.join " "
                        [ "Create"
                        , localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.toTitleCase
                        , "for"
                        , serverName maybeProject pageModel.serverUuid
                        ]

                ServerDetail pageModel ->
                    String.join " "
                        [ localization.virtualComputer
                            |> Helpers.String.toTitleCase
                        , serverName maybeProject pageModel.serverUuid
                        ]

                ServerList _ ->
                    String.join " "
                        [ localization.virtualComputer
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                ServerResize pageModel ->
                    String.join " "
                        [ "Resize"
                        , localization.virtualComputer
                            |> Helpers.String.toTitleCase
                        , serverName maybeProject pageModel.serverUuid
                        ]

                ShareDetail pageModel ->
                    String.join " "
                        [ localization.share
                            |> Helpers.String.toTitleCase
                        , shareName maybeProject pageModel.shareUuid
                        ]

                ShareList _ ->
                    String.join " "
                        [ localization.share |> Helpers.String.pluralize |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                VolumeAttach _ ->
                    String.join " "
                        [ "Attach"
                        , localization.blockDevice
                            |> Helpers.String.toTitleCase
                        ]

                VolumeCreate _ ->
                    String.join " "
                        [ "Create"
                        , localization.blockDevice
                            |> Helpers.String.toTitleCase
                        ]

                VolumeDetail pageModel ->
                    String.join " "
                        [ localization.blockDevice
                            |> Helpers.String.toTitleCase
                        , volumeName maybeProject pageModel.volumeUuid
                        ]

                VolumeList _ ->
                    String.join " "
                        [ localization.blockDevice
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "for"
                        , projectName
                        ]

                VolumeMountInstructions _ ->
                    String.join " "
                        [ "Mount"
                        , localization.blockDevice
                            |> Helpers.String.toTitleCase
                        ]


serverName : Maybe Project -> OSTypes.ServerUuid -> String
serverName maybeProject serverUuid =
    maybeProject
        |> Maybe.andThen (\proj -> GetterSetters.serverLookup proj serverUuid)
        |> Maybe.map (\server -> server.osProps.name)
        |> Maybe.withDefault serverUuid


shareName : Maybe Project -> OSTypes.ShareUuid -> String
shareName maybeProject shareUuid =
    maybeProject
        |> Maybe.andThen (\proj -> GetterSetters.shareLookup proj shareUuid)
        |> Maybe.andThen (\share -> share.name)
        |> Maybe.withDefault shareUuid


volumeName : Maybe Project -> OSTypes.VolumeUuid -> String
volumeName maybeProject volumeUuid =
    maybeProject
        |> Maybe.andThen (\proj -> GetterSetters.volumeLookup proj volumeUuid)
        |> Maybe.andThen (\vol -> vol.name)
        |> Maybe.withDefault volumeUuid
