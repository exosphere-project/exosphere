module View.PageTitle exposing (pageTitle)

import Helpers.GetterSetters as GetterSetters
import Helpers.Url as UrlHelpers
import OpenStack.Types as OSTypes
import Types.Types
    exposing
        ( LoginView(..)
        , Model
        , NonProjectViewConstructor(..)
        , Project
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import View.Helpers as VH


pageTitle : Model -> String
pageTitle model =
    case model.viewState of
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

                SelectProjects keystoneUrl _ ->
                    let
                        providerTitle =
                            keystoneUrl
                                |> UrlHelpers.hostnameFromUrl
                                |> VH.titleFromHostname
                    in
                    "Select Projects for " ++ providerTitle

                MessageLog ->
                    "Message Log"

                Settings ->
                    "Settings"

                HelpAbout ->
                    "About Exosphere"

        ProjectView projectIdentifier _ projectViewConstructor ->
            let
                maybeProject =
                    GetterSetters.projectLookup model projectIdentifier

                projectName =
                    maybeProject
                        |> Maybe.map (\p -> p.auth.project.name)
                        |> Maybe.withDefault "could not find project name"
            in
            case projectViewConstructor of
                ListImages _ _ ->
                    "Images for " ++ projectName

                ListProjectServers _ ->
                    "Servers for " ++ projectName

                ListProjectVolumes _ ->
                    "Volumes for " ++ projectName

                ListQuotaUsage ->
                    "Quota Usage for " ++ projectName

                ServerDetail serverUuid _ ->
                    "Server " ++ serverName maybeProject serverUuid

                CreateServerImage serverUuid _ ->
                    "Create Image for " ++ serverName maybeProject serverUuid

                VolumeDetail volumeUuid _ ->
                    "Volume " ++ volumeName maybeProject volumeUuid

                CreateServer _ ->
                    "Create Server"

                CreateVolume _ _ ->
                    "Create Volume"

                AttachVolumeModal _ _ ->
                    "Attach Volume"

                MountVolInstructions _ ->
                    "Mount Volume"


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
