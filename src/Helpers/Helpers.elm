module Helpers.Helpers exposing
    ( alwaysRegex
    , decodeFloatingIpOption
    , getNewFloatingIpOption
    , hiddenActionContexts
    , httpErrorToString
    , httpErrorWithBodyToString
    , naiveUuidParser
    , newServerMetadata
    , newServerNetworkOptions
    , parseConsoleLogForWorkflowToken
    , pipelineCmd
    , renderUserDataTemplate
    , serverFromThisExoClient
    , serverLessThanThisOld
    , serverOrigin
    , serverPollIntervalMs
    , serverResourceQtys
    , serviceCatalogToEndpoints
    , specialActionContexts
    , stringIsUuidOrDefault
    , stripTimeSinceBootFromLogLine
    )

-- Many functions which get and set things in the data model have been moved from here to GetterSetters.elm.
-- Getter/setter functions that remain here are too "smart" (too much business logic) for GetterSetters.elm.

import Helpers.ExoSetupStatus
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Http
import Json.Decode as Decode
import Json.Encode
import List.Extra
import OpenStack.Types as OSTypes
import Parser exposing ((|.))
import Regex
import Time
import Types.Error
import Types.Guacamole as GuacTypes
import Types.HelperTypes as HelperTypes
    exposing
        ( FloatingIpAssignmentStatus(..)
        , FloatingIpOption(..)
        , FloatingIpReuseOption(..)
        )
import Types.Project exposing (Endpoints, Project)
import Types.Server
    exposing
        ( ExoServerVersion
        , ExoSetupStatus(..)
        , NewServerNetworkOptions(..)
        , Server
        , ServerFromExoProps
        , ServerOrigin(..)
        )
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (SharedMsg)
import Types.Workflow
    exposing
        ( CustomWorkflowAuthToken
        , CustomWorkflowSource
        , ServerCustomWorkflowStatus(..)
        )
import UUID


alwaysRegex : String -> Regex.Regex
alwaysRegex regexStr =
    Regex.fromString regexStr |> Maybe.withDefault Regex.never


stringIsUuidOrDefault : String -> Bool
stringIsUuidOrDefault str =
    -- We accept some login fields from user (e.g. Keystone domains) that could be a name or a UUID.
    -- Further, OpenStack treats "default" as a special case that can be passed in UUID fields.
    -- This function helps functions like Rest.requestAuthToken specify the right JSON field (name or ID).
    let
        stringIsUuid =
            let
                strNoHyphens =
                    String.filter (\c -> c /= '-') str

                isValidHex : Char -> Bool
                isValidHex c =
                    String.any (\h -> c == h) "0123456789abcdefABCDEF"

                isValidLength =
                    String.length strNoHyphens == 32
            in
            String.all isValidHex strNoHyphens && isValidLength

        stringIsDefault =
            str == "default"
    in
    stringIsUuid || stringIsDefault


naiveUuidParser : Parser.Parser HelperTypes.Uuid
naiveUuidParser =
    -- Looks for any combination of hex digits and hyphens
    let
        isUuidChar c =
            Char.isHexDigit c || c == '-'
    in
    Parser.getChompedString <|
        Parser.succeed identity
            |. Parser.chompWhile isUuidChar


serviceCatalogToEndpoints : OSTypes.ServiceCatalog -> Maybe OSTypes.RegionId -> Result String Endpoints
serviceCatalogToEndpoints catalog maybeRegionId =
    let
        getService =
            GetterSetters.getServicePublicUrl catalog maybeRegionId

        -- Future optimization, use a real URL parser
        novaUrlWithMicroversionSupport : String -> String
        novaUrlWithMicroversionSupport url =
            url
                |> String.replace "/v2/" "/v2.1/"
                |> String.replace "/v2.0/" "/v2.1/"

        endpoints =
            [ ( "cinder", getService "volumev3" )
            , ( "glance", getService "image" )
            , ( "keystone", getService "identity" )
            , ( "manila", getService "sharev2" )
            , ( "nova", getService "compute" |> Maybe.map novaUrlWithMicroversionSupport )
            , ( "neutron", getService "network" )
            , ( "jetstream2Accounting", getService "accounting" )
            , ( "designate", getService "dns" )
            ]

        missingServiceName service =
            case service of
                ( name, Nothing ) ->
                    Just name

                _ ->
                    Nothing
    in
    case
        List.map Tuple.second endpoints
    of
        [ Just cinderUrl, Just glanceUrl, Just keystoneUrl, maybeManilaUrl, Just novaUrl, Just neutronUrl, maybeJetstream2AccountingUrl, maybeDesignateUrl ] ->
            Ok <| Endpoints cinderUrl glanceUrl keystoneUrl maybeManilaUrl novaUrl neutronUrl maybeJetstream2AccountingUrl maybeDesignateUrl

        _ ->
            Err <|
                "Could not locate URL(s) in service catalog for the following service(s): "
                    ++ (endpoints
                            |> List.filterMap missingServiceName
                            |> String.join ", "
                       )


encodeFloatingIpOption : FloatingIpOption -> List ( String, Json.Encode.Value )
encodeFloatingIpOption option =
    case option of
        UseFloatingIp reuseOption _ ->
            let
                reuseOptionStr =
                    case reuseOption of
                        CreateNewFloatingIp ->
                            "create"

                        UseExistingFloatingIp floatingIpUuid ->
                            floatingIpUuid
            in
            [ ( "exoFloatingIpOption", Json.Encode.string "useFloatingIp" )
            , ( "exoFloatingIpReuseOption", Json.Encode.string reuseOptionStr )
            ]

        Automatic ->
            [ ( "exoFloatingIpOption", Json.Encode.string "automatic" )
            ]

        DoNotUseFloatingIp ->
            [ ( "exoFloatingIpOption", Json.Encode.string "doNotUseFloatingIp" )
            ]


decodeFloatingIpOption : OSTypes.ServerDetails -> FloatingIpOption
decodeFloatingIpOption serverDetails =
    let
        maybeFloatingIpOptionStr =
            serverDetails.metadata
                |> List.Extra.find (\i -> i.key == "exoFloatingIpOption")
                |> Maybe.map .value

        maybeReuseOptionStr =
            serverDetails.metadata
                |> List.Extra.find (\i -> i.key == "exoFloatingIpReuseOption")
                |> Maybe.map .value
    in
    case maybeFloatingIpOptionStr of
        Nothing ->
            DoNotUseFloatingIp

        Just floatingIpOptionStr ->
            case floatingIpOptionStr of
                "useFloatingIp" ->
                    case maybeReuseOptionStr of
                        Nothing ->
                            UseFloatingIp CreateNewFloatingIp Unknown

                        Just reuseOptionStr ->
                            if reuseOptionStr == "create" then
                                UseFloatingIp CreateNewFloatingIp Unknown

                            else
                                UseFloatingIp (UseExistingFloatingIp reuseOptionStr) Unknown

                "automatic" ->
                    Automatic

                "doNotUseFloatingIp" ->
                    DoNotUseFloatingIp

                _ ->
                    Automatic


getNewFloatingIpOption : Project -> OSTypes.Server -> FloatingIpOption -> FloatingIpOption
getNewFloatingIpOption project osServer floatingIpOption =
    let
        hasPort =
            GetterSetters.getServerPorts project osServer.uuid
                |> List.isEmpty
                |> not

        hasFloatingIp =
            GetterSetters.getServerFloatingIps project osServer.uuid
                |> List.isEmpty
                |> not

        isDoneBuilding =
            osServer.details.openstackStatus /= OSTypes.ServerBuild
    in
    if hasFloatingIp then
        DoNotUseFloatingIp

    else
        case floatingIpOption of
            Automatic ->
                if isDoneBuilding && hasPort then
                    if
                        GetterSetters.getServerFixedIps project osServer.uuid
                            |> List.map ipv4AddressInRfc1918Space
                            |> List.member (Ok HelperTypes.PublicNonRfc1918Space)
                    then
                        DoNotUseFloatingIp

                    else
                        UseFloatingIp CreateNewFloatingIp Attemptable

                else
                    Automatic

            UseFloatingIp reuseOption status ->
                if List.member status [ Unknown, WaitingForResources ] then
                    if isDoneBuilding && hasPort then
                        UseFloatingIp reuseOption Attemptable

                    else
                        UseFloatingIp reuseOption WaitingForResources

                else
                    UseFloatingIp reuseOption status

            DoNotUseFloatingIp ->
                -- This is a terminal state
                DoNotUseFloatingIp


ipv4AddressInRfc1918Space : OSTypes.IpAddressValue -> Result String HelperTypes.IPv4AddressPublicRoutability
ipv4AddressInRfc1918Space ipValue =
    let
        octets =
            String.split "." ipValue
    in
    case List.map String.toInt octets of
        [ Just octet1, Just octet2, Just _, Just _ ] ->
            if
                (octet1 == 10)
                    || (octet1 == 172 && (16 <= octet2 || octet2 <= 31))
                    || (octet1 == 192 && octet2 == 168)
            then
                Ok HelperTypes.PrivateRfc1918Space

            else
                Ok HelperTypes.PublicNonRfc1918Space

        _ ->
            Err "Could not parse IPv4 address, it may be IPv6?"


renderUserDataTemplate :
    Project
    -> String
    -> Maybe String
    -> Bool
    -> Bool
    -> Maybe CustomWorkflowSource
    -> Bool
    -> String
    -> String
    -> Bool
    -> String
renderUserDataTemplate project userDataTemplate maybeKeypairName deployGuacamole deployDesktopEnvironment maybeCustomWorkflowSource installOperatingSystemUpdates instanceConfigMgtRepoUrl instanceConfigMgtRepoCheckout createCluster =
    -- Configure cloud-init user data based on user's choice for SSH keypair and Guacamole
    let
        getPublicKeyFromKeypairName : String -> Maybe String
        getPublicKeyFromKeypairName keypairName =
            project.keypairs
                |> RDPP.withDefault []
                |> List.Extra.find (\kp -> kp.name == keypairName)
                |> Maybe.map .publicKey

        authorizedKeysYaml : String
        authorizedKeysYaml =
            case maybeKeypairName |> Maybe.andThen getPublicKeyFromKeypairName of
                Just key ->
                    "\n    ssh-authorized-keys:\n      - " ++ key ++ "\n"

                Nothing ->
                    "\n"

        ansibleExtraVars : String
        ansibleExtraVars =
            -- JSON format is required to pass boolean values to Ansible as extra vars at runtime
            -- I'm so sorry... doing by hand because Elm Json.Encode doesn't insert enough double quotes
            String.concat
                [ """{\\"guac_enabled\\":"""
                , if deployGuacamole then
                    "true"

                  else
                    "false"
                , """,\\"gui_enabled\\":"""
                , if deployDesktopEnvironment then
                    "true"

                  else
                    "false"
                , case maybeCustomWorkflowSource of
                    Nothing ->
                        ""

                    Just customWorkflowSource ->
                        """,\\"workflow_source_repository\\":"""
                            ++ """\\\""""
                            ++ customWorkflowSource.repository
                            ++ """\\\""""
                            ++ (if customWorkflowSource.reference /= "" then
                                    """,\\"workflow_repo_version\\":""" ++ """\\\"""" ++ customWorkflowSource.reference ++ """\\\""""

                                else
                                    ""
                               )
                , """}"""
                ]

        installOperatingSystemUpatesYaml : String
        installOperatingSystemUpatesYaml =
            if installOperatingSystemUpdates then
                "true"

            else
                "false"

        -- TODO: If no app credential, then use username and ask for password
        ( appCredentialUuid, appCredentialSecret ) =
            case project.secret of
                Types.Project.ApplicationCredential appCredential ->
                    ( appCredential.uuid, appCredential.secret )

                Types.Project.NoProjectSecret ->
                    ( "", "" )

        createClusterYaml : String
        createClusterYaml =
            if createCluster then
                """su - rocky -c "git clone --branch rocky-linux --single-branch --depth 1 https://github.com/XSEDE/CRI_Jetstream_Cluster.git; cd CRI_Jetstream_Cluster; ./cluster_create_local.sh -d 2>&1 | tee local_create.log" """

            else
                """echo "Not creating a cluster, moving along..." """

        openrcFileYamlTemplate : String
        openrcFileYamlTemplate =
            """
- path: /home/rocky/openrc.sh
  content: |
    export OS_AUTH_TYPE=v3applicationcredential
    export OS_AUTH_URL={os-auth-url}
    export OS_IDENTITY_API_VERSION=3
    export OS_REGION_NAME="{os-region}"
    export OS_INTERFACE=public
    export OS_APPLICATION_CREDENTIAL_ID="{os-ac-id}"
    export OS_APPLICATION_CREDENTIAL_SECRET="{os-ac-secret}"
  owner: rocky:rocky
  permissions: '0400'
  defer: true"""

        includeOpenrcFile : Bool
        includeOpenrcFile =
            createCluster

        regionId : String
        regionId =
            case project.region of
                Nothing ->
                    "RegionOne"

                Just region ->
                    region.id

        openrcFileYaml : Maybe String
        openrcFileYaml =
            if includeOpenrcFile then
                [ ( "{os-auth-url}", project.endpoints.keystone )
                , ( "{os-region}", regionId )
                , ( "{os-ac-id}", appCredentialUuid )
                , ( "{os-ac-secret}", appCredentialSecret )
                ]
                    |> List.foldl (\t -> String.replace (Tuple.first t) (Tuple.second t)) openrcFileYamlTemplate
                    |> Just

            else
                Nothing

        filesToWrite =
            [ openrcFileYaml ]
                |> List.filterMap identity

        writeFilesYaml : String
        writeFilesYaml =
            let
                writeFilesHeader =
                    """
write_files:"""
            in
            if List.isEmpty filesToWrite then
                ""

            else
                writeFilesHeader ++ String.concat filesToWrite
    in
    [ ( "{ssh-authorized-keys}\n", authorizedKeysYaml )
    , ( "{ansible-extra-vars}", ansibleExtraVars )
    , ( "{install-os-updates}", installOperatingSystemUpatesYaml )
    , ( "{instance-config-mgt-repo-url}", instanceConfigMgtRepoUrl )
    , ( "{instance-config-mgt-repo-checkout}", instanceConfigMgtRepoCheckout )
    , ( "{create-cluster-command}", createClusterYaml )
    , ( "{write-files}", writeFilesYaml )
    ]
        |> List.foldl (\t -> String.replace (Tuple.first t) (Tuple.second t)) userDataTemplate


newServerMetadata : ExoServerVersion -> UUID.UUID -> Bool -> Bool -> String -> FloatingIpOption -> Maybe CustomWorkflowSource -> List ( String, Json.Encode.Value )
newServerMetadata exoServerVersion exoClientUuid deployGuacamole deployDesktopEnvironment exoCreatorUsername floatingIpCreationOption maybeCustomWorkflowSource =
    let
        guacMetadata =
            if deployGuacamole then
                [ ( "exoGuac"
                  , Json.Encode.string <|
                        Json.Encode.encode 0 <|
                            Json.Encode.object
                                [ ( "v", Json.Encode.int 1 )
                                , ( "ssh", Json.Encode.bool True )
                                , ( "vnc", Json.Encode.bool deployDesktopEnvironment )
                                ]
                  )
                ]

            else
                []
    in
    List.concat
        [ guacMetadata
        , [ ( "exoServerVersion"
            , Json.Encode.string (String.fromInt exoServerVersion)
            )
          , ( "exoClientUuid"
            , Json.Encode.string (UUID.toString exoClientUuid)
            )
          , ( "exoCreatorUsername"
            , Json.Encode.string exoCreatorUsername
            )
          , ( "exoSetup"
            , Json.Encode.string <|
                Helpers.ExoSetupStatus.encodeMetadataItem ExoSetupWaiting Nothing
            )
          ]
        , encodeFloatingIpOption floatingIpCreationOption
        , Maybe.map encodeCustomWorkflowSource maybeCustomWorkflowSource |> Maybe.withDefault []
        ]


newServerNetworkOptions : Project -> NewServerNetworkOptions
newServerNetworkOptions project =
    {- When creating a new server, make a reasonable choice of project network, if we can. -}
    let
        projectNets =
            -- Filter on networks that are status ACTIVE, adminStateUp, and not external
            case project.networks.data of
                RDPP.DoHave networks _ ->
                    networks
                        |> List.filter (\n -> n.status == "ACTIVE")
                        |> List.filter (\n -> n.adminStateUp)
                        |> List.filter (\n -> n.isExternal == False)

                RDPP.DontHave ->
                    []

        maybeProjectNameNet =
            projectNets
                |> List.Extra.find
                    (\n ->
                        String.contains
                            (String.toLower project.auth.project.name)
                            (String.toLower n.name)
                    )
    in
    case ( project.autoAllocatedNetworkUuid.data, project.autoAllocatedNetworkUuid.refreshStatus ) of
        -- Prefer auto-allocated network topology that we get/create
        ( RDPP.DoHave netUuid _, _ ) ->
            AutoSelectedNetwork netUuid

        ( RDPP.DontHave, RDPP.Loading ) ->
            NetworksLoading

        ( RDPP.DontHave, RDPP.NotLoading Nothing ) ->
            -- We haven't gotten auto-allocated network yet, say "loading" anyway
            NetworksLoading

        ( RDPP.DontHave, RDPP.NotLoading (Just _) ) ->
            -- auto-allocation API call failed, so look through list of networks
            projectNets
                |> List.Extra.find (\n -> n.name == "auto_allocated_network")
                |> Maybe.map (.uuid >> AutoSelectedNetwork)
                |> Maybe.withDefault
                    (maybeProjectNameNet
                        |> Maybe.map (.uuid >> AutoSelectedNetwork)
                        |> Maybe.withDefault
                            (case ( project.networks.refreshStatus, projectNets ) of
                                ( RDPP.Loading, _ ) ->
                                    NetworksLoading

                                ( RDPP.NotLoading _, [] ) ->
                                    NoneAvailable

                                ( RDPP.NotLoading _, _ :: _ ) ->
                                    ManualNetworkSelection
                            )
                    )


serverOrigin : OSTypes.ServerDetails -> ServerOrigin
serverOrigin serverDetails =
    let
        maybeDecodedVersion =
            serverDetails.metadata
                |> List.Extra.find (\i -> i.key == "exoServerVersion")
                |> Maybe.map .value
                |> Maybe.andThen String.toInt

        version0 =
            serverDetails.metadata
                |> List.any (\i -> i.key == "exouserPassword")

        exoServerVersion =
            case ( maybeDecodedVersion, version0 ) of
                ( Just v, _ ) ->
                    Just v

                ( Nothing, True ) ->
                    Just 0

                ( Nothing, False ) ->
                    Nothing

        ( exoSetupStatus, exoSetupTimestamp ) =
            List.Extra.find (\i -> i.key == "exoSetup") serverDetails.metadata
                |> Maybe.map .value
                |> Maybe.map Helpers.ExoSetupStatus.decodeExoSetupJson
                |> Maybe.withDefault ( ExoSetupUnknown, Nothing )

        exoSetupStatusRDPP =
            RDPP.RemoteDataPlusPlus (RDPP.DoHave ( exoSetupStatus, exoSetupTimestamp ) (Time.millisToPosix 0)) (RDPP.NotLoading Nothing)

        guacamolePropsDecoder : Decode.Decoder GuacTypes.LaunchedWithGuacProps
        guacamolePropsDecoder =
            Decode.map3
                GuacTypes.LaunchedWithGuacProps
                (Decode.field "ssh" Decode.bool)
                (Decode.field "vnc" Decode.bool)
                (Decode.succeed RDPP.empty)

        guacamoleStatus =
            case
                List.Extra.find (\i -> i.key == "exoGuac") serverDetails.metadata
            of
                Nothing ->
                    GuacTypes.NotLaunchedWithGuacamole

                Just item ->
                    case Decode.decodeString guacamolePropsDecoder item.value of
                        Ok launchedWithGuacProps ->
                            GuacTypes.LaunchedWithGuacamole launchedWithGuacProps

                        Err _ ->
                            GuacTypes.NotLaunchedWithGuacamole

        customWorkflowPropsDecoder : Decode.Decoder CustomWorkflowSource
        customWorkflowPropsDecoder =
            Decode.map3
                CustomWorkflowSource
                (Decode.field "repo" Decode.string)
                (Decode.field "ref" Decode.string)
                (Decode.field "path" Decode.string)

        customWorkflowStatus =
            case
                List.Extra.find (\i -> i.key == "exoCustomWorkflow") serverDetails.metadata
            of
                Nothing ->
                    NotLaunchedWithCustomWorkflow

                Just item ->
                    case Decode.decodeString customWorkflowPropsDecoder item.value of
                        Ok launchedWithCustomWorkflowPropsProps ->
                            LaunchedWithCustomWorkflow
                                { source = launchedWithCustomWorkflowPropsProps
                                , authToken = RDPP.empty
                                }

                        Err _ ->
                            NotLaunchedWithCustomWorkflow

        creatorName =
            serverDetails.metadata
                |> List.Extra.find (\i -> i.key == "exoCreatorUsername")
                |> Maybe.map .value
    in
    case exoServerVersion of
        Just v ->
            ServerFromExo <|
                ServerFromExoProps v exoSetupStatusRDPP RDPP.empty guacamoleStatus customWorkflowStatus creatorName

        Nothing ->
            ServerNotFromExo


encodeCustomWorkflowSource : CustomWorkflowSource -> List ( String, Json.Encode.Value )
encodeCustomWorkflowSource customWorkflowSource =
    [ ( "exoCustomWorkflow"
      , Json.Encode.string <|
            Json.Encode.encode 0 <|
                Json.Encode.object
                    [ ( "v", Json.Encode.int 1 )
                    , ( "repo", Json.Encode.string customWorkflowSource.repository )
                    , ( "ref", Json.Encode.string customWorkflowSource.reference )
                    , ( "path", Json.Encode.string customWorkflowSource.path )
                    ]
      )
    ]


stripTimeSinceBootFromLogLine : String -> String
stripTimeSinceBootFromLogLine line =
    -- Remove everything before first open curly brace
    -- This accommodates lines written to /dev/kmsg which begin with, e.g., `[ 2915.727779]`
    case String.indices "{" line |> List.head of
        Just index ->
            String.dropLeft index line

        Nothing ->
            line


parseConsoleLogForWorkflowToken : String -> Maybe CustomWorkflowAuthToken
parseConsoleLogForWorkflowToken consoleLog =
    let
        loglines =
            String.split "\n" consoleLog

        decodedData =
            loglines
                |> List.map stripTimeSinceBootFromLogLine
                |> List.filterMap
                    (\l -> Decode.decodeString workflowTokenDecoder l |> Result.toMaybe)
    in
    List.reverse decodedData
        |> List.head


workflowTokenDecoder : Decode.Decoder CustomWorkflowAuthToken
workflowTokenDecoder =
    Decode.field
        "exoWorkflowToken"
        Decode.string


serverFromThisExoClient : UUID.UUID -> Server -> Bool
serverFromThisExoClient clientUuid server =
    -- Determine if server was created by this Exosphere client
    List.member (OSTypes.MetadataItem "exoClientUuid" (UUID.toString clientUuid)) server.osProps.details.metadata


serverPollIntervalMs : Project -> Server -> Int
serverPollIntervalMs project server =
    case GetterSetters.serverCreatedByCurrentUser project server.osProps.uuid of
        Just createdByCurrentUser ->
            if createdByCurrentUser then
                case
                    ( server.osProps.details.openstackStatus
                    , ( server.exoProps.deletionAttempted
                      , server.exoProps.targetOpenstackStatus
                      , server.exoProps.serverOrigin
                      )
                    )
                of
                    ( OSTypes.ServerBuild, _ ) ->
                        15000

                    ( _, ( False, Nothing, ServerNotFromExo ) ) ->
                        -- Not created from Exosphere, not deleting or waiting a pending server action
                        60000

                    ( _, ( False, Nothing, ServerFromExo { exoSetupStatus } ) ) ->
                        case exoSetupStatus.data of
                            RDPP.DoHave ( ExoSetupWaiting, _ ) _ ->
                                -- Exosphere-created, booting up for the first time
                                15000

                            RDPP.DoHave ( ExoSetupRunning, _ ) _ ->
                                -- Exosphere-created, running setup
                                10000

                            RDPP.DoHave _ _ ->
                                -- Exosphere-created, not waiting for setup to complete
                                60000

                            RDPP.DontHave ->
                                -- Exosphere-created and Exosphere setup status unknown
                                15000

                    _ ->
                        -- We're expecting OpenStack status to change (or server to be deleted) very soon
                        4500

            else
                300000

        Nothing ->
            300000


serverLessThanThisOld : Server -> Time.Posix -> Int -> Bool
serverLessThanThisOld server currentTime maxServerAgeMillis =
    let
        curTimeMillis =
            Time.posixToMillis currentTime
    in
    (curTimeMillis - Time.posixToMillis server.osProps.details.created) < maxServerAgeMillis


serverResourceQtys : Project -> OSTypes.Flavor -> Server -> HelperTypes.ServerResourceQtys
serverResourceQtys project flavor server =
    { cores = flavor.vcpu
    , vgpus =
        -- Matching on `resources{group}:VGPU` per
        -- https://docs.openstack.org/nova/ussuri/configuration/extra-specs.html#resources
        flavor.extra_specs
            |> List.filter (\{ key } -> String.startsWith "resources" key && String.endsWith ":VGPU" key)
            |> List.head
            |> Maybe.map .value
            |> Maybe.andThen String.toInt
    , ramGb = flavor.ram_mb // 1024
    , rootDiskGb =
        case
            GetterSetters.getBootVolume
                (RDPP.withDefault [] project.volumes)
                server.osProps.uuid
        of
            Just backingVolume ->
                Just backingVolume.size

            Nothing ->
                if flavor.disk_root > 0 then
                    Just flavor.disk_root

                else
                    Nothing
    }


{-| This one helps string functions together in Rest.ApiModelHelpers and other places
-}
pipelineCmd : (SharedModel -> ( SharedModel, Cmd SharedMsg )) -> ( SharedModel, Cmd SharedMsg ) -> ( SharedModel, Cmd SharedMsg )
pipelineCmd fn ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            fn model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


httpErrorWithBodyToString : Types.Error.HttpErrorWithBody -> String
httpErrorWithBodyToString errorWithBody =
    httpErrorToString errorWithBody.error


httpErrorToString : Http.Error -> String
httpErrorToString httpError =
    case httpError of
        Http.BadUrl url ->
            "BadUrl: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus int ->
            "BadStatus: " ++ String.fromInt int

        Http.BadBody string ->
            "BadBody: " ++ string


hiddenActionContexts : List String
hiddenActionContexts =
    [ specialActionContexts.networkConnectivity ]


specialActionContexts : { networkConnectivity : String }
specialActionContexts =
    { networkConnectivity = "check network connectivity" }
