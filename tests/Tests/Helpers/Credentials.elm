module Tests.Helpers.Credentials exposing (openStackCredentialWriteFilesYamlSuite)

import Dict
import Expect
import Helpers.Credentials as Credentials
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Types as OSTypes
import Test exposing (Test, describe, test)
import Time
import Types.Project exposing (Project, ProjectSecret(..))


project : ProjectSecret -> Project
project secret =
    { secret = secret
    , auth =
        { catalog = []
        , project = { name = "Project One", uuid = "project-uuid" }
        , projectDomain = { name = "Default", uuid = "project-domain-uuid" }
        , user = { name = "user-one", uuid = "user-uuid" }
        , userDomain = { name = "Default", uuid = "user-domain-uuid" }
        , expiresAt = Time.millisToPosix 0
        , tokenValue = "token"
        }
    , region = Just { id = "RegionOne", description = "Region One" }
    , endpoints =
        { cinder = "https://openstack.example/cinder"
        , glance = "https://openstack.example/glance"
        , keystone = "https://openstack.example/keystone/v3"
        , manila = Nothing
        , nova = "https://openstack.example/nova"
        , neutron = "https://openstack.example/neutron"
        , placement = Nothing
        , jetstream2Accounting = Nothing
        , designate = Nothing
        }
    , description = Nothing
    , images = RDPP.empty
    , servers = RDPP.empty
    , serverEvents = Dict.empty
    , serverExoActions = Dict.empty
    , serverSecurityGroups = Dict.empty
    , serverVolumeAttachments = Dict.empty
    , serverVolumeActions = Dict.empty
    , serverActionRequestQueue = Dict.empty
    , shares = RDPP.empty
    , shareAccessRules = Dict.empty
    , shareExportLocations = Dict.empty
    , shareTypes = RDPP.empty
    , flavors = RDPP.empty
    , keypairs = RDPP.empty
    , volumes = RDPP.empty
    , volumeSnapshots = RDPP.empty
    , networks = RDPP.empty
    , autoAllocatedNetworkUuid = RDPP.empty
    , floatingIps = RDPP.empty
    , dnsRecordSets = RDPP.empty
    , ports = RDPP.empty
    , securityGroups = RDPP.empty
    , securityGroupActions = Dict.empty
    , registeredLimits = RDPP.empty
    , projectLimits = RDPP.empty
    , projectUsages = RDPP.empty
    , computeQuota = RDPP.empty
    , volumeQuota = RDPP.empty
    , networkQuota = RDPP.empty
    , shareQuota = RDPP.empty
    , serverImages = []
    , jetstream2Allocations = RDPP.empty
    , knownUsernames = Dict.empty
    }


applicationCredential : OSTypes.ApplicationCredential
applicationCredential =
    { uuid = "app-cred-id"
    , secret = "app-cred-secret"
    }


openStackCredentialWriteFilesYamlSuite : Test
openStackCredentialWriteFilesYamlSuite =
    let
        expectContains : String -> String -> Expect.Expectation
        expectContains needle haystack =
            String.contains needle haystack
                |> Expect.equal True
    in
    describe "OpenStack credential write_files cloud-init"
        [ test "writes openrc.sh and clouds.yaml with application credential material" <|
            \_ ->
                project (ApplicationCredential applicationCredential)
                    |> Credentials.getOpenStackCredentialWriteFilesYaml
                    |> Expect.all
                        [ expectContains "\nwrite_files:"
                        , expectContains "- path: /home/exouser/openrc.sh"
                        , expectContains "- path: /home/exouser/.config/openstack/clouds.yaml"
                        , expectContains "export OS_APPLICATION_CREDENTIAL_ID=\"app-cred-id\""
                        , expectContains "export OS_APPLICATION_CREDENTIAL_SECRET=\"app-cred-secret\""
                        , expectContains "application_credential_id: app-cred-id"
                        , expectContains "application_credential_secret: app-cred-secret"
                        , expectContains "owner: exouser:exouser"
                        , expectContains "permissions: '0400'"
                        , expectContains "defer: true"
                        ]
        , test "does not write files when project has no stored secret" <|
            \_ ->
                project NoProjectSecret
                    |> Credentials.getOpenStackCredentialWriteFilesYaml
                    |> Expect.equal ""
        ]
