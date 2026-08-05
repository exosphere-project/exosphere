module Tests.LocalStorage exposing (endpointsDecoderMigrationSuite)

import Expect
import Json.Decode as Decode
import LocalStorage.LocalStorage as LocalStorage
import Test exposing (Test, describe, test)


{-| A stored-endpoints JSON blob as persisted by an Exosphere predating the Swift endpoint: it carries
every field Exosphere has historically written but NO "swift" field. The decoder MUST tolerate the
missing "swift" field and yield `swift == Nothing`, or every already-persisted project would
silently fail to decode on next load.
-}
preSwiftEndpointsJson : String
preSwiftEndpointsJson =
    """
    { "cinder": "https://openstack.example/cinder"
    , "glance": "https://openstack.example/glance"
    , "keystone": "https://openstack.example/keystone/v3"
    , "manila": null
    , "nova": "https://openstack.example/nova"
    , "neutron": "https://openstack.example/neutron"
    , "jetstream2Accounting": null
    , "designate": null
    }
    """


endpointsDecoderMigrationSuite : Test
endpointsDecoderMigrationSuite =
    describe "endpointsDecoder tolerates the swift migration"
        [ test "a legacy blob without a swift field decodes with swift == Nothing" <|
            \_ ->
                case Decode.decodeString LocalStorage.endpointsDecoder preSwiftEndpointsJson of
                    Ok endpoints ->
                        Expect.equal Nothing endpoints.swift

                    Err e ->
                        Expect.fail ("expected Ok endpoints, got Err: " ++ Decode.errorToString e)
        , test "a blob WITH a swift field decodes with swift == Just url" <|
            \_ ->
                let
                    json =
                        """
                        { "cinder": "https://openstack.example/cinder"
                        , "glance": "https://openstack.example/glance"
                        , "keystone": "https://openstack.example/keystone/v3"
                        , "manila": null
                        , "nova": "https://openstack.example/nova"
                        , "neutron": "https://openstack.example/neutron"
                        , "jetstream2Accounting": null
                        , "designate": null
                        , "swift": "https://openstack.example/swift"
                        }
                        """
                in
                case Decode.decodeString LocalStorage.endpointsDecoder json of
                    Ok endpoints ->
                        Expect.equal (Just "https://openstack.example/swift") endpoints.swift

                    Err e ->
                        Expect.fail ("expected Ok endpoints, got Err: " ++ Decode.errorToString e)
        ]
