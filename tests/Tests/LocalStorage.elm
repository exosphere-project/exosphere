module Tests.LocalStorage exposing (endpointsDecoderMigrationSuite)

import Expect
import Json.Decode as Decode
import LocalStorage.LocalStorage as LocalStorage
import Test exposing (Test, describe, test)


{-| A stored-endpoints JSON blob as persisted by an Exosphere predating the S3 endpoint: it carries
every field Exosphere has historically written (including swift) but NO "s3" field. The decoder MUST
tolerate the missing "s3" field and yield `s3 == Nothing`, or every already-persisted project would
silently fail to decode on next load.
-}
preS3EndpointsJson : String
preS3EndpointsJson =
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


{-| A stored-endpoints JSON blob as persisted by an Exosphere predating the Swift endpoint: it carries
every field Exosphere has historically written before object storage but NO "swift" field. The
decoder MUST tolerate the missing "swift" field and yield `swift == Nothing`, or every
already-persisted project would silently fail to decode on next load.
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
    describe "endpointsDecoder tolerates the s3 migration"
        [ test "a legacy blob without an s3 field decodes with s3 == Nothing" <|
            \_ ->
                case Decode.decodeString LocalStorage.endpointsDecoder preS3EndpointsJson of
                    Ok endpoints ->
                        Expect.equal Nothing endpoints.s3

                    Err e ->
                        Expect.fail ("expected Ok endpoints, got Err: " ++ Decode.errorToString e)
        , test "a blob WITH an s3 field decodes with s3 == Just url" <|
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
                        , "s3": "http://s3.example.com"
                        }
                        """
                in
                case Decode.decodeString LocalStorage.endpointsDecoder json of
                    Ok endpoints ->
                        Expect.equal (Just "http://s3.example.com") endpoints.s3

                    Err e ->
                        Expect.fail ("expected Ok endpoints, got Err: " ++ Decode.errorToString e)
        , test "a legacy blob missing BOTH swift and s3 decodes both as Nothing" <|
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
                        }
                        """
                in
                case Decode.decodeString LocalStorage.endpointsDecoder json of
                    Ok endpoints ->
                        Expect.equal ( Nothing, Nothing ) ( endpoints.swift, endpoints.s3 )

                    Err e ->
                        Expect.fail ("expected Ok endpoints, got Err: " ++ Decode.errorToString e)
        , test "a legacy blob without a swift field decodes with swift == Nothing" <|
            \_ ->
                case Decode.decodeString LocalStorage.endpointsDecoder preSwiftEndpointsJson of
                    Ok endpoints ->
                        Expect.equal Nothing endpoints.swift

                    Err e ->
                        Expect.fail ("expected Ok endpoints, got Err: " ++ Decode.errorToString e)
        , test "a blob WITH a swift field decodes with swift == Just url" <|
            \_ ->
                case Decode.decodeString LocalStorage.endpointsDecoder preS3EndpointsJson of
                    Ok endpoints ->
                        Expect.equal (Just "https://openstack.example/swift") endpoints.swift

                    Err e ->
                        Expect.fail ("expected Ok endpoints, got Err: " ++ Decode.errorToString e)
        ]
