module Tests.LocalStorage exposing
    ( endpointsDecoderMigrationSuite
    , extensionApprovalsMigrationSuite
    , extensionBatchesMigrationSuite
    )

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


extensionApprovalsMigrationSuite : Test
extensionApprovalsMigrationSuite =
    describe "storedStateDecoder tolerates the extension-approvals migration"
        [ test "a version-9 blob without exoextApprovals decodes with extensionApprovals == []" <|
            \_ ->
                case Decode.decodeString LocalStorage.storedStateDecoder """{ "9": { "projects": [] } }""" of
                    Ok storedState ->
                        Expect.equal [] storedState.extensionApprovals

                    Err e ->
                        Expect.fail ("expected Ok storedState, got Err: " ++ Decode.errorToString e)
        , test "a blob WITH exoextApprovals decodes the stored records" <|
            \_ ->
                let
                    json =
                        """
                        { "9":
                            { "projects": []
                            , "exoextApprovals":
                                [ { "cloudUrl": "https://keystone.example/v3"
                                  , "projectUuid": "proj-1"
                                  , "instanceUuid": "vm-1"
                                  , "nameAtApproval": "my-vm"
                                  , "approvedAt": "2026-07-17T12:00:00Z"
                                  , "manifestEtagAtApproval": "etag-xyz"
                                  }
                                ]
                            }
                        }
                        """
                in
                case Decode.decodeString LocalStorage.storedStateDecoder json of
                    Ok storedState ->
                        Expect.equal [ "vm-1" ] (List.map .instanceUuid storedState.extensionApprovals)

                    Err e ->
                        Expect.fail ("expected Ok storedState, got Err: " ++ Decode.errorToString e)
        ]


extensionBatchesMigrationSuite : Test
extensionBatchesMigrationSuite =
    describe "storedStateDecoder tolerates the extension-batches migration"
        [ test "a version-9 blob without exoextBatches decodes with extensionBatches == []" <|
            \_ ->
                -- Every already-persisted blob is this shape, so it has to read as "no batch
                -- to resume" rather than failing the whole stored state.
                case Decode.decodeString LocalStorage.storedStateDecoder """{ "9": { "projects": [] } }""" of
                    Ok storedState ->
                        Expect.equal [] storedState.extensionBatches

                    Err e ->
                        Expect.fail ("expected Ok storedState, got Err: " ++ Decode.errorToString e)
        , test "a blob WITH exoextBatches decodes the stored tail, in order" <|
            \_ ->
                let
                    json =
                        """
                        { "9":
                            { "projects": []
                            , "exoextBatches":
                                [ { "cloudUrl": "https://keystone.example/v3"
                                  , "projectUuid": "proj-1"
                                  , "instanceUuid": "vm-1"
                                  , "batchId": "exo-cs-batch-1000"
                                  , "remaining": ["i-2", "i-3"]
                                  }
                                ]
                            }
                        }
                        """
                in
                case Decode.decodeString LocalStorage.storedStateDecoder json of
                    Ok storedState ->
                        Expect.equal [ ( "vm-1", [ "i-2", "i-3" ] ) ]
                            (List.map (\b -> ( b.instanceUuid, b.remaining )) storedState.extensionBatches)

                    Err e ->
                        Expect.fail ("expected Ok storedState, got Err: " ++ Decode.errorToString e)
        , test "the approvals and batches stores are independent — neither breaks the other" <|
            \_ ->
                case Decode.decodeString LocalStorage.storedStateDecoder """{ "9": { "projects": [], "exoextBatches": [ { "instanceUuid": "vm-1", "remaining": ["i-2"] } ] } }""" of
                    Ok storedState ->
                        Expect.equal ( 0, 1 )
                            ( List.length storedState.extensionApprovals, List.length storedState.extensionBatches )

                    Err e ->
                        Expect.fail ("expected Ok storedState, got Err: " ++ Decode.errorToString e)
        ]
