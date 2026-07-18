module Tests.CloudShield.Discovery exposing (manifestObjectLocationSuite)

import CloudShield.Discovery as Discovery
import Expect
import Test exposing (Test, describe, test)


sentinel :
    { store : Discovery.Store, container : Maybe String, prefix : Maybe String, manifest : Maybe String }
    -> Discovery.Sentinel
sentinel { store, container, prefix, manifest } =
    { kind = "cloudshield"
    , schema = Just "1.0"
    , store = store
    , flags = []
    , published = Nothing
    , container = container
    , prefix = prefix
    , manifest = manifest
    }


manifestObjectLocationSuite : Test
manifestObjectLocationSuite =
    describe "manifestObjectLocation resolves the store=swift manifest object (phase-0-spec.md §3.1/§3.3)"
        [ test "store=swift with container/prefix/manifest resolves container + prefix-joined key" <|
            \_ ->
                Expect.equal
                    (Just { container = "exo-cs-mailbox", objectName = "instance-abc/manifest.json" })
                    (Discovery.manifestObjectLocation
                        (sentinel
                            { store = Discovery.StoreSwift
                            , container = Just "exo-cs-mailbox"
                            , prefix = Just "instance-abc/"
                            , manifest = Just "manifest.json"
                            }
                        )
                    )
        , test "store=swift with no prefix omits it from the key" <|
            \_ ->
                Expect.equal
                    (Just { container = "exo-cs-mailbox", objectName = "manifest.json" })
                    (Discovery.manifestObjectLocation
                        (sentinel
                            { store = Discovery.StoreSwift
                            , container = Just "exo-cs-mailbox"
                            , prefix = Nothing
                            , manifest = Just "manifest.json"
                            }
                        )
                    )
        , test "store=swift missing container resolves to Nothing" <|
            \_ ->
                Expect.equal Nothing
                    (Discovery.manifestObjectLocation
                        (sentinel
                            { store = Discovery.StoreSwift
                            , container = Nothing
                            , prefix = Just "instance-abc/"
                            , manifest = Just "manifest.json"
                            }
                        )
                    )
        , test "store=swift missing manifest key resolves to Nothing" <|
            \_ ->
                Expect.equal Nothing
                    (Discovery.manifestObjectLocation
                        (sentinel
                            { store = Discovery.StoreSwift
                            , container = Just "exo-cs-mailbox"
                            , prefix = Just "instance-abc/"
                            , manifest = Nothing
                            }
                        )
                    )
        , test "store=metadata never resolves a swift location, even if container/manifest are set" <|
            \_ ->
                Expect.equal Nothing
                    (Discovery.manifestObjectLocation
                        (sentinel
                            { store = Discovery.StoreMetadata
                            , container = Just "exo-cs-mailbox"
                            , prefix = Just "instance-abc/"
                            , manifest = Just "manifest.json"
                            }
                        )
                    )
        ]
