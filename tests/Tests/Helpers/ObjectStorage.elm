module Tests.Helpers.ObjectStorage exposing (serviceCatalogSwiftSuite)

import Expect
import Helpers.Helpers as Helpers
import OpenStack.ObjectStorage as ObjectStorage
import OpenStack.Types as OSTypes
import Test exposing (Test, describe, test)
import Types.Error exposing (ErrorLevel(..))
import Types.SharedMsg as SharedMsg


{-| Build a Public endpoint with a placeholder region.
-}
publicEndpoint : String -> OSTypes.Endpoint
publicEndpoint url =
    { interface = OSTypes.Public
    , url = url
    , regionId = "RegionOne"
    }


service : String -> String -> OSTypes.Service
service name url =
    { name = name
    , type_ = name
    , endpoints = [ publicEndpoint url ]
    }


{-| A catalog containing every service Exosphere requires (so the decode succeeds),
optionally plus object-store.
-}
baseCatalog : OSTypes.ServiceCatalog
baseCatalog =
    [ service "volumev3" "https://cinder.example.com/v3"
    , service "image" "https://glance.example.com"
    , service "identity" "https://keystone.example.com/v3"
    , service "compute" "https://nova.example.com/v2.1"
    , service "network" "https://neutron.example.com"
    ]


serviceCatalogSwiftSuite : Test
serviceCatalogSwiftSuite =
    describe "serviceCatalogToEndpoints maps the object-store (Swift) service"
        [ test "maps an object-store service to endpoints.swift = Just url" <|
            \_ ->
                let
                    catalog =
                        baseCatalog
                            ++ [ service "object-store" "https://swift.example.com/swift/v1/AUTH_proj" ]
                in
                case Helpers.serviceCatalogToEndpoints catalog Nothing of
                    Ok endpoints ->
                        Expect.equal (Just "https://swift.example.com/swift/v1/AUTH_proj") endpoints.swift

                    Err e ->
                        Expect.fail ("expected Ok endpoints, got Err: " ++ e)
        , test "maps a missing object-store service to endpoints.swift = Nothing" <|
            \_ ->
                case Helpers.serviceCatalogToEndpoints baseCatalog Nothing of
                    Ok endpoints ->
                        let
                            uploadStatusReasons =
                                [ ObjectStorage.Failed "transport failed"
                                , ObjectStorage.Rejected "file too large"
                                ]
                                    |> List.map
                                        (\status ->
                                            case status of
                                                ObjectStorage.Failed reason ->
                                                    reason

                                                ObjectStorage.Rejected reason ->
                                                    reason

                                                _ ->
                                                    ""
                                        )

                            uploadCallbackPayload =
                                case
                                    SharedMsg.ReceiveUploadObject
                                        { actionContext = "upload object"
                                        , level = ErrorWarn
                                        , recoveryHint = Nothing
                                        }
                                        7
                                        "container"
                                        (Just "prefix/")
                                        (Ok ())
                                of
                                    SharedMsg.ReceiveUploadObject _ _ containerName maybePrefix _ ->
                                        ( containerName, maybePrefix )

                                    _ ->
                                        ( "", Nothing )
                        in
                        Expect.all
                            [ \_ -> Expect.equal Nothing endpoints.swift
                            , \_ -> Expect.equal [ "transport failed", "file too large" ] uploadStatusReasons
                            , \_ -> Expect.equal ( "container", Just "prefix/" ) uploadCallbackPayload
                            ]
                            ()

                    Err e ->
                        Expect.fail ("expected Ok endpoints, got Err: " ++ e)
        ]
