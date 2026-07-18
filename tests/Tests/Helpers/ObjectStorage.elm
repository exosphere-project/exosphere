module Tests.Helpers.ObjectStorage exposing (serviceCatalogSwiftSuite)

import Expect
import Helpers.Helpers as Helpers
import OpenStack.Types as OSTypes
import Test exposing (Test, describe, test)


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
                        Expect.equal Nothing endpoints.swift

                    Err e ->
                        Expect.fail ("expected Ok endpoints, got Err: " ++ e)
        ]
