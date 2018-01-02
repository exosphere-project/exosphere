module Helpers exposing (processError, serviceCatalogToEndpoints, getExternalNetwork, checkFloatingIpState, serverLookup, providerLookup)

import Types.HelperTypes as HelperTypes
import Types.Types exposing (..)
import Types.OpenstackTypes as OpenstackTypes


processError : Model -> a -> ( Model, Cmd Msg )
processError model error =
    let
        newMsgs =
            toString error :: model.messages
    in
        ( { model | messages = newMsgs }, Cmd.none )


serviceCatalogToEndpoints : OpenstackTypes.ServiceCatalog -> Endpoints
serviceCatalogToEndpoints catalog =
    Endpoints
        (getServicePublicUrl "glance" catalog)
        (getServicePublicUrl "nova" catalog)
        (getServicePublicUrl "neutron" catalog)


getServicePublicUrl : String -> OpenstackTypes.ServiceCatalog -> HelperTypes.Url
getServicePublicUrl serviceName catalog =
    let
        maybeService =
            getServiceFromCatalog serviceName catalog

        maybePublicEndpoint =
            getPublicEndpointFromService maybeService
    in
        case maybePublicEndpoint of
            Just endpoint ->
                endpoint.url

            Nothing ->
                ""


getServiceFromCatalog : String -> OpenstackTypes.ServiceCatalog -> Maybe OpenstackTypes.Service
getServiceFromCatalog serviceName catalog =
    List.filter (\s -> s.name == serviceName) catalog
        |> List.head


getPublicEndpointFromService : Maybe OpenstackTypes.Service -> Maybe OpenstackTypes.Endpoint
getPublicEndpointFromService maybeService =
    case maybeService of
        Just service ->
            List.filter (\e -> e.interface == OpenstackTypes.Public) service.endpoints
                |> List.head

        Nothing ->
            Nothing


getExternalNetwork : Provider -> Maybe Network
getExternalNetwork provider =
    List.filter (\n -> n.isExternal) provider.networks |> List.head


checkFloatingIpState : ServerDetails -> FloatingIpState -> FloatingIpState
checkFloatingIpState serverDetails floatingIpState =
    let
        hasFixedIp =
            List.filter (\a -> a.openstackType == Fixed) serverDetails.ipAddresses
                |> List.isEmpty
                |> not

        hasFloatingIp =
            List.filter (\a -> a.openstackType == Floating) serverDetails.ipAddresses
                |> List.isEmpty
                |> not

        isActive =
            serverDetails.status == "ACTIVE"
    in
        case floatingIpState of
            RequestedWaiting ->
                if hasFloatingIp then
                    Success
                else
                    RequestedWaiting

            Failed ->
                Failed

            _ ->
                if hasFloatingIp then
                    Success
                else if hasFixedIp && isActive then
                    Requestable
                else
                    NotRequestable


serverLookup : Provider -> ServerUuid -> Maybe Server
serverLookup provider serverUuid =
    List.filter (\s -> s.uuid == serverUuid) provider.servers |> List.head


providerLookup : Model -> ProviderName -> Maybe Provider
providerLookup model providerName =
    List.filter (\p -> p.name == providerName) model.providers
        |> List.head
