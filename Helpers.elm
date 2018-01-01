module Helpers exposing (..)

import Types exposing (..)


processError : Model -> a -> ( Model, Cmd Msg )
processError model error =
    let
        newMsgs =
            toString error :: model.messages
    in
        ( { model | messages = newMsgs }, Cmd.none )


getExternalNetwork : Model -> Maybe Network
getExternalNetwork model =
    List.filter (\n -> n.isExternal) model.networks |> List.head


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


serverLookup : Model -> Uuid -> Maybe Server
serverLookup model serverUuid =
    List.filter (\s -> s.uuid == serverUuid) model.servers |> List.head
