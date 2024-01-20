module OpenStack.DnsRecordSet exposing (DnsRecordSet, RecordType, addressToRecord, fromStringToRecordType)

import List.Extra
import OpenStack.HelperTypes
import Set


type alias DnsRecordSet =
    { id : OpenStack.HelperTypes.Uuid
    , name : String
    , type_ : RecordType
    , ttl : Maybe Int
    , records : Set.Set String
    }


type RecordType
    = ARecord
    | PTRRecord
    | SOARecord
    | NSRecord


fromStringToRecordType : String -> Result String RecordType
fromStringToRecordType recordSet =
    case recordSet of
        "A" ->
            Ok ARecord

        "PTR" ->
            Ok PTRRecord

        "SOA" ->
            Ok SOARecord

        "NS" ->
            Ok NSRecord

        _ ->
            Err (recordSet ++ " is not valid")


addressToRecord : List DnsRecordSet -> String -> Maybe DnsRecordSet
addressToRecord dnsRecordSets address =
    dnsRecordSets
        |> List.Extra.find
            (\{ records } ->
                records |> Set.toList |> List.any (\z -> z == address)
            )
