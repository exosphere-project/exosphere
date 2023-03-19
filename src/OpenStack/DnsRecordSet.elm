module OpenStack.DnsRecordSet exposing (DnsRecordSet, fromStringToRecordType)

import OpenStack.HelperTypes
import Set


type alias DnsRecordSet =
    { id : OpenStack.HelperTypes.Uuid
    , name : String
    , type_ : RecordType
    , ttl : Maybe String
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
