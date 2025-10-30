module OpenStack.DnsRecordSet exposing
    ( DnsRecordSet
    , DnsZone
    , RecordType(..)
    , fromStringToRecordType
    , lookupRecordsByAddress
    , recordTypeToString
    )

import OpenStack.HelperTypes
import Set


type alias DnsRecordSet =
    { zone_id : OpenStack.HelperTypes.Uuid
    , zone_name : String
    , id : OpenStack.HelperTypes.Uuid
    , name : String
    , type_ : RecordType
    , ttl : Maybe Int
    , records : Set.Set String
    }


type alias DnsZone =
    { zone_id : OpenStack.HelperTypes.Uuid
    , zone_name : String
    }


type RecordType
    = ARecord
    | PTRRecord
    | SOARecord
    | NSRecord
    | CNAMERecord
    | TXTRecord
    | UnsupportedRecordType String


fromStringToRecordType : String -> RecordType
fromStringToRecordType record =
    case record of
        "A" ->
            ARecord

        "PTR" ->
            PTRRecord

        "SOA" ->
            SOARecord

        "NS" ->
            NSRecord

        "CNAME" ->
            CNAMERecord

        "TXT" ->
            TXTRecord

        _ ->
            UnsupportedRecordType record


recordTypeToString : RecordType -> String
recordTypeToString type_ =
    case type_ of
        ARecord ->
            "A"

        PTRRecord ->
            "PTR"

        SOARecord ->
            "SOA"

        NSRecord ->
            "NS"

        CNAMERecord ->
            "CNAME"

        TXTRecord ->
            "TXT"

        UnsupportedRecordType str ->
            str


lookupRecordsByAddress : List DnsRecordSet -> String -> List DnsRecordSet
lookupRecordsByAddress dnsRecordSets address =
    dnsRecordSets
        |> List.filter (.records >> Set.member address)
