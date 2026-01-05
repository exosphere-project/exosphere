module Helpers.Cidr exposing (expandIPv6, isValidCidr)

import OpenStack.SecurityGroupRule exposing (SecurityGroupRuleEthertype(..))


isValidCidr : SecurityGroupRuleEthertype -> String -> Bool
isValidCidr etherType cidr =
    let
        parts =
            String.split "/" cidr
    in
    case parts of
        [ ipAddress, prefixLengthString ] ->
            case String.toInt prefixLengthString of
                Just prefixLength ->
                    case etherType of
                        Ipv4 ->
                            isValidIPv4 ipAddress
                                && (prefixLength >= 0)
                                && (prefixLength <= 32)

                        Ipv6 ->
                            isValidIPv6 ipAddress
                                && (prefixLength >= 0)
                                && (prefixLength <= 128)

                        UnsupportedEthertype _ ->
                            False

                Nothing ->
                    False

        _ ->
            False


isValidIPv4 : String -> Bool
isValidIPv4 ipAddress =
    let
        parts =
            String.split "." ipAddress
    in
    if List.length parts /= 4 then
        False

    else
        List.all
            (\part ->
                case String.toInt part of
                    Just n ->
                        n >= 0 && n <= 255

                    Nothing ->
                        False
            )
            parts


isValidIPv6 : String -> Bool
isValidIPv6 ipAddress =
    -- Normalise the IPv6 address to its expanded form.
    -- e.g. 2001:db8::ff00:42:8329 -> 2001:0db8:0000:0000:0000:ff00:0042:8329
    case expandIPv6 ipAddress of
        Just expandedAddress ->
            let
                groups =
                    String.split ":" expandedAddress
            in
            List.length groups == 8 && List.all isValidIPv6Group groups

        Nothing ->
            False


{-| Expand an IPv6 address to its full form.

    e.g. 2001:db8::ff00:42:8329 -> 2001:0db8:0000:0000:0000:ff00:0042:8329
    e.g. 2001:db8:0000:0000:0000:ff00:42:8329 -> 2001:0db8:0000:0000:0000:ff00:0042:8329

-}
expandIPv6 : String -> Maybe String
expandIPv6 ipAddress =
    let
        parts =
            String.split "::" ipAddress
    in
    case parts of
        -- e.g. 2001:db8::ff00:42:8329
        [ left, right ] ->
            let
                leftGroups =
                    filterEmpty (String.split ":" left)

                rightGroups =
                    filterEmpty (String.split ":" right)

                totalGroups =
                    List.length leftGroups + List.length rightGroups

                numZerosToInsert =
                    8 - totalGroups

                zeros =
                    List.repeat numZerosToInsert "0"

                expandedGroups =
                    leftGroups ++ zeros ++ rightGroups

                paddedGroups =
                    List.map padIPv6Group expandedGroups
            in
            if List.length paddedGroups == 8 && numZerosToInsert > 0 then
                Just (String.join ":" paddedGroups)

            else
                Nothing

        -- e.g. 2001:db8:0000:0000:0000:ff00:42:8329
        [ singlePart ] ->
            let
                groups =
                    filterEmpty (String.split ":" singlePart)

                paddedGroups =
                    List.map padIPv6Group groups
            in
            case List.length paddedGroups of
                8 ->
                    -- Already expanded.
                    Just (String.join ":" paddedGroups)

                _ ->
                    -- Wrong number of groups, or the input began or ended with a single ":".
                    Nothing

        _ ->
            -- Consecutive sections of zeros are replaced with two colons (::).
            -- This may only be used once in an address, as multiple use would render the address indeterminate.
            --  ref. https://en.wikipedia.org/wiki/IPv6#Address_representation
            Nothing


padIPv6Group : String -> String
padIPv6Group group =
    String.padLeft 4 '0' group


isValidIPv6Group : String -> Bool
isValidIPv6Group group =
    (String.length group <= 4)
        && (String.length group > 0)
        && String.all Char.isHexDigit group


filterEmpty : List String -> List String
filterEmpty =
    List.filter (\s -> s /= "")
