module Helpers.SshKeyTypeGuesser exposing (KeyTypeGuess(..), guessKeyType)

{-| Guess what kind of key text a user has pasted, so a form can warn before OpenStack rejects it.

Nova validates an uploaded public key with pyca/cryptography's `load_ssh_public_key`, called from
`generate_fingerprint` in nova/crypto.py (<https://opendev.org/openstack/nova/src/branch/master/nova/crypto.py>).
That loader accepts only a single-line OpenSSH public key, `<type> <base64 blob> [comment]`, and rejects leading
whitespace, other encodings (RFC 4716, PEM), and a blob that does not decode to the structure its type requires.
This guesser checks the same structure but not the key material, so a `PublicKey` guess is not proof that Nova will
accept the key.

-}

import Bitwise
import List.Extra
import Maybe.Extra
import Parser exposing ((|.))


type KeyTypeGuess
    = PublicKey String
    | PrivateKey
    | Rfc4716PublicKey
    | PemPublicKey
    | Unknown


{-| The number of length-prefixed fields in each key type's blob, including the leading type name.

Sources: RFC 4253 section 6.6 (ssh-rsa, ssh-dss), RFC 5656 section 3.1 (ecdsa-sha2-\*), RFC 8709 section 4
(ssh-ed25519), and OpenSSH's PROTOCOL.u2f (sk-\*, which append an application string). The `rsa-sha2-256` and
`rsa-sha2-512` names are signature algorithms; an RSA public key is always `ssh-rsa`.

-}
blobFieldCounts : List ( String, Int )
blobFieldCounts =
    [ ( "ssh-ed25519", 2 )
    , ( "ecdsa-sha2-nistp256", 3 )
    , ( "ecdsa-sha2-nistp384", 3 )
    , ( "ecdsa-sha2-nistp521", 3 )
    , ( "sk-ssh-ed25519@openssh.com", 3 )
    , ( "sk-ecdsa-sha2-nistp256@openssh.com", 4 )
    , ( "ssh-rsa", 3 )
    , ( "ssh-dss", 5 )
    ]


guessKeyType : String -> KeyTypeGuess
guessKeyType key =
    let
        trimmedKey =
            String.trim key
    in
    if isPrivateKey trimmedKey then
        PrivateKey

    else if String.startsWith "---- BEGIN SSH2 PUBLIC KEY" trimmedKey then
        Rfc4716PublicKey

    else if String.startsWith "-----BEGIN PUBLIC KEY" trimmedKey || String.startsWith "-----BEGIN RSA PUBLIC KEY" trimmedKey then
        PemPublicKey

    else
        openSshPublicKeyType trimmedKey
            |> Maybe.map PublicKey
            |> Maybe.withDefault Unknown



--- OpenSSH public keys


openSshPublicKeyType : String -> Maybe String
openSshPublicKeyType key =
    case String.words key of
        keyType :: blob :: _ ->
            List.Extra.find (Tuple.first >> (==) keyType) blobFieldCounts
                |> Maybe.andThen
                    (\( _, fieldCount ) ->
                        decodeBase64 blob
                            |> Maybe.andThen lengthPrefixedFields
                            |> Maybe.Extra.filter
                                (\fields ->
                                    List.length fields
                                        == fieldCount
                                        && (List.head fields == Just (List.map Char.toCode (String.toList keyType)))
                                )
                            |> Maybe.map (\_ -> keyType)
                    )

        _ ->
            Nothing


{-| Split an SSH wire-format blob into its fields, each a big-endian uint32 length followed by that many bytes.
Fails if a length overruns the blob, which is how a truncated key shows up.
-}
lengthPrefixedFields : List Int -> Maybe (List (List Int))
lengthPrefixedFields bytes =
    case bytes of
        [] ->
            Just []

        b0 :: b1 :: b2 :: b3 :: rest ->
            let
                fieldLength =
                    ((b0 * 256 + b1) * 256 + b2) * 256 + b3
            in
            if List.length rest < fieldLength then
                Nothing

            else
                lengthPrefixedFields (List.drop fieldLength rest)
                    |> Maybe.map ((::) (List.take fieldLength rest))

        _ ->
            Nothing


{-| Strict standard base64 (RFC 4648 section 4) to bytes: padded to a multiple of 4, `=` only at the end.

Exosphere's direct base64 dependency decodes to a UTF-8 string, which cannot represent arbitrary binary, and the
bytes-based `danfishgold/base64-bytes` cannot be added alongside it because both expose a module named `Base64`.

-}
decodeBase64 : String -> Maybe (List Int)
decodeBase64 encoded =
    let
        chars =
            String.toList encoded

        paddingLength =
            chars
                |> List.reverse
                |> List.Extra.takeWhile ((==) '=')
                |> List.length
    in
    if modBy 4 (List.length chars) /= 0 || paddingLength > 2 then
        Nothing

    else
        chars
            |> List.take (List.length chars - paddingLength)
            |> Maybe.Extra.combineMap sextet
            |> Maybe.andThen
                (List.Extra.greedyGroupsOf 4 >> Maybe.Extra.combineMap sextetsToBytes)
            |> Maybe.map List.concat


sextet : Char -> Maybe Int
sextet char =
    let
        code =
            Char.toCode char
    in
    if Char.isUpper char then
        Just (code - Char.toCode 'A')

    else if Char.isLower char then
        Just (code - Char.toCode 'a' + 26)

    else if Char.isDigit char then
        Just (code - Char.toCode '0' + 52)

    else if char == '+' then
        Just 62

    else if char == '/' then
        Just 63

    else
        Nothing


{-| Four sextets make three bytes; a final group of three or two (the unpadded remainder) makes two or one.
-}
sextetsToBytes : List Int -> Maybe (List Int)
sextetsToBytes sextets =
    let
        bits =
            List.foldl (\s acc -> Bitwise.or (Bitwise.shiftLeftBy 6 acc) s) 0 sextets

        byteAt shift =
            Bitwise.and 0xFF (Bitwise.shiftRightZfBy shift bits)
    in
    case List.length sextets of
        4 ->
            Just [ byteAt 16, byteAt 8, byteAt 0 ]

        3 ->
            Just [ byteAt 10, byteAt 2 ]

        2 ->
            Just [ byteAt 4 ]

        _ ->
            Nothing



--- Private keys


isPrivateKey : String -> Bool
isPrivateKey key =
    -- PuTTY's .ppk format holds the private key and starts with this header rather than a PEM label.
    String.startsWith "PuTTY-User-Key-File-" key
        || (Parser.run privateKeyParser key |> Result.withDefault False)


privateKeyParser : Parser.Parser Bool
privateKeyParser =
    let
        anyPrivatePemLabel =
            Parser.succeed ()
                |. dashes
                |. Parser.spaces
                |. Parser.oneOf
                    [ Parser.token "BEGIN"
                    , Parser.token "END"
                    ]
                |. Parser.chompUntil "PRIVATE"
                |. Parser.keyword "PRIVATE"
                |. Parser.chompUntil "-"
                |. dashes
    in
    Parser.succeed True
        |. anyPrivatePemLabel
        |. Parser.spaces
        |. base64String


dashes : Parser.Parser ()
dashes =
    Parser.succeed ()
        |. Parser.chompIf isDash
        |. Parser.chompWhile isDash


isDash : Char -> Bool
isDash c =
    c == '-'


base64String : Parser.Parser ()
base64String =
    Parser.succeed ()
        |. Parser.chompIf isBase64
        |. Parser.chompWhile isBase64


isBase64 : Char -> Bool
isBase64 c =
    Char.isAlphaNum c || (c == '+') || (c == '/') || (c == '=')
