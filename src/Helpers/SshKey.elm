module Helpers.SshKey exposing (KeyTypeGuess(..), guessKeyType)

import Parser exposing ((|.))


type KeyTypeGuess
    = PublicKey
    | PrivateKey
    | Unknown


recognizedPublicKeyTypes : List String
recognizedPublicKeyTypes =
    [ "ssh-ed25519"
    , "ecdsa-sha2-nistp256"
    , "ecdsa-sha2-nistp384"
    , "ecdsa-sha2-nistp512"
    , "rsa-sha2-512"
    , "rsa-sha2-256"
    , "ssh-rsa"
    , "ssh-dss"
    , "ssh-dsa"
    ]


guessKeyType : String -> KeyTypeGuess
guessKeyType key =
    Parser.run keyTypeParser key
        |> Result.withDefault Unknown


keyTypeParser : Parser.Parser KeyTypeGuess
keyTypeParser =
    Parser.oneOf
        [ publicKeyParser
        , privateKeyParser
        , Parser.succeed Unknown
        ]


publicKeyParser : Parser.Parser KeyTypeGuess
publicKeyParser =
    Parser.succeed PublicKey
        |. Parser.oneOf (List.map Parser.keyword recognizedPublicKeyTypes)
        |. Parser.spaces
        |. base64String
        |. Parser.spaces


privateKeyParser : Parser.Parser KeyTypeGuess
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
    Parser.succeed PrivateKey
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
