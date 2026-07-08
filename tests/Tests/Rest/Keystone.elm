module Tests.Rest.Keystone exposing (ec2CredentialsDecoderSuite)

import Expect
import Json.Decode as Decode
import Rest.Keystone as Keystone
import Test exposing (Test, describe, test)


{-| A real-shape GET /v3/users/<id>/credentials/OS-EC2 list response. The OS-EC2 extension puts
`access`/`secret`/`tenant_id` as TOP-LEVEL fields on each item (not inside a JSON-string `blob`), and
the list spans ALL of the user's projects — extra fields (links, trust\_id, user\_id) must be ignored.
-}
ec2ListJson : String
ec2ListJson =
    """
    { "credentials":
        [ { "user_id": "u1"
          , "tenant_id": "proj-a"
          , "access": "AKIA1"
          , "secret": "s3cr3t1"
          , "trust_id": null
          , "links": { "self": "https://keystone.example/v3/users/u1/credentials/OS-EC2/AKIA1" }
          }
        , { "user_id": "u1"
          , "tenant_id": "proj-b"
          , "access": "AKIA2"
          , "secret": "s3cr3t2"
          , "trust_id": null
          , "links": { "self": "https://keystone.example/v3/users/u1/credentials/OS-EC2/AKIA2" }
          }
        ]
    }
    """


ec2CredentialsDecoderSuite : Test
ec2CredentialsDecoderSuite =
    describe "ec2CredentialsDecoder decodes the OS-EC2 list response"
        [ test "decodes two credentials with access/secret/tenantId, ignoring extra fields" <|
            \_ ->
                case Decode.decodeString Keystone.ec2CredentialsDecoder ec2ListJson of
                    Ok creds ->
                        Expect.equal
                            [ { access = "AKIA1", secret = "s3cr3t1", tenantId = "proj-a" }
                            , { access = "AKIA2", secret = "s3cr3t2", tenantId = "proj-b" }
                            ]
                            creds

                    Err e ->
                        Expect.fail ("expected Ok credentials, got Err: " ++ Decode.errorToString e)
        , test "decodes an empty credentials list to []" <|
            \_ ->
                case Decode.decodeString Keystone.ec2CredentialsDecoder """{ "credentials": [] }""" of
                    Ok creds ->
                        Expect.equal [] creds

                    Err e ->
                        Expect.fail ("expected Ok [], got Err: " ++ Decode.errorToString e)
        ]
