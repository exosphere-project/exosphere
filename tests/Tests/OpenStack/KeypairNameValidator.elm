module Tests.OpenStack.KeypairNameValidator exposing (keypairNameValidatorSuite)

import Expect
import OpenStack.KeypairNameValidator exposing (keypairNameValidator)
import Test exposing (Test, describe, test)


keypairNameValidatorSuite : Test
keypairNameValidatorSuite =
    describe "Keypair name validation"
        [ test "accepts letters, digits, spaces, hyphens and underscores" <|
            \_ ->
                keypairNameValidator "My key_2026-09"
                    |> Expect.equal []
        , test "ignores surrounding whitespace, which is trimmed on submit" <|
            \_ ->
                keypairNameValidator "  my key  "
                    |> Expect.equal []
        , test "requires a name" <|
            \_ ->
                keypairNameValidator ""
                    |> Expect.equal [ "Name is required" ]
        , test "requires more than whitespace" <|
            \_ ->
                keypairNameValidator "   "
                    |> Expect.equal [ "Name is required" ]
        , test "lists each disallowed character once, in order of appearance" <|
            \_ ->
                keypairNameValidator "me@example.com@work"
                    |> Expect.equal [ "Name cannot contain \"@\", \".\"" ]
        , test "rejects non-ASCII letters" <|
            \_ ->
                keypairNameValidator "clé"
                    |> Expect.equal [ "Name cannot contain \"é\"" ]
        , test "names invisible characters that would look like an allowed space" <|
            \_ ->
                keypairNameValidator "my\tkey\u{00A0}name\u{2003}2"
                    |> Expect.equal [ "Name cannot contain tabs, non-standard spaces" ]
        , test "accepts 255 characters" <|
            \_ ->
                keypairNameValidator (String.repeat 255 "a")
                    |> Expect.equal []
        , test "rejects more than 255 characters" <|
            \_ ->
                keypairNameValidator (String.repeat 256 "a")
                    |> Expect.equal [ "Name must be 255 characters or fewer" ]
        , test "reports every problem" <|
            \_ ->
                keypairNameValidator (String.repeat 256 ".")
                    |> Expect.equal [ "Name must be 255 characters or fewer", "Name cannot contain \".\"" ]
        ]
