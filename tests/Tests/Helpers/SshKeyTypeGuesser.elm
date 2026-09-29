module Tests.Helpers.SshKeyTypeGuesser exposing (sshKeySuite)

import Expect
import Helpers.SshKeyTypeGuesser
import Test exposing (Test, describe, test)


sshKeySuite : Test
sshKeySuite =
    describe "Test SSH key parsing"
        [ test "private key" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.PrivateKey
                    (Helpers.SshKeyTypeGuesser.guessKeyType """-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAABlwAAAAdzc2gtcn
NhAAAAAwEAAQAAAYEA1FDXUsbyz035qXCaDg5+qzbIQO8pM+mfEJcnK96EaO7lSGSq9Rgw
eSioeg91b5LItl9bhztZDWR2QbUMXQZ7jmtxfc1zeTTgXThHNf9lppghhm8O2tMEQIJI4t
Z0saRUTx2sRcinYw80ILM4iJxOO3P/PwjfIUHgi8d7v0yEwJiZqlNseUgLrq2+UiLfNRxK
qJz4vXGKrdXzddhGTzEWyIHwEHPrdFGYqLhvMv4Kn1RdN4l4fu8Rrqg4gLSR88fHi/xLkx
RrzVKSKMhHfpPQTRwrUbBXtw8cQDUZXJBBVm3R56vieTrCABtS4j8MhS3d/Qbxgb4sQnGX
qtSVGxCmu9H+2LoctGAjPinYbS/6a519/cezVqpBIZk/pklspTu3ADw5badNKI/bJpAuUn
fwPDi5SX3RpQb0+DvRv5B841j8Ap2VrL9vloM+Adon1YZu9o/dZTg5blhGmV2gOrnSenMe
FHEuy0mpZxaMrzxfPyE6rzMcDHGRXGvX/JRHwGEVAAAFiDxvHNI8bxzSAAAAB3NzaC1yc2
EAAAGBANRQ11LG8s9N+alwmg4Ofqs2yEDvKTPpnxCXJyvehGju5UhkqvUYMHkoqHoPdW+S
yLZfW4c7WQ1kdkG1DF0Ge45rcX3Nc3k04F04RzX/ZaaYIYZvDtrTBECCSOLWdLGkVE8drE
XIp2MPNCCzOIicTjtz/z8I3yFB4IvHe79MhMCYmapTbHlIC66tvlIi3zUcSqic+L1xiq3V
83XYRk8xFsiB8BBz63RRmKi4bzL+Cp9UXTeJeH7vEa6oOIC0kfPHx4v8S5MUa81SkijIR3
6T0E0cK1GwV7cPHEA1GVyQQVZt0eer4nk6wgAbUuI/DIUt3f0G8YG+LEJxl6rUlRsQprvR
/ti6HLRgIz4p2G0v+mudff3Hs1aqQSGZP6ZJbKU7twA8OW2nTSiP2yaQLlJ38Dw4uUl90a
UG9Pg70b+QfONY/AKdlay/b5aDPgHaJ9WGbvaP3WU4OW5YRpldoDq50npzHhRxLstJqWcW
jK88Xz8hOq8zHAxxkVxr1/yUR8BhFQAAAAMBAAEAAAGAJt597RubDDS8RjblHTmuGu42jx
y5sFVO15y0gSWFnChQNYaofaJmDWhSH7aAy2JV+H1QpltJHFiOBc19a/Jp4FLvPhbE0yXJ
BYfuEYamN2+Wg6QFVi5Xku/HJDAawQLSpIFMLqJjcpEv++STrv7em6fKzOF06APFdhGZKB
Z8Hz5Qs4v+Sd3Uta/9LdBQiMqbKG9EYnpM5zJKFgL4LDtSbnbLWle+fVcK2aiaQv2bODwb
rLUwKBzgYddOMNHd/oFOQ4CmJh03HdIs5sS918G6/G1NEhYMcL0rad0yZJlG6YEgqXr8uM
aUIomEFxl9atzNyWl7oQz/Br3p7AmYahMm/ZDRQBCZnRxfNrqs/UmD8OQ6kh/icU3COTcP
zA1cCzNFGeK6fIlftm5N95G5FBSBLRnwVZRGGOfgB635V0+TD64Ybe8WnEXvxvEJKrcj0G
xl4H1EXjhvmywGIc1AFUwaGAuHbaOsePeEMt+/9Y8YtKGl0NPHaYEXc8ts5RPxmzipAAAA
wEB4SXL7Q7v4t9G50DnAOEoINIbY5Nq+rgf+0AXaIqnbB4uVftHokaXILmTTo8CSWG4/NR
WrCAGephDOZTVFtCBjBmqOjzfXxjkg5vqszNk06fS1s0fgWs1u3HmdqtNvE5nlK8QUyUBU
6WlHvXjiVV8YD9RjyzlILZK/7QPUfpBxaBDo1k063VsPMW8ozTH3u3keS7UmughTUe9zks
KVxAbGu8+6HeARWtgRD/CoVKEYtZQWejgNZVlqicM3MDIswgAAAMEA7w5KpNt1a50SafFZ
AOW5TYrd0xr2TQvS3Z6fEAgQArQHc5MIKI8REI7nkwubYANVKctQ7q0NSDmPuZUzlZ5RyH
rUH6CLQSG2Mo3qtFVmINU89jzzXfung4gxTBeKihoJeF8LQ3PdodiIzZDNocT0+Iu7ZaGS
POC/NIoztX8ssWO/+Km+PvRT3sxQnpotAsvQ2IqovVZw2eeXfBMBclR+e3eAZiMQS0TK1V
r+kBqcDf1ScgWn8+TgGaRbIpFrd0svAAAAwQDjXVjSf4TDEB9pHZYaWDbfTNJvtYuaB3cm
6rXVTvpC3mbBLGSgOsHEoEqxh8EbuwOXzc4ubcOeUnl/ky6/G65sEZdaBqC7yiILRSLaCx
K50NfJ03QzOeW6CH/tGJNcZPfNFaE6oiBJAmtFlm3WhFrcrh0CcciT7uKEc4FVy0cUMIx6
c2ysNqUmsRQkCyNqTT0D9wUK9A01KFL8RnDPu1Qp4MkOIH+cp0LNOJJRgdDEnQF94TC6sv
45trPDE9R+dvsAAAAOY21hcnRAdGhpbmtwYWQBAgMEBQ==
-----END OPENSSH PRIVATE KEY-----""")
        , test "public key" <|
            \_ ->
                Expect.equal (Helpers.SshKeyTypeGuesser.PublicKey "ssh-rsa")
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDUUNdSxvLPTfmpcJoODn6rNshA7ykz6Z8Qlycr3oRo7uVIZKr1GDB5KKh6D3Vvksi2X1uHO1kNZHZBtQxdBnuOa3F9zXN5NOBdOEc1/2WmmCGGbw7a0wRAgkji1nSxpFRPHaxFyKdjDzQgsziInE47c/8/CN8hQeCLx3u/TITAmJmqU2x5SAuurb5SIt81HEqonPi9cYqt1fN12EZPMRbIgfAQc+t0UZiouG8y/gqfVF03iXh+7xGuqDiAtJHzx8eL/EuTFGvNUpIoyEd+k9BNHCtRsFe3DxxANRlckEFWbdHnq+J5OsIAG1LiPwyFLd39BvGBvixCcZeq1JUbEKa70f7Yuhy0YCM+KdhtL/prnX39x7NWqkEhmT+mSWylO7cAPDltp00oj9smkC5Sd/A8OLlJfdGlBvT4O9G/kHzjWPwCnZWsv2+Wgz4B2ifVhm72j91lODluWEaZXaA6udJ6cx4UcS7LSalnFoyvPF8/ITqvMxwMcZFca9f8lEfAYRU= cmart@foobar""")
        , test "public key with the word 'private' in it" <|
            \_ ->
                Expect.equal (Helpers.SshKeyTypeGuesser.PublicKey "ssh-rsa")
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDUUNdSxvLPTfmpcJoODn6rNshA7ykz6Z8Qlycr3oRo7uVIZKr1GDB5KKh6D3Vvksi2X1uHO1kNZHZBtQxdBnuOa3F9zXN5NOBdOEc1/2WmmCGGbw7a0wRAgkji1nSxpFRPHaxFyKdjDzQgsziInE47c/8/CN8hQeCLx3u/TITAmJmqU2x5SprivateIt81HEqonPi9cYqt1fN12EZPMRbIgfAQc+t0UZiouG8y/gqfVF03iXh+7xGuqDiAtJHzx8eL/EuTFGvNUpIoyEd+k9BNHCtRsFe3DxxANRlckEFWbdHnq+J5OsIAG1LiPwyFLd39BvGBvixCcZeq1JUbEKa70f7Yuhy0YCM+KdhtL/prnX39x7NWqkEhmT+mSWylO7cAPDltp00oj9smkC5Sd/A8OLlJfdGlBvT4O9G/kHzjWPwCnZWsv2+Wgz4B2ifVhm72j91lODluWEaZXaA6udJ6cx4UcS7LSalnFoyvPF8/ITqvMxwMcZFca9f8lEfAYRU= cmart@privateer""")
        , test "nistp521 public key" <|
            \_ ->
                Expect.equal (Helpers.SshKeyTypeGuesser.PublicKey "ecdsa-sha2-nistp521")
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ecdsa-sha2-nistp521 AAAAE2VjZHNhLXNoYTItbmlzdHA1MjEAAAAIbmlzdHA1MjEAAACFBACIvvJrc7uVrOhEPAhan206+Ibf+NAMBTMecoGL6uXmNX0ngHIfu3p/p/XQMSlV+1iu5jXMLWpDTR5woqdPRtRShgH90hpyZM60hPuMYHIm4UroKsQa88uLm4tYwBunG6KikTry7lvKOhemfZHx+y3wAq+P0P6s+D3wEkvplnol/g1MgA== user@example""")
        , test "ed25519 public key" <|
            \_ ->
                Expect.equal (Helpers.SshKeyTypeGuesser.PublicKey "ssh-ed25519")
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF/pPxwDo5JSrwE0yheNl2DQFPHRJRnVj3/gTv1I+7xO user@example""")
        , test "nistp256 public key" <|
            \_ ->
                Expect.equal (Helpers.SshKeyTypeGuesser.PublicKey "ecdsa-sha2-nistp256")
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBNjyihbnQbTuKntFhU9ieZa9+2tQ1ZPVuz6xqs7VUxmr9dxoAqCS2aGCXHsNEs8y1NZI48o3dzFXm5Dvom0M+08= user@example""")
        , test "security key ed25519 public key" <|
            \_ ->
                Expect.equal (Helpers.SshKeyTypeGuesser.PublicKey "sk-ssh-ed25519@openssh.com")
                    (Helpers.SshKeyTypeGuesser.guessKeyType """sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIJPhniyH8XF5f5JDga75J3K9tLbwETdghRsr3IV6h0IZAAAABHNzaDo= user@example""")
        , test "security key nistp256 public key" <|
            \_ ->
                Expect.equal (Helpers.SshKeyTypeGuesser.PublicKey "sk-ecdsa-sha2-nistp256@openssh.com")
                    (Helpers.SshKeyTypeGuesser.guessKeyType """sk-ecdsa-sha2-nistp256@openssh.com AAAAInNrLWVjZHNhLXNoYTItbmlzdHAyNTZAb3BlbnNzaC5jb20AAAAIbmlzdHAyNTYAAABBBAIKLdaqEDMe3kGmZtgh+iZtK81s2jiZFXja7zpxuXfEUAf7tEwULd9q+9gE7QZhqTwHNARBkb8MroCnSyPYVwMAAAAEc3NoOg== user@example""")
        , test "surrounding whitespace, which is trimmed on submit" <|
            \_ ->
                Expect.equal (Helpers.SshKeyTypeGuesser.PublicKey "ssh-ed25519")
                    (Helpers.SshKeyTypeGuesser.guessKeyType """
  ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF/pPxwDo5JSrwE0yheNl2DQFPHRJRnVj3/gTv1I+7xO user@example
""")
        , test "public key without a comment" <|
            \_ ->
                Expect.equal (Helpers.SshKeyTypeGuesser.PublicKey "ssh-ed25519")
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF/pPxwDo5JSrwE0yheNl2DQFPHRJRnVj3/gTv1I+7xO""")
        , test "truncated blob" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.Unknown
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF/pPxwDo5JSrwE0yheNl2DQFPHRJRnVj3/gTv1I""")
        , test "blob wrapped onto several lines" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.Unknown
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJSB6X792i/rzUMfcBFq7g92hdKZEy0F/F5YOWm8
wfEeP9d7fxf8ZC7moPJ20h+cTHcCKpre4Tbp4qOw9Q8liwR4iby8avVqbOA1pddi+ja1ejr01aKyFlt4S8GHhisBiZYl6N3j0YF3qrqqVzYOpvXGbZ7HklSNoypz0UKuzwcS2kM6r46J+Xe5CV2zB6FDmjvltkxvamaho7AHuwD+aVY1yrzfmdYMhHhuJJa6+S/KxpPotHQJLyFRXWjyVZWjJ9T+o0bKFfQAcs4VvN0qhLuilCbiHmmlK3XNVumWNoVUdDueSET1qtZr05V91qrMfM6+4esHztJ18nFkrzg9nj""")
        , test "blob of another key type" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.Unknown
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-ed25519 AAAAB3NzaC1yc2EAAAADAQABAAABAQDJSB6X792i/rzUMfcBFq7g92hdKZEy0F/F5YOWm8wfEeP9d7fxf8ZC7moPJ20h+cTHcCKpre4Tbp4qOw9Q8liwR4iby8avVqbOA1pddi+ja1ejr01aKyFlt4S8GHhisBiZYl6N3j0YF3qrqqVzYOpvXGbZ7HklSNoypz0UKuzwcS2kM6r46J+Xe5CV2zB6FDmjvltkxvamaho7AHuwD+aVY1yrzfmdYMhHhuJJa6+S/KxpPotHQJLyFRXWjyVZWjJ9T+o0bKFfQAcs4VvN0qhLuilCbiHmmlK3XNVumWNoVUdDueSET1qtZr05V91qrMfM6+4esHztJ18nFkrzg9nj""")
        , test "signature algorithm name instead of key type" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.Unknown
                    (Helpers.SshKeyTypeGuesser.guessKeyType """rsa-sha2-512 AAAAB3NzaC1yc2EAAAADAQABAAABAQDJSB6X792i/rzUMfcBFq7g92hdKZEy0F/F5YOWm8wfEeP9d7fxf8ZC7moPJ20h+cTHcCKpre4Tbp4qOw9Q8liwR4iby8avVqbOA1pddi+ja1ejr01aKyFlt4S8GHhisBiZYl6N3j0YF3qrqqVzYOpvXGbZ7HklSNoypz0UKuzwcS2kM6r46J+Xe5CV2zB6FDmjvltkxvamaho7AHuwD+aVY1yrzfmdYMhHhuJJa6+S/KxpPotHQJLyFRXWjyVZWjJ9T+o0bKFfQAcs4VvN0qhLuilCbiHmmlK3XNVumWNoVUdDueSET1qtZr05V91qrMfM6+4esHztJ18nFkrzg9nj""")
        , test "known key type with a non-base64 blob" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.Unknown
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-ed25519 not*base64""")
        , test "known key type with a short blob" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.Unknown
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-rsa x""")
        , test "RFC 4716 public key" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.Rfc4716PublicKey
                    (Helpers.SshKeyTypeGuesser.guessKeyType """---- BEGIN SSH2 PUBLIC KEY ----
Comment: "2048-bit RSA, user@example"
AAAAB3NzaC1yc2EAAAADAQABAAABAQDJSB6X792i/rzUMfcBFq7g92hdKZEy0F/F5YOWm8
wfEeP9d7fxf8ZC7moPJ20h+cTHcCKpre4Tbp4qOw9Q8liwR4iby8avVqbOA1pddi+ja1ej
r01aKyFlt4S8GHhisBiZYl6N3j0YF3qrqqVzYOpvXGbZ7HklSNoypz0UKuzwcS2kM6r46J
+Xe5CV2zB6FDmjvltkxvamaho7AHuwD+aVY1yrzfmdYMhHhuJJa6+S/KxpPotHQJLyFRXW
jyVZWjJ9T+o0bKFfQAcs4VvN0qhLuilCbiHmmlK3XNVumWNoVUdDueSET1qtZr05V91qrM
fM6+4esHztJ18nFkrzg9nj
---- END SSH2 PUBLIC KEY ----""")
        , test "PKCS8 PEM public key" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.PemPublicKey
                    (Helpers.SshKeyTypeGuesser.guessKeyType """-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAyUgel+/dov681DH3ARau
4PdoXSmRMtBfxeWDlpvMHxHj/Xe38X/GQu5qDydtIfnEx3Aiqa3uE26eKjsPUPJY
sEeIm8vGr1amzgNaXXYvo2tXo69NWishZbeEvBh4YrAYmWJejd49GBd6q6qlc2Dq
b1xm2ex5JUjaMqc9FCrs8HEtpDOq+Oifl3uQldswehQ5o75bZMb2pmoaOwB7sA/m
lWNcq835nWDIR4biSWuvkvysaT6LR0CS8hUV1o8lWVoyfU/qNGyhX0AHLOFbzdKo
S7opQm4h5ppSt1zVbpljaFVHQ7nkhE9arWa9OVfdaqzHzOvuHrB87SdfJxZK84PZ
4wIDAQAB
-----END PUBLIC KEY-----""")
        , test "PKCS1 PEM RSA public key" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.PemPublicKey
                    (Helpers.SshKeyTypeGuesser.guessKeyType """-----BEGIN RSA PUBLIC KEY-----
MIIBCgKCAQEAyUgel+/dov681DH3ARau4PdoXSmRMtBfxeWDlpvMHxHj/Xe38X/G
Qu5qDydtIfnEx3Aiqa3uE26eKjsPUPJYsEeIm8vGr1amzgNaXXYvo2tXo69NWish
ZbeEvBh4YrAYmWJejd49GBd6q6qlc2Dqb1xm2ex5JUjaMqc9FCrs8HEtpDOq+Oif
l3uQldswehQ5o75bZMb2pmoaOwB7sA/mlWNcq835nWDIR4biSWuvkvysaT6LR0CS
8hUV1o8lWVoyfU/qNGyhX0AHLOFbzdKoS7opQm4h5ppSt1zVbpljaFVHQ7nkhE9a
rWa9OVfdaqzHzOvuHrB87SdfJxZK84PZ4wIDAQAB
-----END RSA PUBLIC KEY-----""")
        , test "PuTTY private key" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.PrivateKey
                    (Helpers.SshKeyTypeGuesser.guessKeyType """PuTTY-User-Key-File-3: ssh-ed25519
Encryption: none
Comment: user@example
Public-Lines: 2
AAAAC3NzaC1lZDI1NTE5AAAAIF/pPxwDo5JSrwE0yheNl2DQFPHRJRnVj3/gTv1I+7xO
Private-Lines: 1
AAAAIDvw0QrzJ3xT9pQ2c3zZ6oQh8k0vQ0y7tGmH0V9QJv1a
Private-MAC: 0000000000000000000000000000000000000000000000000000000000000000""")
        , test "not an SSH key at all" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.Unknown
                    (Helpers.SshKeyTypeGuesser.guessKeyType """this is not an ssh key""")
        ]
