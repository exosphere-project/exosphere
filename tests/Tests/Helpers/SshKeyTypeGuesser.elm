module Tests.Helpers.SshKeyTypeGuesser exposing (sshKeySuite)

-- Test related Modules
-- Exosphere Modules Under Test

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
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
-----END OPENSSH PRIVATE KEY-----""")
        , test "public key" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.PublicKey
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDUUNdSxvLPTfmpcJoODn6rNshA7ykz6Z8Qlycr3oRo7uVIZKr1GDB5KKh6D3Vvksi2X1uHO1kNZHZBtQxdBnuOa3F9zXN5NOBdOEc1/2WmmCGGbw7a0wRAgkji1nSxpFRPHaxFyKdjDzQgsziInE47c/8/CN8hQeCLx3u/TITAmJmqU2x5SAuurb5SIt81HEqonPi9cYqt1fN12EZPMRbIgfAQc+t0UZiouG8y/gqfVF03iXh+7xGuqDiAtJHzx8eL/EuTFGvNUpIoyEd+k9BNHCtRsFe3DxxANRlckEFWbdHnq+J5OsIAG1LiPwyFLd39BvGBvixCcZeq1JUbEKa70f7Yuhy0YCM+KdhtL/prnX39x7NWqkEhmT+mSWylO7cAPDltp00oj9smkC5Sd/A8OLlJfdGlBvT4O9G/kHzjWPwCnZWsv2+Wgz4B2ifVhm72j91lODluWEaZXaA6udJ6cx4UcS7LSalnFoyvPF8/ITqvMxwMcZFca9f8lEfAYRU= cmart@foobar""")
        , test "public key with the word 'private' in it" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.PublicKey
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDUUNdSxvLPTfmpcJoODn6rNshA7ykz6Z8Qlycr3oRo7uVIZKr1GDB5KKh6D3Vvksi2X1uHO1kNZHZBtQxdBnuOa3F9zXN5NOBdOEc1/2WmmCGGbw7a0wRAgkji1nSxpFRPHaxFyKdjDzQgsziInE47c/8/CN8hQeCLx3u/TITAmJmqU2x5SprivateIt81HEqonPi9cYqt1fN12EZPMRbIgfAQc+t0UZiouG8y/gqfVF03iXh+7xGuqDiAtJHzx8eL/EuTFGvNUpIoyEd+k9BNHCtRsFe3DxxANRlckEFWbdHnq+J5OsIAG1LiPwyFLd39BvGBvixCcZeq1JUbEKa70f7Yuhy0YCM+KdhtL/prnX39x7NWqkEhmT+mSWylO7cAPDltp00oj9smkC5Sd/A8OLlJfdGlBvT4O9G/kHzjWPwCnZWsv2+Wgz4B2ifVhm72j91lODluWEaZXaA6udJ6cx4UcS7LSalnFoyvPF8/ITqvMxwMcZFca9f8lEfAYRU= cmart@privateer""")
        , test "not an SSH key at all" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.Unknown
                    (Helpers.SshKeyTypeGuesser.guessKeyType """this is not an ssh key""")
        ]
