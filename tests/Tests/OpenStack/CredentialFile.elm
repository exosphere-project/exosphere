module Tests.OpenStack.CredentialFile exposing (credentialFileSuite)

import Expect
import OpenStack.CredentialFile exposing (Kind(..))
import Test exposing (Test, describe, test)


openrcAppCredential : String
openrcAppCredential =
    """#!/usr/bin/env bash

export OS_AUTH_TYPE=v3applicationcredential
export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
export OS_APPLICATION_CREDENTIAL_ID="abcd-efgh"
export OS_APPLICATION_CREDENTIAL_SECRET="supersecret"
"""


cloudsYaml : String
cloudsYaml =
    """clouds:
  openstack:
    auth:
      auth_url: https://cell.alliance.rebel:5000
      application_credential_id: abcd-efgh
      application_credential_secret: supersecret
"""


{-| A shell script may write out a clouds.yaml, which puts a column zero `clouds:` line inside
a file that is unambiguously an OpenRC file.
-}
openrcWithCloudsYamlHereDoc : String
openrcWithCloudsYamlHereDoc =
    """#!/usr/bin/env bash

export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
export OS_APPLICATION_CREDENTIAL_ID=abcd-efgh
export OS_APPLICATION_CREDENTIAL_SECRET=supersecret

cat > ~/.config/openstack/clouds.yaml <<EOF
clouds:
  openstack:
    auth:
      auth_url: https://elsewhere:5000/v3
      application_credential_id: ijkl-mnop
      application_credential_secret: alsosecret
EOF
"""


withWindowsLineEndings : String -> String
withWindowsLineEndings =
    String.replace "\n" "\u{000D}\n"


credentialFileSuite : Test
credentialFileSuite =
    describe "detecting which credential file was supplied"
        [ test "detects OpenRC" <|
            \() ->
                OpenStack.CredentialFile.detect openrcAppCredential
                    |> Expect.equal OpenRcFile
        , test "detects OpenRC with CRLF line endings" <|
            \() ->
                OpenStack.CredentialFile.detect (withWindowsLineEndings openrcAppCredential)
                    |> Expect.equal OpenRcFile
        , test "detects OpenRC without the export keyword" <|
            \() ->
                OpenStack.CredentialFile.detect "OS_AUTH_URL=https://cell.alliance.rebel:5000/v3\n"
                    |> Expect.equal OpenRcFile
        , test "detects clouds.yaml" <|
            \() ->
                OpenStack.CredentialFile.detect cloudsYaml
                    |> Expect.equal CloudsYamlFile
        , test "detects clouds.yaml with CRLF line endings" <|
            \() ->
                OpenStack.CredentialFile.detect (withWindowsLineEndings cloudsYaml)
                    |> Expect.equal CloudsYamlFile
        , test "rejects an empty string" <|
            \() ->
                OpenStack.CredentialFile.detect ""
                    |> Expect.equal UnrecognizedFile
        , test "rejects prose" <|
            \() ->
                OpenStack.CredentialFile.detect "please find my credentials attached"
                    |> Expect.equal UnrecognizedFile
        , test "rejects a shell script that sets no OpenStack variables" <|
            \() ->
                OpenStack.CredentialFile.detect "#!/usr/bin/env bash\nexport PATH=/usr/bin\nunset OS_AUTH_URL\n"
                    |> Expect.equal UnrecognizedFile
        , test "rejects YAML that is not a clouds.yaml" <|
            \() ->
                OpenStack.CredentialFile.detect "servers:\n  one:\n    name: bespin\n"
                    |> Expect.equal UnrecognizedFile
        , test "classifies a shell script that writes a clouds.yaml as OpenRC" <|
            \() ->
                OpenStack.CredentialFile.detect openrcWithCloudsYamlHereDoc
                    |> Expect.equal OpenRcFile
        , test "rejects a private key" <|
            \() ->
                OpenStack.CredentialFile.detect "-----BEGIN OPENSSH PRIVATE KEY-----\nb3BlbnNzaC1rZXktdjEAAAAA\n-----END OPENSSH PRIVATE KEY-----\n"
                    |> Expect.equal UnrecognizedFile
        ]
