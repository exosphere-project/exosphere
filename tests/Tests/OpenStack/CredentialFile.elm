module Tests.OpenStack.CredentialFile exposing (credentialFileSuite, redactSecretsSuite)

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


{-| A clouds.yaml may quote an OpenRC file back at us, in a comment or in a block of text, which
puts an `OS_` assignment inside a file that is unambiguously a clouds.yaml.
-}
cloudsYamlQuotingOpenRc : String
cloudsYamlQuotingOpenRc =
    """# Generated from openrc.sh, which had:
#   export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
clouds:
  cloud_riders:
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      application_credential_id: abcd-efgh
      application_credential_secret: supersecret
    notes: |
      OS_PROJECT_NAME=cloud-riders
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
        , test "classifies a clouds.yaml that quotes an OpenRC file as clouds.yaml" <|
            \() ->
                OpenStack.CredentialFile.detect cloudsYamlQuotingOpenRc
                    |> Expect.equal CloudsYamlFile
        , test "rejects a private key" <|
            \() ->
                OpenStack.CredentialFile.detect "-----BEGIN OPENSSH PRIVATE KEY-----\nb3BlbnNzaC1rZXktdjEAAAAA\n-----END OPENSSH PRIVATE KEY-----\n"
                    |> Expect.equal UnrecognizedFile
        ]


redactSecretsSuite : Test
redactSecretsSuite =
    describe "hiding the secrets in a credential file"
        [ test "hides an openrc application credential secret and leaves its ID showing" <|
            \() ->
                OpenStack.CredentialFile.redactSecrets openrcAppCredential
                    |> Expect.equal
                        """#!/usr/bin/env bash

export OS_AUTH_TYPE=v3applicationcredential
export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
export OS_APPLICATION_CREDENTIAL_ID="abcd-efgh"
export OS_APPLICATION_CREDENTIAL_SECRET=••••••••
"""
        , test "hides an openrc password without showing how long it is" <|
            \() ->
                [ "export OS_PASSWORD=\"hunter2\"", "export OS_PASSWORD=\"correct-horse-battery-staple\"" ]
                    |> List.map OpenStack.CredentialFile.redactSecrets
                    |> Expect.equal [ "export OS_PASSWORD=••••••••", "export OS_PASSWORD=••••••••" ]
        , test "hides a clouds.yaml secret nested under auth and keeps its indentation" <|
            \() ->
                OpenStack.CredentialFile.redactSecrets cloudsYaml
                    |> Expect.equal
                        """clouds:
  openstack:
    auth:
      auth_url: https://cell.alliance.rebel:5000
      application_credential_id: abcd-efgh
      application_credential_secret: ••••••••
"""
        , test "hides a PowerShell password without stopping at $env:" <|
            \() ->
                OpenStack.CredentialFile.redactSecrets "$env:OS_PASSWORD = \"hunter2\""
                    |> Expect.equal "$env:OS_PASSWORD = ••••••••"
        , test "leaves a blank value as it is, there being nothing to hide" <|
            \() ->
                [ "export OS_PASSWORD=\nexport OS_AUTH_URL=https://cell.alliance.rebel:5000/v3\n"
                , "    auth:\n      application_credential_secret:   \n    region_name: IU\n"
                ]
                    |> List.map OpenStack.CredentialFile.redactSecrets
                    |> Expect.equal
                        [ "export OS_PASSWORD=\nexport OS_AUTH_URL=https://cell.alliance.rebel:5000/v3\n"
                        , "    auth:\n      application_credential_secret:   \n    region_name: IU\n"
                        ]
        , test "hides a YAML value written below its key, up to the next key as deep as it" <|
            \() ->
                [ "      password: |\n        hunter2\n\n        and more\n      username: enfysnest\n"
                , "      password: |-\n        hunter2\n      username: enfysnest\n"
                , "      password: >\n        hunter2\n      username: enfysnest\n"
                , "      application_credential_secret:\n        hunter2\n      application_credential_id: abcd-efgh\n"
                ]
                    |> List.map OpenStack.CredentialFile.redactSecrets
                    |> Expect.equal
                        [ "      password: |\n        ••••••••\n\n        ••••••••\n      username: enfysnest\n"
                        , "      password: |-\n        ••••••••\n      username: enfysnest\n"
                        , "      password: >\n        ••••••••\n      username: enfysnest\n"
                        , "      application_credential_secret:\n        ••••••••\n      application_credential_id: abcd-efgh\n"
                        ]
        , test "knows a YAML value written below its key is a secret" <|
            \() ->
                [ "password: |\n  hunter2\n", "password: |-\n  hunter2\n", "password: >\n  hunter2\n", "password:\n  hunter2\n" ]
                    |> List.map OpenStack.CredentialFile.hasSecrets
                    |> Expect.equal [ True, True, True, True ]
        , test "leaves text without a secret as it is" <|
            \() ->
                "export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3\nexport OS_REGION_NAME=\"IU\"\n"
                    |> OpenStack.CredentialFile.redactSecrets
                    |> Expect.equal "export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3\nexport OS_REGION_NAME=\"IU\"\n"
        , test "keeps every line" <|
            \() ->
                [ openrcAppCredential, cloudsYaml, openrcWithCloudsYamlHereDoc ]
                    |> List.map (OpenStack.CredentialFile.redactSecrets >> String.lines >> List.length)
                    |> Expect.equal (List.map (String.lines >> List.length) [ openrcAppCredential, cloudsYaml, openrcWithCloudsYamlHereDoc ])
        , test "knows a file with a secret in it has secrets" <|
            \() ->
                [ openrcAppCredential, cloudsYaml ]
                    |> List.map OpenStack.CredentialFile.hasSecrets
                    |> Expect.equal [ True, True ]
        , test "knows a file without a secret, or with only a blank one, has none" <|
            \() ->
                [ "export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3\n", "export OS_PASSWORD=\n", "" ]
                    |> List.map OpenStack.CredentialFile.hasSecrets
                    |> Expect.equal [ False, False, False ]
        ]
