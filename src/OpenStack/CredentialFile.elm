module OpenStack.CredentialFile exposing (Kind(..), detect)

{-| Tells apart the two credential file formats a user can hand to Exosphere, by looking at the
contents rather than the file name, since browsers do not reliably report a file name we could
trust and users rename these files freely.
-}

import OpenStack.CloudsYaml
import OpenStack.OpenRc


type Kind
    = OpenRcFile
    | CloudsYamlFile
    | UnrecognizedFile


{-| OpenRC is checked first because a shell script can legitimately contain a line reading
`clouds:` at column zero, inside a here-document that writes out a clouds.yaml. A clouds.yaml
can never assign an `OS_` variable, so there is no mirror image of that ambiguity and the order
is safe.
-}
detect : String -> Kind
detect contents =
    if OpenStack.OpenRc.looksLikeOpenRc contents then
        OpenRcFile

    else if OpenStack.CloudsYaml.looksLikeCloudsYaml contents then
        CloudsYamlFile

    else
        UnrecognizedFile
