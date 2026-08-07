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


detect : String -> Kind
detect contents =
    if OpenStack.CloudsYaml.looksLikeCloudsYaml contents then
        CloudsYamlFile

    else if OpenStack.OpenRc.looksLikeOpenRc contents then
        OpenRcFile

    else
        UnrecognizedFile
