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


{-| A file that decodes as a clouds.yaml with a credential in it is a clouds.yaml, whatever else
it may contain. Nothing weaker settles the question: an `OS_` assignment can appear inside a
clouds.yaml, in a comment or a block of text, and a `clouds:` line can appear inside a shell
script, in a here-document that writes one out. Only once the strong answer comes back empty do
the two heuristics run, and then only to choose which of the two files the user was probably
trying to give us, so that the message names the right one.
-}
detect : String -> Kind
detect contents =
    if cloudsYamlIsUsable contents then
        CloudsYamlFile

    else if OpenStack.OpenRc.looksLikeOpenRc contents then
        OpenRcFile

    else if OpenStack.CloudsYaml.looksLikeCloudsYaml contents then
        CloudsYamlFile

    else
        UnrecognizedFile


cloudsYamlIsUsable : String -> Bool
cloudsYamlIsUsable contents =
    case OpenStack.CloudsYaml.parse contents of
        Ok _ ->
            True

        Err _ ->
            False
