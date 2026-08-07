module OpenStack.CloudsYaml exposing (CloudEntry, Error(..), looksLikeCloudsYaml, parse)

{-| Reads the subset of `clouds.yaml` that carries OpenStack application credentials.

Two shapes are supported, and they are really the same shape:

  - the file Exosphere itself generates (see `Helpers.Credentials.getCloudsYaml`), which may
    contain one entry per project
  - the file Horizon offers for download when you create an application credential

Anything else fails closed. A cloud entry without an application credential (for example a
password based entry) is ignored rather than logged in with, because this flow only knows how
to authenticate with application credentials.

-}

import Dict
import Helpers.String exposing (normalizeLineEndings)
import OpenStack.Types as OSTypes
import Yaml.Decode as YD


{-| One usable cloud from a `clouds.yaml` file, named by its key under `clouds:`.
-}
type alias CloudEntry =
    { name : String
    , authUrl : OSTypes.KeystoneUrl
    , appCredential : OSTypes.ApplicationCredential
    , regionName : Maybe String
    }


type Error
    = NotCloudsYaml
    | NoAppCredentials


{-| A cheap check for whether some text is worth handing to `parse`, used to tell a
`clouds.yaml` apart from an OpenRC file before either is parsed.
-}
looksLikeCloudsYaml : String -> Bool
looksLikeCloudsYaml contents =
    contents
        |> normalizeLineEndings
        |> String.lines
        |> List.any isTopLevelCloudsKey


isTopLevelCloudsKey : String -> Bool
isTopLevelCloudsKey line =
    let
        beforeComment : String
        beforeComment =
            line |> String.split "#" |> List.head |> Maybe.withDefault ""
    in
    -- A top level key carries no indentation and opens a nested block, so the line holds
    -- `clouds:` and nothing else.
    String.trimRight beforeComment == "clouds:"


{-| Returns every cloud in the file that carries a complete application credential, or an
error when there is no such cloud.
-}
parse : String -> Result Error (List CloudEntry)
parse contents =
    case YD.fromString cloudsDecoder (normalizeLineEndings contents) of
        Err _ ->
            Err NotCloudsYaml

        Ok entries ->
            case List.filterMap identity entries of
                [] ->
                    Err NoAppCredentials

                usableEntries ->
                    Ok usableEntries


cloudsDecoder : YD.Decoder (List (Maybe CloudEntry))
cloudsDecoder =
    YD.field "clouds" (YD.dict cloudDecoder)
        |> YD.map Dict.toList
        |> YD.map (List.map (\( name, toEntry ) -> toEntry name))


{-| Decodes one entry under `clouds:` into a function awaiting its own key, because
`Yaml.Decode.dict` does not hand the key to the value decoder.
-}
cloudDecoder : YD.Decoder (String -> Maybe CloudEntry)
cloudDecoder =
    YD.oneOf
        [ YD.map3
            (\authUrl appCredential regionName name ->
                Just (CloudEntry name authUrl appCredential regionName)
            )
            (YD.at [ "auth", "auth_url" ] nonBlankString)
            (YD.map2 OSTypes.ApplicationCredential
                (YD.at [ "auth", "application_credential_id" ] nonBlankString)
                (YD.at [ "auth", "application_credential_secret" ] nonBlankString)
            )
            regionNameDecoder
        , YD.succeed (\_ -> Nothing)
        ]


{-| `region_name` is optional, and Exosphere writes it alongside `auth` while some tools write
it inside `auth`, so look in both places.
-}
regionNameDecoder : YD.Decoder (Maybe String)
regionNameDecoder =
    YD.oneOf
        [ YD.field "region_name" nonBlankString |> YD.map Just
        , YD.at [ "auth", "region_name" ] nonBlankString |> YD.map Just
        , YD.succeed Nothing
        ]


nonBlankString : YD.Decoder String
nonBlankString =
    YD.string
        |> YD.andThen
            (\rawValue ->
                let
                    value : String
                    value =
                        String.trim rawValue
                in
                if String.isEmpty value then
                    YD.fail "value is blank"

                else
                    YD.succeed value
            )
