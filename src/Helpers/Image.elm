module Helpers.Image exposing (ImageOperatingSystem, detectImageOperatingSystem, detectOperatingSystem, guessOsDefaultUsername)

import List.Extra
import OpenStack.Types as OSTypes


type alias ImageOperatingSystem =
    { distribution : String
    , version : Maybe String
    , supported : Maybe Bool
    }


supportedDistributions :
    List
        { identifier : String
        , friendlyName : String
        , supportedVersions : List String
        }
supportedDistributions =
    {- A list of known distributions and their supported versions

       These distribution common names come from the OpenStack Glance documentation
       https://docs.openstack.org/glance/latest/admin/useful-image-properties.html

       Leaving an empty list of versions will allow detection of the
       distribution leaving the `supported` field empty
    -}
    [ { identifier = "debian", friendlyName = "Debian", supportedVersions = [ "11" ] }
    , { identifier = "ubuntu", friendlyName = "Ubuntu", supportedVersions = [ "20.04", "22.04", "24.04", "24.10" ] }
    , { identifier = "rocky", friendlyName = "Rocky Linux", supportedVersions = [ "8", "9" ] }

    -- Images removed from JS2, but the code should still function
    , { identifier = "alma", friendlyName = "AlmaLinux", supportedVersions = [ "8", "9" ] }

    -- Technically may still work, but we don't officially support any versions
    , { identifier = "centos", friendlyName = "CentOS", supportedVersions = [] }
    ]


explicitlyUnsupportedDistributions : List String
explicitlyUnsupportedDistributions =
    [ "Windows" ]


{-| Given an `os_distro`, guess the default SSH username for that OS.

    - List of distros: https://docs.openstack.org/glance/latest/admin/useful-image-properties.html
    - Default usernames: https://docs.openstack.org/image-guide/obtain-images.html

In general, the OpenStack image distro identifier is the same as the default username but there are exceptions.

-}
guessOsDefaultUsername : Maybe String -> Maybe String -> Maybe String
guessOsDefaultUsername osDistro maybeOsVersion =
    osDistro
        |> Maybe.andThen
            (\osDistro_ ->
                case String.toLower osDistro_ of
                    "debian" ->
                        Just "debian"

                    "ubuntu" ->
                        Just "ubuntu"

                    "rocky" ->
                        Just "rocky"

                    "almalinux" ->
                        Just "almalinux"

                    "centos" ->
                        case maybeOsVersion of
                            Just versionStr ->
                                [ "7", "8" ]
                                    |> List.any (\v -> String.startsWith v versionStr)
                                    |> (\isOldVersion ->
                                            if isOldVersion then
                                                Just "centos"

                                            else
                                                Just "cloud-user"
                                       )

                            -- CentOS 9 & later
                            Nothing ->
                                Just "cloud-user"

                    "fedora" ->
                        Just "fedora"

                    "rhel" ->
                        Just "cloud-user"

                    "alpine" ->
                        Just "alpine"

                    "arch" ->
                        Just "arch"

                    "opensuse" ->
                        Just "opensuse"

                    "kali" ->
                        Just "kali"

                    "cirros" ->
                        Just "cirros"

                    -- BSDs
                    "freebsd" ->
                        Just "freebsd"

                    "openbsd" ->
                        Just "openbsd"

                    "netbsd" ->
                        Just "netbsd"

                    -- Microsoft
                    "msdos" ->
                        Nothing

                    "windows" ->
                        Nothing

                    -- Other
                    _ ->
                        Nothing
            )


detectOperatingSystem : String -> Maybe String -> Maybe String -> Maybe ImageOperatingSystem
detectOperatingSystem imageName maybeOsDistro maybeOsVersion =
    let
        stringSimilar : String -> String -> Bool
        stringSimilar left right =
            String.toLower left == String.toLower right

        stringContainsSimilar : String -> String -> Bool
        stringContainsSimilar needle haystack =
            String.contains
                (String.toLower needle)
                (String.toLower haystack)

        imageIsLikelyToBe : String -> Bool
        imageIsLikelyToBe name =
            maybeOsDistro
                |> Maybe.map (stringSimilar name)
                |> Maybe.withDefault (stringContainsSimilar name imageName)
    in
    case
        ( List.Extra.find imageIsLikelyToBe explicitlyUnsupportedDistributions
        , List.Extra.find (.identifier >> imageIsLikelyToBe) supportedDistributions
        )
    of
        ( Just unsupportedDistribution, _ ) ->
            Just (ImageOperatingSystem unsupportedDistribution maybeOsVersion (Just False))

        ( _, Just { friendlyName, supportedVersions } ) ->
            let
                maybeVersionIsSupported =
                    maybeOsVersion
                        |> Maybe.andThen
                            (\v ->
                                if List.any (stringSimilar v) supportedVersions then
                                    Just True

                                else
                                    Nothing
                            )
            in
            Just (ImageOperatingSystem friendlyName maybeOsVersion maybeVersionIsSupported)

        _ ->
            {- If we have a named distribution from the image tags,
               we'll just use that with unknown support as a fallback
            -}
            maybeOsDistro
                |> Maybe.map (\distribution -> ImageOperatingSystem distribution maybeOsVersion Nothing)


detectImageOperatingSystem : OSTypes.Image -> Maybe ImageOperatingSystem
detectImageOperatingSystem { name, osDistro, osVersion } =
    detectOperatingSystem name osDistro osVersion
