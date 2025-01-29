module Helpers.Image exposing (detectImageOperatingSystem)

import List.Extra
import OpenStack.Types exposing (ImageOperatingSystem)


supportedDistributions : List ( String, List String )
supportedDistributions =
    {- A list of known distributions and their supported versions

       Leaving an empty list of versions will allow detection of the
       distribution leaving the `supported` field empty
    -}
    [ ( "Debian", [ "11" ] )
    , ( "Ubuntu", [ "20.04", "22.04", "24.04", "24.10" ] )
    , ( "rocky", [ "8", "9" ] )

    -- Images removed from JS2, but the code should still function
    , ( "AlmaLinux", [ "8", "9" ] )

    -- Technically may still work, but we don't officially support any versions
    , ( "CentOS", [] )
    ]


explicitlyUnsupportedDistributions : List String
explicitlyUnsupportedDistributions =
    [ "Windows" ]


detectImageOperatingSystem : String -> Maybe String -> Maybe String -> Maybe ImageOperatingSystem
detectImageOperatingSystem imageName maybeOsDistro maybeOsVersion =
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
        , List.Extra.find (Tuple.first >> imageIsLikelyToBe) supportedDistributions
        )
    of
        ( Just unsupportedDistribution, _ ) ->
            Just (ImageOperatingSystem unsupportedDistribution maybeOsVersion (Just False))

        ( _, Just ( distribution, supportedVersions ) ) ->
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
            Just (ImageOperatingSystem distribution maybeOsVersion maybeVersionIsSupported)

        _ ->
            {- If we have a named distribution from the image tags,
               we'll just use that with unknown support as a fallback
            -}
            maybeOsDistro
                |> Maybe.map (\distribution -> ImageOperatingSystem distribution maybeOsVersion Nothing)
