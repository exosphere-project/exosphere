module Tests.Helpers.Image exposing (imageOSDetectionSuite)

-- Test related Modules
-- Exosphere Modules Under Test

import Expect
import Helpers.Image exposing (detectOperatingSystem)
import Test exposing (Test, describe, test)


imageOSDetectionSuite : Test
imageOSDetectionSuite =
    describe "Image operating system detection"
        [ test "helpful name, no metadata" <|
            \_ ->
                -- Is `supported = Nothing` the behavior we want? We definitely support Ubuntu but not sure of the version.
                Expect.equal
                    (detectOperatingSystem "Featured-Ubuntu24" Nothing Nothing)
                    (Just { distribution = "Ubuntu", version = Nothing, supported = Nothing })
        , test "helpful name and metadata" <|
            \_ ->
                Expect.equal
                    (detectOperatingSystem "Featured-Ubuntu24" (Just "ubuntu") (Just "24.04"))
                    (Just { distribution = "Ubuntu", version = Just "24.04", supported = Just True })
        , test "Rocky Linux image" <|
            \_ ->
                Expect.equal
                    (detectOperatingSystem "Featured-RockyLinux8" (Just "rocky") (Just "8"))
                    (Just { distribution = "Rocky Linux", version = Just "8", supported = Just True })
        , test "JS2 Windows image, no metadata" <|
            \_ ->
                Expect.equal
                    (detectOperatingSystem "Windows-Server-2022-JS2-Beta" Nothing Nothing)
                    (Just { distribution = "Windows", version = Nothing, supported = Just False })
        , test "explicitly unsupported image with metadata" <|
            \_ ->
                Expect.equal
                    (detectOperatingSystem "any-image-name-here" (Just "windows") (Just "3.1"))
                    (Just { distribution = "Windows", version = Just "3.1", supported = Just False })
        , test "unknown operating system with metadata" <|
            \_ ->
                Expect.equal
                    (detectOperatingSystem "any-image-name-here" (Just "NeXTSTEP") (Just "3.3"))
                    (Just { distribution = "NeXTSTEP", version = Just "3.3", supported = Nothing })
        , test "conflicting name and metadata, we should use metadata first" <|
            \_ ->
                Expect.equal
                    (detectOperatingSystem "Windows-Server-2022-JS2-Beta" (Just "ubuntu") (Just "24.04"))
                    (Just { distribution = "Ubuntu", version = Just "24.04", supported = Just True })
        , test "conflicting name and metadata, other way around" <|
            \_ ->
                Expect.equal
                    (detectOperatingSystem "Featured-Ubuntu24" (Just "Windows") (Just "95"))
                    (Just { distribution = "Windows", version = Just "95", supported = Just False })
        , test "unhelpful name but with metadata" <|
            \_ ->
                Expect.equal
                    (detectOperatingSystem "unhelpful-image-name" (Just "ubuntu") (Just "24.04"))
                    (Just { distribution = "Ubuntu", version = Just "24.04", supported = Just True })
        , test "a complete enigma" <|
            \_ ->
                Expect.equal
                    (detectOperatingSystem "unhelpful-image-name" Nothing Nothing)
                    Nothing
        , test "sneaky edge case, will not pass" <|
            \_ ->
                Expect.notEqual
                    (detectOperatingSystem "not-windows-actually-ubuntu" Nothing Nothing)
                    (Just { distribution = "Ubuntu", version = Nothing, supported = Nothing })
        ]
