module Tests.OpenStack.ObjectStorage exposing
    ( objectPathSuite
    )

import Expect
import OpenStack.ObjectStorage as ObjectStorage
import Test exposing (Test, describe, test)


objectPathSuite : Test
objectPathSuite =
    describe "objectPath splits object names on / into separate URL segments"
        [ test "a simple object becomes [container, name]" <|
            \_ ->
                Expect.equal [ "mycontainer", "file.txt" ] (ObjectStorage.objectPath "mycontainer" "file.txt")
        , test "a pseudo-folder path splits each segment" <|
            \_ ->
                Expect.equal [ "c", "a", "b", "file.txt" ] (ObjectStorage.objectPath "c" "a/b/file.txt")
        , test "spaces, #, ? are kept in the segment (Url.Builder encodes them later)" <|
            \_ ->
                Expect.equal [ "c", "a", "file name #1?.txt" ] (ObjectStorage.objectPath "c" "a/file name #1?.txt")
        , test "unicode segments are preserved" <|
            \_ ->
                Expect.equal [ "c", "café", "£.txt" ] (ObjectStorage.objectPath "c" "café/£.txt")
        , test "a trailing slash yields a trailing empty segment" <|
            \_ ->
                Expect.equal [ "c", "a", "b", "" ] (ObjectStorage.objectPath "c" "a/b/")
        , test "consecutive slashes yield empty segments" <|
            \_ ->
                Expect.equal [ "c", "a", "", "b" ] (ObjectStorage.objectPath "c" "a//b")
        ]
