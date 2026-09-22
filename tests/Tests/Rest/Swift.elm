module Tests.Rest.Swift exposing (aclUpdateHeadersSuite, cacheBusterSuite)

import Expect
import OpenStack.ObjectStorage as ObjectStorage
import Rest.Swift
import Test exposing (Test, describe, test)
import Time


cacheBusterSuite : Test
cacheBusterSuite =
    describe "Rest.Swift.cacheBuster"
        [ test "same millis with different nonce yields different strings" <|
            \_ ->
                Expect.notEqual
                    (Rest.Swift.cacheBuster (Time.millisToPosix 1000) 1)
                    (Rest.Swift.cacheBuster (Time.millisToPosix 1000) 2)
        , test "different millis with same nonce yields different strings" <|
            \_ ->
                Expect.notEqual
                    (Rest.Swift.cacheBuster (Time.millisToPosix 1000) 1)
                    (Rest.Swift.cacheBuster (Time.millisToPosix 2000) 1)
        ]


{-| `RemoveAcl` sends both revoke headers: Ceph RGW acts on the empty-valued `X-Container-*`, native
Swift acts on either one, so a proxy that drops empty-valued headers still revokes on Swift.
-}
aclUpdateHeadersSuite : Test
aclUpdateHeadersSuite =
    describe "Rest.Swift.aclUpdateHeaders maps a ContainerAclUpdate to Swift ACL headers"
        [ test "RemoveAcl on read sends the empty set header and the remove header" <|
            \_ ->
                Expect.equal
                    [ ( "X-Container-Read", "" ), ( "X-Remove-Container-Read", "true" ) ]
                    (Rest.Swift.aclUpdateHeaders
                        { read = ObjectStorage.RemoveAcl, write = ObjectStorage.LeaveAcl }
                    )
        , test "RemoveAcl on write sends the write pair" <|
            \_ ->
                Expect.equal
                    [ ( "X-Container-Write", "" ), ( "X-Remove-Container-Write", "true" ) ]
                    (Rest.Swift.aclUpdateHeaders
                        { read = ObjectStorage.LeaveAcl, write = ObjectStorage.RemoveAcl }
                    )
        , test "SetAcl sends only the set header, LeaveAcl sends nothing" <|
            \_ ->
                Expect.equal
                    [ ( "X-Container-Read", ".r:*" ) ]
                    (Rest.Swift.aclUpdateHeaders
                        { read = ObjectStorage.SetAcl ".r:*", write = ObjectStorage.LeaveAcl }
                    )
        , test "LeaveAcl on both fields sends no headers at all" <|
            \_ ->
                Expect.equal []
                    (Rest.Swift.aclUpdateHeaders
                        { read = ObjectStorage.LeaveAcl, write = ObjectStorage.LeaveAcl }
                    )
        ]
