module Tests.OpenStack.VolumeSnapshots exposing (volumeSnapshotsSuite)

-- Test related Modules
-- Exosphere Modules Under Test

import Expect
import Json.Decode as Decode
import OpenStack.VolumeSnapshots exposing (Status(..), volumeSnapshotDecoder)
import Test exposing (Test, describe, test)
import Time exposing (millisToPosix)


cinderVolumeSnapshots : String
cinderVolumeSnapshots =
    """
{
    "volume_snapshots": [
        {
            "status": "available",
            "description": "This is a snapshot with a description",
            "created_at": "2015-03-03T14:30:00.000000",
            "volume_id": "f5d3c3b3-3a1b-4d1d-9b3a-0f3b3d3f0e7d",
            "metadata": {},
            "id": "a7b6d2c6-2a1b-4d1d-9b3a-0f3b3d3f0e7d",
            "size": 1,
            "name": "snapshot-001-with-description"
        },
        {
            "status": "available",
            "description": null,
            "created_at": "2015-03-03T14:30:00.000000",
            "volume_id": "f5d3c3b3-3a1b-4d1d-9b3a-0f3b3d3f0e7d",
            "metadata": {},
            "id": "b7b6d2c6-2a1b-4d1d-9b3a-0f3b3d3f0e7d",
            "size": 1,
            "name": "snapshot-002-no-description"
        }
    ]
}
    """


volumeSnapshotsSuite : Test
volumeSnapshotsSuite =
    describe "Decoding volume snapshots"
        [ test "volume snapshots" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString
                        (Decode.field "volume_snapshots" (Decode.list volumeSnapshotDecoder))
                        cinderVolumeSnapshots
                    )
                    (Ok
                        [ { uuid = "a7b6d2c6-2a1b-4d1d-9b3a-0f3b3d3f0e7d"
                          , name = Just "snapshot-001-with-description"
                          , description = Just "This is a snapshot with a description"
                          , volumeId = "f5d3c3b3-3a1b-4d1d-9b3a-0f3b3d3f0e7d"
                          , sizeInGiB = 1
                          , createdAt = millisToPosix 1425393000000
                          , status = Available
                          }
                        , { uuid = "b7b6d2c6-2a1b-4d1d-9b3a-0f3b3d3f0e7d"
                          , name = Just "snapshot-002-no-description"
                          , description = Nothing
                          , volumeId = "f5d3c3b3-3a1b-4d1d-9b3a-0f3b3d3f0e7d"
                          , sizeInGiB = 1
                          , createdAt = millisToPosix 1425393000000
                          , status = Available
                          }
                        ]
                    )
        ]
