module Tests.OpenStack.Volumes exposing (volumeSuite)

import Expect
import Json.Decode as Decode
import OpenStack.Types as OSTypes
import OpenStack.Volumes exposing (volumeDecoder)
import Test exposing (Test, describe, test)
import Time exposing (millisToPosix)


volumeJson : String -> String
volumeJson volumeImageMetadata =
    """
{
    "name": "test-volume",
    "id": "020fcde6-e5a0-40d1-b28f-bf320fec43a0",
    "status": "in-use",
    "size": 100,
    "description": "",
    "attachments": [
        {
            "volume_id": "020fcde6-e5a0-40d1-b28f-bf320fec43a0",
            "server_id": "bac482fc-9f29-425e-9688-d05a3064f701",
            "attachment_id": "b543cb68-67c1-41d0-aa24-b212e3c0ad92",
            "device": "/dev/sda"
        }
    ],
    "volume_image_metadata": """
        ++ volumeImageMetadata
        ++ """,
    "created_at": "2026-06-02T15:52:25.000000",
    "user_id": "d2da00fa79ac482a947069a226f5f368"
}
"""


expectedVolume : Maybe OSTypes.NameAndUuid -> OSTypes.Volume
expectedVolume imageMetadata =
    { name = Just "test-volume"
    , uuid = "020fcde6-e5a0-40d1-b28f-bf320fec43a0"
    , status = OSTypes.InUse
    , size = 100
    , description = Just ""
    , attachments =
        [ { volumeUuid = "020fcde6-e5a0-40d1-b28f-bf320fec43a0"
          , serverUuid = "bac482fc-9f29-425e-9688-d05a3064f701"
          , attachmentUuid = "b543cb68-67c1-41d0-aa24-b212e3c0ad92"
          , device = Just "/dev/sda"
          }
        ]
    , imageMetadata = imageMetadata
    , createdAt = millisToPosix 1780415545000
    , userUuid = "d2da00fa79ac482a947069a226f5f368"
    }


volumeSuite : Test
volumeSuite =
    describe "Decoding volumes"
        [ test "volume image metadata with image name and ID" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString
                        volumeDecoder
                        (volumeJson "{ \"image_name\": \"tra230023-kimi-devops-vm\", \"image_id\": \"66efa857-0fe4-4faf-aab6-9a2ff5bcb80d\" }")
                    )
                    (Ok
                        (expectedVolume
                            (Just
                                { name = "tra230023-kimi-devops-vm"
                                , uuid = "66efa857-0fe4-4faf-aab6-9a2ff5bcb80d"
                                }
                            )
                        )
                    )
        , test "null volume image metadata" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString volumeDecoder (volumeJson "null"))
                    (Ok (expectedVolume Nothing))
        , test "partial volume image metadata" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString
                        volumeDecoder
                        (volumeJson "{ \"signature_verified\": \"False\" }")
                    )
                    (Ok (expectedVolume Nothing))
        ]
