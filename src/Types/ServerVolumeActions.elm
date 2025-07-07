module Types.ServerVolumeActions exposing (ServerVolumeAction(..), ServerVolumeActionRequest, ServerVolumeRequestStatus(..))

import OpenStack.Types exposing (VolumeUuid)


type ServerVolumeAction
    = AttachVolume VolumeUuid
    | DetachVolume VolumeUuid


type ServerVolumeRequestStatus
    = Pending
    | Accepted


type alias ServerVolumeActionRequest =
    { action : ServerVolumeAction
    , status : ServerVolumeRequestStatus
    }
