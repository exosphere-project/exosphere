module Types.ServerVolumeActions exposing (ServerVolumeAction(..), ServerVolumeActionRequest, ServerVolumeRequestStatus(..))

import OpenStack.Types exposing (VolumeUuid)


type ServerVolumeAction
    = AttachVolume VolumeUuid
    | DetachVolume VolumeUuid


type ServerVolumeRequestStatus
    = -- Pending means we have initiated an API call that has not yet returned.
      Pending
      -- Accepted means the API call succeeded but Cinder isn't reflecing the result (of attaching or detaching) yet.
    | Accepted


type alias ServerVolumeActionRequest =
    { action : ServerVolumeAction
    , status : ServerVolumeRequestStatus
    }
