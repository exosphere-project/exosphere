module Route exposing (NavigablePage(..), NavigableProjectPage(..))

import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes


type NavigablePage
    = GetSupport (Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid ))
    | HelpAbout
    | LoginJetstream
    | LoginOpenstack
    | LoginPicker
    | ProjectPage HelperTypes.ProjectIdentifier NavigableProjectPage


type NavigableProjectPage
    = FloatingIpAssign (Maybe OSTypes.IpAddressUuid) (Maybe OSTypes.ServerUuid)
    | FloatingIpList
    | KeypairCreate
    | KeypairList
    | ServerCreate OSTypes.ImageUuid String (Maybe Bool)
    | ServerCreateImage OSTypes.ServerUuid (Maybe String)
    | ServerDetail OSTypes.ServerUuid
    | ServerList
    | VolumeAttach (Maybe OSTypes.ServerUuid) (Maybe OSTypes.VolumeUuid)
    | VolumeCreate
    | VolumeDetail OSTypes.VolumeUuid
    | VolumeList
