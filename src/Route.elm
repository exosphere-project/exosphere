module Route exposing (NavigablePage(..), NavigableProjectPage(..), routeToUrl)

import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes


type NavigablePage
    = GetSupport (Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid ))
    | HelpAbout
    | LoadingUnscopedProjects OSTypes.AuthTokenString
    | LoginJetstream (Maybe HelperTypes.JetstreamCreds)
    | LoginOpenstack (Maybe OSTypes.OpenstackLogin)
    | LoginPicker
    | MessageLog Bool
    | PageNotFound
    | ProjectPage HelperTypes.ProjectIdentifier NavigableProjectPage
    | Settings


type NavigableProjectPage
    = AllResourcesList
    | FloatingIpAssign (Maybe OSTypes.IpAddressUuid) (Maybe OSTypes.ServerUuid)
    | FloatingIpList
    | ImageList
    | KeypairCreate
    | KeypairList
    | ServerCreate OSTypes.ImageUuid String (Maybe Bool)
    | ServerCreateImage OSTypes.ServerUuid (Maybe String)
    | ServerDetail OSTypes.ServerUuid
    | ServerList
    | VolumeMountInstructions OSTypes.VolumeAttachment
    | VolumeAttach (Maybe OSTypes.ServerUuid) (Maybe OSTypes.VolumeUuid)
    | VolumeCreate
    | VolumeDetail OSTypes.VolumeUuid
    | VolumeList


routeToUrl : Maybe String -> NavigablePage -> String
routeToUrl maybePathPrefix page =
    case page of
        HelpAbout ->
            "TODO"

        _ ->
            "TODO"
