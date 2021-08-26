module Route exposing (ProjectRouteConstructor(..), Route(..), routeToUrl)

import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes


type Route
    = GetSupport (Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid ))
    | HelpAbout
    | LoadingUnscopedProjects OSTypes.AuthTokenString
    | LoginJetstream (Maybe HelperTypes.JetstreamCreds)
    | LoginOpenstack (Maybe OSTypes.OpenstackLogin)
    | LoginPicker
    | MessageLog Bool
    | PageNotFound
    | ProjectRoute HelperTypes.ProjectIdentifier ProjectRouteConstructor
    | SelectProjects OSTypes.KeystoneUrl
    | Settings


type ProjectRouteConstructor
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
    | VolumeAttach (Maybe OSTypes.ServerUuid) (Maybe OSTypes.VolumeUuid)
    | VolumeCreate
    | VolumeDetail OSTypes.VolumeUuid
    | VolumeList
    | VolumeMountInstructions OSTypes.VolumeAttachment


routeToUrl : Maybe String -> Route -> String
routeToUrl maybePathPrefix page =
    case page of
        HelpAbout ->
            "TODO"

        _ ->
            "TODO"
