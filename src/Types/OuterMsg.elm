module Types.OuterMsg exposing (OuterMsg(..))

import Page.AllResourcesList
import Page.FloatingIpAssign
import Page.FloatingIpList
import Page.GetSupport
import Page.ImageList
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginJetstream
import Page.LoginOpenstack
import Page.LoginPicker
import Page.MessageLog
import Page.SelectProjects
import Page.ServerCreate
import Page.ServerCreateImage
import Page.ServerDetail
import Page.ServerList
import Page.Settings
import Page.VolumeAttach
import Page.VolumeCreate
import Page.VolumeDetail
import Page.VolumeList
import Types.HelperTypes as HelperTypes
import Types.SharedMsg
import Types.View as ViewTypes


type
    OuterMsg
    -- TODO order these
    = SetNonProjectView ViewTypes.NonProjectViewConstructor
    | SetProjectView HelperTypes.ProjectIdentifier ViewTypes.ProjectViewConstructor
    | SharedMsg Types.SharedMsg.SharedMsg
    | LoginOpenstackMsg Page.LoginOpenstack.Msg
    | LoginJetstreamMsg Page.LoginJetstream.Msg
    | MessageLogMsg Page.MessageLog.Msg
    | SettingsMsg Page.Settings.Msg
    | GetSupportMsg Page.GetSupport.Msg
    | LoginPickerMsg Page.LoginPicker.Msg
    | SelectProjectsMsg Page.SelectProjects.Msg
    | FloatingIpListMsg Page.FloatingIpList.Msg
    | FloatingIpAssignMsg Page.FloatingIpAssign.Msg
    | KeypairListMsg Page.KeypairList.Msg
    | KeypairCreateMsg Page.KeypairCreate.Msg
    | VolumeCreateMsg Page.VolumeCreate.Msg
    | VolumeDetailMsg Page.VolumeDetail.Msg
    | VolumeListMsg Page.VolumeList.Msg
    | VolumeAttachMsg Page.VolumeAttach.Msg
    | ServerListMsg Page.ServerList.Msg
    | ServerDetailMsg Page.ServerDetail.Msg
    | ServerCreateMsg Page.ServerCreate.Msg
    | ServerCreateImageMsg Page.ServerCreateImage.Msg
    | ImageListMsg Page.ImageList.Msg
    | AllResourcesListMsg Page.AllResourcesList.Msg
