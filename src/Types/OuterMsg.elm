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
import Page.VolumeMountInstructions
import Types.HelperTypes as HelperTypes
import Types.SharedMsg
import Types.View as ViewTypes


type OuterMsg
    = SetNonProjectView ViewTypes.NonProjectViewConstructor
    | SetProjectView HelperTypes.ProjectIdentifier ViewTypes.ProjectViewConstructor
    | SharedMsg Types.SharedMsg.SharedMsg
    | AllResourcesListMsg Page.AllResourcesList.Msg
    | FloatingIpAssignMsg Page.FloatingIpAssign.Msg
    | FloatingIpListMsg Page.FloatingIpList.Msg
    | GetSupportMsg Page.GetSupport.Msg
    | ImageListMsg Page.ImageList.Msg
    | KeypairCreateMsg Page.KeypairCreate.Msg
    | KeypairListMsg Page.KeypairList.Msg
    | LoginJetstreamMsg Page.LoginJetstream.Msg
    | LoginOpenstackMsg Page.LoginOpenstack.Msg
    | LoginPickerMsg Page.LoginPicker.Msg
    | MessageLogMsg Page.MessageLog.Msg
    | SelectProjectsMsg Page.SelectProjects.Msg
    | SettingsMsg Page.Settings.Msg
    | VolumeAttachMsg Page.VolumeAttach.Msg
    | VolumeCreateMsg Page.VolumeCreate.Msg
    | VolumeDetailMsg Page.VolumeDetail.Msg
    | VolumeListMsg Page.VolumeList.Msg
    | VolumeMountInstructionsMsg Page.VolumeMountInstructions.Msg
    | ServerCreateMsg Page.ServerCreate.Msg
    | ServerCreateImageMsg Page.ServerCreateImage.Msg
    | ServerDetailMsg Page.ServerDetail.Msg
    | ServerListMsg Page.ServerList.Msg
