module Types.OuterMsg exposing (OuterMsg(..))

import Page.FloatingIpAssign
import Page.FloatingIpList
import Page.GetSupport
import Page.Home
import Page.ImageList
import Page.InstanceSourcePicker
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginOpenstack
import Page.LoginPicker
import Page.MessageLog
import Page.ProjectOverview
import Page.SelectProjectRegions
import Page.SelectProjects
import Page.ServerCreate
import Page.ServerCreateImage
import Page.ServerDetail
import Page.ServerList
import Page.ServerResize
import Page.Settings
import Page.VolumeAttach
import Page.VolumeCreate
import Page.VolumeDetail
import Page.VolumeList
import Page.VolumeMountInstructions
import Types.SharedMsg


type OuterMsg
    = SharedMsg Types.SharedMsg.SharedMsg
    | ProjectOverviewMsg Page.ProjectOverview.Msg
    | FloatingIpAssignMsg Page.FloatingIpAssign.Msg
    | FloatingIpListMsg Page.FloatingIpList.Msg
    | GetSupportMsg Page.GetSupport.Msg
    | HomeMsg Page.Home.Msg
    | ImageListMsg Page.ImageList.Msg
    | InstanceSourcePickerMsg Page.InstanceSourcePicker.Msg
    | KeypairCreateMsg Page.KeypairCreate.Msg
    | KeypairListMsg Page.KeypairList.Msg
    | LoginOpenstackMsg Page.LoginOpenstack.Msg
    | LoginPickerMsg Page.LoginPicker.Msg
    | MessageLogMsg Page.MessageLog.Msg
    | SelectProjectRegionsMsg Page.SelectProjectRegions.Msg
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
    | ServerResizeMsg Page.ServerResize.Msg
