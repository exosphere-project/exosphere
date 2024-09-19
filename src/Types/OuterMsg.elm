module Types.OuterMsg exposing (OuterMsg(..))

import Page.FloatingIpAssign
import Page.FloatingIpCreate
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
import Page.SecurityGroupDetail
import Page.SecurityGroupList
import Page.SelectProjectRegions
import Page.SelectProjects
import Page.ServerCreate
import Page.ServerCreateImage
import Page.ServerDetail
import Page.ServerList
import Page.ServerResize
import Page.ServerSecurityGroups
import Page.Settings
import Page.ShareCreate
import Page.ShareDetail
import Page.ShareList
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
    | FloatingIpCreateMsg Page.FloatingIpCreate.Msg
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
    | SecurityGroupDetailMsg Page.SecurityGroupDetail.Msg
    | SecurityGroupListMsg Page.SecurityGroupList.Msg
    | ServerCreateMsg Page.ServerCreate.Msg
    | ServerCreateImageMsg Page.ServerCreateImage.Msg
    | ServerDetailMsg Page.ServerDetail.Msg
    | ServerListMsg Page.ServerList.Msg
    | ServerResizeMsg Page.ServerResize.Msg
    | ServerSecurityGroupsMsg Page.ServerSecurityGroups.Msg
    | ShareCreateMsg Page.ShareCreate.Msg
    | ShareDetailMsg Page.ShareDetail.Msg
    | ShareListMsg Page.ShareList.Msg
