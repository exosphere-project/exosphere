module Types.View exposing
    ( LoginView(..)
    , NonProjectViewConstructor(..)
    , ProjectViewConstructor(..)
    , ViewState(..)
    )

import OpenStack.Types as OSTypes
import Page.Credentials
import Page.FloatingIpAssign
import Page.FloatingIpCreate
import Page.FloatingIpList
import Page.GetSupport
import Page.Home
import Page.ImageList
import Page.InstanceSourcePicker
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginOpenIdConnect
import Page.LoginOpenstack
import Page.MessageLog
import Page.ProjectOverview
import Page.SecurityGroupDetail
import Page.SecurityGroupList
import Page.SelectProjectRegions
import Page.SelectProjects
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
import Types.HelperTypes as HelperTypes



-- Types that describe view state. After the Legacy Views are migrated to pages, we may want to move these to OuterModel.


type ViewState
    = NonProjectView NonProjectViewConstructor
    | ProjectView HelperTypes.ProjectIdentifier ProjectViewConstructor


type NonProjectViewConstructor
    = GetSupport Page.GetSupport.Model
    | HelpAbout
    | Home Page.Home.Model
    | LoadingUnscopedProjects
    | Login LoginView
    | LoginPicker
    | MessageLog Page.MessageLog.Model
    | PageNotFound
    | SelectProjectRegions Page.SelectProjectRegions.Model
    | SelectProjects Page.SelectProjects.Model
    | Settings Page.Settings.Model


type LoginView
    = LoginOpenstack Page.LoginOpenstack.Model
    | LoginOpenIdConnect Page.LoginOpenIdConnect.Model


type ProjectViewConstructor
    = ProjectOverview Page.ProjectOverview.Model
    | Credentials Page.Credentials.Model
    | FloatingIpAssign Page.FloatingIpAssign.Model
    | FloatingIpList Page.FloatingIpList.Model
    | ImageList Page.ImageList.Model
    | InstanceSourcePicker Page.InstanceSourcePicker.Model
    | FloatingIpCreate Page.FloatingIpCreate.Model
    | KeypairCreate Page.KeypairCreate.Model
    | KeypairList Page.KeypairList.Model
    | SecurityGroupDetail Page.SecurityGroupDetail.Model
    | SecurityGroupList Page.SecurityGroupList.Model
    | ServerCreate HelperTypes.CreateServerPageModel
    | ServerCreateImage Page.ServerCreateImage.Model
    | ServerDetail Page.ServerDetail.Model
    | ServerList Page.ServerList.Model
    | ServerResize Page.ServerResize.Model
    | ServerSecurityGroups Page.ServerSecurityGroups.Model
    | ShareCreate Page.ShareCreate.Model
    | ShareDetail Page.ShareDetail.Model
    | ShareList Page.ShareList.Model
      -- TODO these should be in alphabetical order
    | VolumeList Page.VolumeList.Model
    | VolumeAttach Page.VolumeAttach.Model
    | VolumeCreate Page.VolumeCreate.Model
    | VolumeDetail Page.VolumeDetail.Model
    | VolumeMountInstructions OSTypes.VolumeAttachment
