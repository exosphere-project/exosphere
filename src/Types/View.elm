module Types.View exposing
    ( LoginView(..)
    , NonProjectViewConstructor(..)
    , ProjectViewConstructor(..)
    , ViewState(..)
    )

import OpenStack.Types as OSTypes
import Page.FloatingIpAssign
import Page.FloatingIpList
import Page.GetSupport
import Page.Home
import Page.InstanceSourcePicker
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginJetstream
import Page.LoginOpenstack
import Page.MessageLog
import Page.ProjectOverview
import Page.SelectProjects
import Page.ServerCreateImage
import Page.ServerDetail
import Page.ServerList
import Page.Settings
import Page.VolumeAttach
import Page.VolumeCreate
import Page.VolumeDetail
import Page.VolumeList
import Types.HelperTypes as HelperTypes



-- Types that describe view state. After the Legacy Views are migrated to pages, we may want to move these to OuterModel.


type ViewState
    = NonProjectView NonProjectViewConstructor
    | ProjectView HelperTypes.ProjectIdentifier { createPopup : Bool } ProjectViewConstructor


type NonProjectViewConstructor
    = GetSupport Page.GetSupport.Model
    | HelpAbout
    | Home Page.Home.Model
    | LoadingUnscopedProjects OSTypes.AuthTokenString
    | Login LoginView
    | LoginPicker
    | MessageLog Page.MessageLog.Model
    | PageNotFound
    | SelectProjects Page.SelectProjects.Model
    | Settings Page.Settings.Model


type LoginView
    = LoginOpenstack Page.LoginOpenstack.Model
    | LoginJetstream Page.LoginJetstream.Model


type ProjectViewConstructor
    = ProjectOverview Page.ProjectOverview.Model
    | FloatingIpAssign Page.FloatingIpAssign.Model
    | FloatingIpList Page.FloatingIpList.Model
    | InstanceSourcePicker Page.InstanceSourcePicker.Model
    | KeypairCreate Page.KeypairCreate.Model
    | KeypairList Page.KeypairList.Model
    | ServerCreate HelperTypes.CreateServerPageModel
    | ServerCreateImage Page.ServerCreateImage.Model
    | ServerDetail Page.ServerDetail.Model
    | ServerList Page.ServerList.Model
      -- TODO these should be in alphabetical order
    | VolumeList Page.VolumeList.Model
    | VolumeAttach Page.VolumeAttach.Model
    | VolumeCreate Page.VolumeCreate.Model
    | VolumeDetail Page.VolumeDetail.Model
    | VolumeMountInstructions OSTypes.VolumeAttachment
