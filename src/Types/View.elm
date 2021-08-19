module Types.View exposing
    ( LoginView(..)
    , NonProjectViewConstructor(..)
    , ProjectViewConstructor(..)
    , ViewState(..)
    )

import OpenStack.Types as OSTypes
import Page.AllResources
import Page.FloatingIpAssign
import Page.FloatingIpList
import Page.GetSupport
import Page.ImageList
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginJetstream
import Page.LoginOpenstack
import Page.MessageLog
import Page.SelectProjects
import Page.ServerCreateImage
import Page.ServerDetail
import Page.ServerList
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
    = LoginPicker
    | Login LoginView
    | LoadingUnscopedProjects OSTypes.AuthTokenString
    | SelectProjects Page.SelectProjects.Model
    | MessageLog Page.MessageLog.Model
    | Settings
    | GetSupport Page.GetSupport.Model
    | HelpAbout
    | PageNotFound


type LoginView
    = LoginOpenstack Page.LoginOpenstack.Model
    | LoginJetstream Page.LoginJetstream.Model


type
    ProjectViewConstructor
    -- TODO order these
    = AllResources Page.AllResources.Model
    | ImageList Page.ImageList.Model
    | ServerList Page.ServerList.Model
    | ServerDetail Page.ServerDetail.Model
    | ServerCreate HelperTypes.CreateServerPageModel
    | ServerCreateImage Page.ServerCreateImage.Model
    | VolumeList Page.VolumeList.Model
    | VolumeDetail Page.VolumeDetail.Model
    | VolumeCreate Page.VolumeCreate.Model
    | VolumeAttach Page.VolumeAttach.Model
    | VolumeMountInstructions OSTypes.VolumeAttachment
    | FloatingIpList Page.FloatingIpList.Model
    | FloatingIpAssign Page.FloatingIpAssign.Model
    | KeypairList Page.KeypairList.Model
    | KeypairCreate Page.KeypairCreate.Model
