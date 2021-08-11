module Types.OuterMsg exposing (OuterMsg(..))

import Page.GetSupport
import Page.LoginJetstream
import Page.LoginOpenstack
import Page.LoginPicker
import Page.MessageLog
import Page.SelectProjects
import Page.Settings
import Types.HelperTypes as HelperTypes
import Types.SharedMsg
import Types.View as ViewTypes


type OuterMsg
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
