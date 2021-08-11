module Types.OuterMsg exposing (OuterMsg(..))

import Page.GetSupport
import Page.LoginJetstream
import Page.LoginOpenstack
import Page.MessageLog
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
