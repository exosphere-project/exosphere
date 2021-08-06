module Types.OuterMsg exposing (OuterMsg(..))

import Page.LoginOpenstack
import Page.Nested
import Types.HelperTypes as HelperTypes
import Types.SharedMsg
import Types.View as ViewTypes


type OuterMsg
    = SetNonProjectView ViewTypes.NonProjectViewConstructor
    | SetProjectView HelperTypes.ProjectIdentifier ViewTypes.ProjectViewConstructor
    | SharedMsg Types.SharedMsg.SharedMsg
    | NestedViewMsg Page.Nested.Msg
    | LoginOpenstackMsg Page.LoginOpenstack.Msg
