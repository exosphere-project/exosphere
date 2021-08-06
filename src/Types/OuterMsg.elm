module Types.OuterMsg exposing (OuterMsg(..))

import Types.HelperTypes as HelperTypes
import Types.Msg
import Types.View as ViewTypes
import View.LoginOpenstack
import View.Nested


type OuterMsg
    = SetNonProjectView ViewTypes.NonProjectViewConstructor
    | SetProjectView HelperTypes.ProjectIdentifier ViewTypes.ProjectViewConstructor
    | SharedMsg Types.Msg.SharedMsg
    | NestedViewMsg View.Nested.Msg
    | LoginOpenstackMsg View.LoginOpenstack.Msg
