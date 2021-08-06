module Types.OuterMsg exposing (OuterMsg(..))

import Page.Example
import Page.LoginOpenstack
import Types.HelperTypes as HelperTypes
import Types.SharedMsg
import Types.View as ViewTypes


type OuterMsg
    = SetNonProjectView ViewTypes.NonProjectViewConstructor
    | SetProjectView HelperTypes.ProjectIdentifier ViewTypes.ProjectViewConstructor
    | SharedMsg Types.SharedMsg.SharedMsg
    | ExamplePageMsg Page.Example.Msg
    | LoginOpenstackMsg Page.LoginOpenstack.Msg
