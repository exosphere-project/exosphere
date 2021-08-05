module View.Login exposing (loginPickerButton)

import Element
import Style.Helpers as SH
import Types.Msg exposing (SharedMsg(..))
import Types.View
    exposing
        ( LoginView(..)
        , NonProjectViewConstructor(..)
        , OpenstackLoginFormEntryType(..)
        )
import View.Types
import Widget


loginPickerButton : View.Types.Context -> Element.Element SharedMsg
loginPickerButton context =
    Widget.textButton
        (SH.materialStyle context.palette).button
        { text = "Other Login Methods"
        , onPress =
            Just <| SetNonProjectView <| LoginPicker
        }
