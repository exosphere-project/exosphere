module LegacyView.Login exposing (loginPickerButton)

import Element
import Style.Helpers as SH
import Types.OuterMsg exposing (OuterMsg(..))
import Types.View
    exposing
        ( LoginView(..)
        , NonProjectViewConstructor(..)
        )
import View.Types
import Widget


loginPickerButton : View.Types.Context -> Element.Element OuterMsg
loginPickerButton context =
    Widget.textButton
        (SH.materialStyle context.palette).button
        { text = "Other Login Methods"
        , onPress =
            Just <| SetNonProjectView <| LoginPicker
        }
