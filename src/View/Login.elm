module View.Login exposing (loginPickerButton)

import Element
import Style.Helpers as SH
import Types.Msg exposing (Msg(..))
import Types.View
    exposing
        ( JetstreamProvider(..)
        , LoginView(..)
        , NonProjectViewConstructor(..)
        , OpenstackLoginFormEntryType(..)
        )
import View.Types
import Widget


loginPickerButton : View.Types.Context -> Element.Element Msg
loginPickerButton context =
    Widget.textButton
        (SH.materialStyle context.palette).button
        { text = "Other Login Methods"
        , onPress =
            Just <| SetNonProjectView <| LoginPicker
        }
