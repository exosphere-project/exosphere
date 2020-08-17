module View.CreateServerImage exposing (createServerImage)

import Element
import Element.Input as Input
import Helpers.Helpers as Helpers
import OpenStack.Types as OSTypes
import Style.Theme
import Types.Types
    exposing
        ( Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        )
import View.Helpers as VH
import Widget
import Widget.Style.Material


createServerImage : Project -> OSTypes.ServerUuid -> String -> Element.Element Msg
createServerImage project serverUuid imageName =
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2 (Element.text "Create Image from Server")
        , Input.text
            [ Element.spacing 12 ]
            { text = imageName
            , placeholder = Nothing
            , onChange = \n -> ProjectMsg (Helpers.getProjectId project) <| SetProjectView <| CreateServerImage serverUuid n
            , label = Input.labelAbove [] (Element.text "Image name")
            }
        , Widget.textButton
            (Widget.Style.Material.containedButton Style.Theme.exoPalette)
            { text = "Create"
            , onPress =
                Just <|
                    ProjectMsg (Helpers.getProjectId project) <|
                        RequestCreateServerImage serverUuid imageName
            }
        ]
