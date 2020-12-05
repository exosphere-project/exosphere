module View.CreateServerImage exposing (createServerImage)

import Element
import Element.Input as Input
import OpenStack.Types as OSTypes
import Types.Types
    exposing
        ( Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Style
        )
import View.Helpers as VH
import Widget
import Widget.Style.Material


createServerImage : Style -> Project -> OSTypes.ServerUuid -> String -> Element.Element Msg
createServerImage style project serverUuid imageName =
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2 (Element.text "Create Image from Server")
        , Input.text
            [ Element.spacing 12 ]
            { text = imageName
            , placeholder = Nothing
            , onChange = \n -> ProjectMsg project.auth.project.uuid <| SetProjectView <| CreateServerImage serverUuid n
            , label = Input.labelAbove [] (Element.text "Image name")
            }
        , Widget.textButton
            (Widget.Style.Material.containedButton style.palette)
            { text = "Create"
            , onPress =
                Just <|
                    ProjectMsg project.auth.project.uuid <|
                        RequestCreateServerImage serverUuid imageName
            }
        ]
