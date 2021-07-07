module View.CreateServerImage exposing (createServerImage)

import Element
import Element.Input as Input
import Helpers.String
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Types.Types
    exposing
        ( Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , ServerSpecificMsgConstructor(..)
        )
import View.Helpers as VH
import View.Types
import Widget
import Widget.Style.Material


createServerImage : View.Types.Context -> Project -> OSTypes.ServerUuid -> String -> Element.Element Msg
createServerImage context project serverUuid imageName =
    Element.column VH.exoColumnAttributes
        [ Element.el
            (VH.heading2 context.palette)
            (Element.text <|
                String.join
                    " "
                    [ String.join " "
                        [ "Create"
                        , context.localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.toTitleCase
                        , "from"
                        ]
                    , context.localization.virtualComputer
                        |> Helpers.String.toTitleCase
                    ]
            )
        , Input.text
            [ Element.spacing 12 ]
            { text = imageName
            , placeholder = Nothing
            , onChange = \n -> ProjectMsg project.auth.project.uuid <| SetProjectView <| CreateServerImage serverUuid n
            , label =
                Input.labelAbove []
                    (Element.text <|
                        String.join " "
                            [ context.localization.staticRepresentationOfBlockDeviceContents
                                |> Helpers.String.toTitleCase
                            , "name"
                            ]
                    )
            }
        , Widget.textButton
            (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
            { text = "Create"
            , onPress =
                Just <|
                    ProjectMsg project.auth.project.uuid <|
                        ServerMsg serverUuid <|
                            RequestCreateServerImage imageName
            }
        ]
