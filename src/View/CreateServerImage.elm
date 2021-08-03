module View.CreateServerImage exposing (createServerImage)

import Element
import Element.Input as Input
import Helpers.String
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Types.Msg exposing (Msg(..), ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..))
import Types.Types
    exposing
        ( Project
        )
import Types.View exposing (ProjectViewConstructor(..))
import View.Helpers as VH
import View.Types
import Widget


createServerImage : View.Types.Context -> Project -> OSTypes.ServerUuid -> String -> Element.Element Msg
createServerImage context project serverUuid imageName =
    Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
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
        , Element.column VH.formContainer
            [ Input.text
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
            , Element.row [ Element.width Element.fill ]
                [ Element.el [ Element.alignRight ]
                    (Widget.textButton
                        (SH.materialStyle context.palette).primaryButton
                        { text = "Create"
                        , onPress =
                            Just <|
                                ProjectMsg project.auth.project.uuid <|
                                    ServerMsg serverUuid <|
                                        RequestCreateServerImage imageName
                        }
                    )
                ]
            ]
        ]
