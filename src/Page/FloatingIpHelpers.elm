module Page.FloatingIpHelpers exposing (serverPicker)

import Element
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Types as OSTypes
import Style.Widgets.Select
import Style.Widgets.Spacer exposing (spacer)
import Types.Project exposing (Project)
import View.Helpers as VH
import View.Types


serverPicker : View.Types.Context -> Project -> Maybe OSTypes.ServerUuid -> (Maybe OSTypes.ServerUuid -> msg) -> Element.Element msg
serverPicker context project currentServerUuid toMsg =
    let
        selectServerText =
            String.join " "
                [ "Select"
                , Helpers.String.indefiniteArticle context.localization.virtualComputer
                , context.localization.virtualComputer
                , "(optional)"
                ]

        serverChoices =
            project.servers
                |> RDPP.withDefault []
                |> List.filter
                    (\s ->
                        not <|
                            List.member s.osProps.details.openstackStatus
                                [ OSTypes.ServerSoftDeleted
                                , OSTypes.ServerError
                                , OSTypes.ServerBuild
                                , OSTypes.ServerDeleted
                                ]
                    )
                |> List.filter
                    (\s ->
                        GetterSetters.getServerFloatingIps project s.osProps.uuid |> List.isEmpty
                    )
                |> List.map
                    (\s ->
                        ( s.osProps.uuid
                        , VH.extendedResourceName (Just s.osProps.name) s.osProps.uuid context.localization.virtualComputer
                        )
                    )
    in
    Element.column
        [ Element.spacing spacer.px16
        , Element.width Element.fill
        ]
        [ Element.text selectServerText
        , if List.isEmpty serverChoices then
            Element.paragraph []
                [ Element.text <|
                    String.join " "
                        [ "You don't have any"
                        , context.localization.virtualComputer
                            |> Helpers.String.pluralize
                        , "that don't already have"
                        , Helpers.String.indefiniteArticle context.localization.floatingIpAddress
                        , context.localization.floatingIpAddress
                        , "assigned."
                        ]
                ]

          else
            Style.Widgets.Select.select
                []
                context.palette
                { label = ""
                , onChange = toMsg
                , options = serverChoices
                , selected = currentServerUuid
                }
        ]
