module View.Breadcrumb exposing (breadcrumb)

import Element
import Element.Font as Font
import FeatherIcons as Icons
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Route
import Style.Helpers as SH
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Link as Link
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.OuterModel
import Types.SharedMsg exposing (SharedMsg)
import Types.View
    exposing
        ( NonProjectViewConstructor(..)
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import View.PageTitle
import View.Types



-- TODO consider some central translation of view state to route


type alias Item =
    { route : Maybe Route.Route
    , label : String
    }


renderItem : Bool -> View.Types.Context -> Item -> Element.Element SharedMsg
renderItem disableClick context item =
    if disableClick then
        Element.text item.label

    else
        case item.route of
            Just route ->
                Link.navigate Link.Direct context.palette (Route.toUrl context.urlPathPrefix route) (Element.text item.label)

            Nothing ->
                Element.text item.label


breadcrumb : Types.OuterModel.OuterModel -> View.Types.Context -> Element.Element SharedMsg
breadcrumb outerModel context =
    -- Home page doesn't need a breadcrumb
    case outerModel.viewState of
        NonProjectView (Home _) ->
            Element.none

        _ ->
            breadcrumb_ outerModel context


breadcrumb_ : Types.OuterModel.OuterModel -> View.Types.Context -> Element.Element SharedMsg
breadcrumb_ outerModel context =
    let
        viewStateItems : List Item
        viewStateItems =
            case outerModel.viewState of
                NonProjectView constructor ->
                    case constructor of
                        Login _ ->
                            [ { route = Just <| Route.LoginPicker
                              , label = "Log in"
                              }
                            , { route = Nothing
                              , label = View.PageTitle.pageTitle outerModel
                              }
                            ]

                        _ ->
                            [ { route = Nothing
                              , label = View.PageTitle.pageTitle outerModel
                              }
                            ]

                ProjectView projectId constructor ->
                    let
                        projectItem =
                            case GetterSetters.projectLookup outerModel.sharedModel projectId of
                                Just project ->
                                    [ { route = Just <| Route.ProjectRoute projectId <| Route.ProjectOverview
                                      , label =
                                            Helpers.String.toTitleCase context.localization.unitOfTenancy
                                                ++ " "
                                                ++ project.auth.project.name
                                      }
                                    ]

                                Nothing ->
                                    [ { route = Nothing
                                      , label = "Unknown " ++ context.localization.unitOfTenancy
                                      }
                                    ]

                        projectPage =
                            case constructor of
                                ProjectOverview _ ->
                                    []

                                Credentials _ ->
                                    [ { route = Nothing
                                      , label = context.localization.credential |> Helpers.String.pluralize |> Helpers.String.toTitleCase
                                      }
                                    ]

                                FloatingIpAssign _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ "Assign"
                                                , context.localization.floatingIpAddress
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                FloatingIpList _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.floatingIpAddress
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                ImageList _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.staticRepresentationOfBlockDeviceContents
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                InstanceSourcePicker _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.virtualComputer
                                                    |> Helpers.String.toTitleCase
                                                , "sources"
                                                ]
                                      }
                                    ]

                                FloatingIpCreate _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ "Create"
                                                , context.localization.floatingIpAddress
                                                ]
                                      }
                                    ]

                                KeypairCreate _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ "Upload"
                                                , context.localization.pkiPublicKeyForSsh
                                                ]
                                      }
                                    ]

                                KeypairList _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.pkiPublicKeyForSsh
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                SecurityGroupDetail pageModel ->
                                    [ { route = Just <| Route.ProjectRoute projectId <| Route.SecurityGroupList
                                      , label =
                                            String.join " "
                                                [ context.localization.securityGroup
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    , { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.securityGroup
                                                    |> Helpers.String.toTitleCase
                                                , View.PageTitle.securityGroupName (GetterSetters.projectLookup outerModel.sharedModel projectId) pageModel.securityGroupUuid
                                                ]
                                      }
                                    ]

                                SecurityGroupList _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.securityGroup
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                ServerCreate _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ "Create"
                                                , context.localization.virtualComputer
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                ServerCreateImage _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ "Create"
                                                , context.localization.staticRepresentationOfBlockDeviceContents
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                ServerDetail pageModel ->
                                    [ { route = Just <| Route.ProjectRoute projectId <| Route.ServerList
                                      , label =
                                            String.join " "
                                                [ context.localization.virtualComputer
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    , { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.virtualComputer
                                                    |> Helpers.String.toTitleCase
                                                , View.PageTitle.serverName (GetterSetters.projectLookup outerModel.sharedModel projectId) pageModel.serverUuid
                                                ]
                                      }
                                    ]

                                ServerList _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.virtualComputer
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                ServerResize pageModel ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ "Resize"
                                                , context.localization.virtualComputer
                                                    |> Helpers.String.toTitleCase
                                                , View.PageTitle.serverName (GetterSetters.projectLookup outerModel.sharedModel projectId) pageModel.serverUuid
                                                ]
                                      }
                                    ]

                                ServerSecurityGroups pageModel ->
                                    [ { route = Just <| Route.ProjectRoute projectId <| Route.ServerList
                                      , label =
                                            String.join " "
                                                [ context.localization.virtualComputer
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    , { route = Just <| Route.ProjectRoute projectId <| Route.ServerDetail pageModel.serverUuid
                                      , label =
                                            String.join " "
                                                [ context.localization.virtualComputer
                                                    |> Helpers.String.toTitleCase
                                                , View.PageTitle.serverName (GetterSetters.projectLookup outerModel.sharedModel projectId) pageModel.serverUuid
                                                ]
                                      }
                                    , { route = Nothing
                                      , label =
                                            context.localization.securityGroup
                                                |> Helpers.String.pluralize
                                                |> Helpers.String.toTitleCase
                                      }
                                    ]

                                ShareCreate _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ "Create"
                                                , context.localization.share
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                ShareDetail pageModel ->
                                    [ { route = Just <| Route.ProjectRoute projectId <| Route.ShareList
                                      , label =
                                            String.join " "
                                                [ context.localization.share
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    , { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.share
                                                    |> Helpers.String.toTitleCase
                                                , View.PageTitle.shareName (GetterSetters.projectLookup outerModel.sharedModel projectId) pageModel.shareUuid
                                                ]
                                      }
                                    ]

                                ShareList _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.share
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                VolumeAttach _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ "Attach"
                                                , context.localization.blockDevice
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                VolumeCreate _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ "Create"
                                                , context.localization.blockDevice
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                VolumeDetail pageModel ->
                                    [ { route = Just <| Route.ProjectRoute projectId <| Route.VolumeList
                                      , label =
                                            String.join " "
                                                [ context.localization.blockDevice
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    , { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.blockDevice
                                                    |> Helpers.String.toTitleCase
                                                , View.PageTitle.volumeName (GetterSetters.projectLookup outerModel.sharedModel projectId) pageModel.volumeUuid
                                                ]
                                      }
                                    ]

                                VolumeList _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ context.localization.blockDevice
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]

                                VolumeMountInstructions _ ->
                                    [ { route = Nothing
                                      , label =
                                            String.join " "
                                                [ "Mount"
                                                , context.localization.blockDevice
                                                    |> Helpers.String.toTitleCase
                                                ]
                                      }
                                    ]
                    in
                    List.concat
                        [ projectItem
                        , projectPage
                        ]

        firstItem : Item
        firstItem =
            { route = Just Route.Home, label = "Home" }

        ( lastItem, restOfItems ) =
            case List.reverse viewStateItems of
                [] ->
                    ( Nothing, [] )

                head :: tail ->
                    ( Just head, List.reverse tail )

        separator : Element.Element msg
        separator =
            featherIcon
                [ Font.color (context.palette.neutral.icon |> SH.toElementColor) ]
                (Icons.chevronRight |> Icons.withSize 14)
    in
    Element.wrappedRow
        [ Element.spacing spacer.px8
        , Text.fontSize Text.Small
        ]
    <|
        List.intersperse
            separator
        <|
            List.concat
                [ List.map (renderItem False context) <|
                    firstItem
                        :: restOfItems
                , case lastItem of
                    Just item ->
                        [ renderItem True context item ]

                    Nothing ->
                        []
                ]
