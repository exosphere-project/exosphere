module View.Breadcrumb exposing (breadcrumb)

import Element
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Route
import Style.Helpers as SH
import Types.OuterModel
import Types.SharedMsg exposing (SharedMsg)
import Types.View
    exposing
        ( LoginView(..)
        , NonProjectViewConstructor(..)
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import View.Helpers
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
                Element.link
                    (View.Helpers.linkAttribs context)
                    { url = Route.toUrl context.urlPathPrefix route
                    , label = Element.text item.label
                    }

            Nothing ->
                Element.text item.label


breadcrumb : Types.OuterModel.OuterModel -> View.Types.Context -> Element.Element SharedMsg
breadcrumb outerModel context =
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
                              , label = View.PageTitle.pageTitle outerModel context
                              }
                            ]

                        _ ->
                            [ { route = Nothing
                              , label = View.PageTitle.pageTitle outerModel context
                              }
                            ]

                ProjectView projectId _ constructor ->
                    let
                        projectItem =
                            case GetterSetters.projectLookup outerModel.sharedModel projectId of
                                Just project ->
                                    [ { route = Just <| Route.ProjectRoute projectId <| Route.AllResourcesList
                                      , label = "Project " ++ project.auth.project.name
                                      }
                                    ]

                                Nothing ->
                                    [ { route = Nothing
                                      , label = "Unknown project"
                                      }
                                    ]

                        projectPage =
                            case constructor of
                                AllResourcesList _ ->
                                    []

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
            { route = Nothing, label = "Home" }

        lastItem : Maybe Item
        lastItem =
            viewStateItems |> List.reverse |> List.head

        restOfItems : List Item
        restOfItems =
            viewStateItems |> List.reverse |> List.tail |> Maybe.map List.reverse |> Maybe.withDefault []

        separator : Element.Element msg
        separator =
            Element.el [ Font.color (context.palette.muted |> SH.toElementColor) ] <| Element.text ">"
    in
    Element.wrappedRow
        [ Element.paddingXY 10 4
        , Element.spacingXY 10 4
        , Font.size 15
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