module Page.Home exposing (Model, Msg, headerView, init, update, view)

import Element
import Element.Font as Font
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Helpers.Url as UrlHelpers
import Route
import Set
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Card exposing (clickableCardFixedSize)
import Style.Widgets.DeleteButton
import Style.Widgets.Icon as Icon
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.HelperTypes as HelperTypes
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    ()


type Msg
    = NoOp
    | Logout
    | TogglePopover String


init : Model
init =
    ()


update : Msg -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )

        Logout ->
            ( model, Cmd.none, SharedMsg.Logout )

        TogglePopover id ->
            ( model, Cmd.none, SharedMsg.TogglePopover id )


uniqueKeystoneHostnames : SharedModel -> List HelperTypes.KeystoneHostname
uniqueKeystoneHostnames sharedModel =
    sharedModel.projects
        |> List.map (.endpoints >> .keystone >> UrlHelpers.hostnameFromUrl)
        -- convert list to set and then back to remove duplicate values
        |> Set.fromList
        |> Set.toList


headerView : View.Types.Context -> SharedModel -> Element.Element Msg
headerView context sharedModel =
    let
        removeAllText =
            String.join " "
                [ "Remove All"
                , Helpers.String.toTitleCase
                    context.localization.unitOfTenancy
                    |> Helpers.String.pluralize
                ]

        removePopconfirmId =
            "RemoveAllProjects"
    in
    Element.row [ Element.width Element.fill, Element.spacing spacer.px24 ]
        [ Text.heading context.palette
            VH.headerHeadingAttributes
            Element.none
            "Home"
        , if List.isEmpty <| uniqueKeystoneHostnames sharedModel then
            Element.none

          else
            Style.Widgets.DeleteButton.deletePopconfirm context
                TogglePopover
                removePopconfirmId
                { confirmation =
                    Element.column
                        [ Element.spacing spacer.px8
                        , Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)
                        ]
                        [ "Are you sure you want to remove all "
                            ++ (context.localization.unitOfTenancy
                                    |> Helpers.String.pluralize
                               )
                            ++ "?"
                            |> Text.body
                        , "Nothing will be deleted on the cloud, only from the view." |> Text.text Text.Small []
                        , (context.localization.unitOfTenancy
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                          )
                            ++ " can be added back later."
                            |> Text.text Text.Small []
                        ]
                , buttonText = Just "Remove"
                , onConfirm = Just Logout
                , onCancel = Just NoOp
                }
                Style.Types.PositionLeftTop
                (\toggle _ ->
                    Widget.iconButton
                        (SH.materialStyle context.palette).button
                        { icon =
                            Element.row [ Element.spacing spacer.px8 ]
                                [ Element.text removeAllText
                                , FeatherIcons.logOut |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                                ]
                        , text = removeAllText
                        , onPress =
                            Just toggle
                        }
                )
                |> Element.el [ Element.alignRight ]
        ]



-- TODO show, as separate cards, any unscoped providers that the user needs to choose projects for


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel _ =
    Element.column
        [ Element.width Element.fill
        , Element.spacing spacer.px24
        ]
        [ if List.isEmpty <| uniqueKeystoneHostnames sharedModel then
            Element.text <|
                String.join " "
                    [ "You are not logged into any"
                    , context.localization.unitOfTenancy |> Helpers.String.pluralize
                    , "yet."
                    ]

          else
            Element.none
        , Element.wrappedRow
            [ Element.spacing spacer.px24 ]
            (List.append (List.map (renderProject context) sharedModel.projects) [ addProjectCard context sharedModel ])
        ]


addProjectCard : View.Types.Context -> SharedModel -> Element.Element Msg
addProjectCard context sharedModel =
    Element.link []
        { url =
            Route.toUrl context.urlPathPrefix
                (Route.defaultLoginPage
                    sharedModel.style.defaultLoginView
                )
        , label =
            card
                context.palette
                [ Element.column
                    [ Element.centerX, Element.centerY, Element.spacing spacer.px24 ]
                    [ FeatherIcons.plusCircle
                        |> FeatherIcons.withSize 85
                        |> FeatherIcons.toHtml []
                        |> Element.html
                        |> Element.el
                            [ context.palette.neutral.icon
                                |> SH.toElementColor
                                |> Font.color
                            ]
                    , Element.text ("Add " ++ context.localization.unitOfTenancy)
                    ]
                ]
        }


renderProject : View.Types.Context -> Project -> Element.Element Msg
renderProject context project =
    let
        renderProjectName =
            Element.el
                [ Element.padding spacer.px8
                , Element.centerX
                ]
            <|
                Element.row [ Element.spacing spacer.px8 ]
                    [ Element.el
                        [ context.palette.neutral.text.subdued
                            |> SH.toElementColor
                            |> Font.color
                        ]
                      <|
                        Text.strong
                            (context.localization.unitOfTenancy
                                |> Helpers.String.toTitleCase
                            )
                    , Text.strong <|
                        project.auth.project.name
                    ]

        renderProjectDescription =
            case project.description of
                Nothing ->
                    Element.none

                Just description ->
                    if description == "" then
                        Element.none

                    else
                        renderProjectDescription_ description

        renderProjectDescription_ description =
            Element.el
                [ Element.height (Element.px 25)
                , Element.centerX
                , Element.width Element.fill
                , context.palette.neutral.text.subdued
                    |> SH.toElementColor
                    |> Font.color
                ]
                (VH.ellipsizedText description)

        friendlyName =
            VH.friendlyCloudName context project

        renderCloudName =
            Element.wrappedRow
                [ Element.spacing spacer.px8
                , Element.centerX
                , Element.alignBottom
                ]
            <|
                [ Element.text friendlyName ]

        title =
            Element.column
                [ Element.width Element.fill
                , Element.height (Element.px 110)
                , Element.paddingEach { top = spacer.px8, bottom = spacer.px12, left = spacer.px8, right = spacer.px8 }
                , Element.spacing spacer.px8
                ]
                [ renderProjectName
                , renderProjectDescription
                , renderCloudName
                ]

        renderResourceCount : String -> Element.Element Msg -> Int -> Element.Element Msg
        renderResourceCount resourceNameSingular icon count =
            Element.row [ Element.spacing spacer.px8 ]
                [ icon
                , Element.text <|
                    String.join " "
                        [ String.fromInt count
                        , if count > 1 then
                            Helpers.String.pluralize resourceNameSingular

                          else
                            resourceNameSingular
                        ]
                ]

        renderResourceQuantity name icon list =
            case List.length list of
                0 ->
                    Element.none

                n ->
                    renderResourceCount name icon n

        cardBody =
            Element.column
                [ Element.height (Element.px 120)
                , Element.padding spacer.px8
                , Element.spacing spacer.px12
                ]
                [ renderResourceQuantity
                    context.localization.virtualComputer
                    (FeatherIcons.server |> FeatherIcons.toHtml [] |> Element.html |> Element.el [])
                    (RDPP.withDefault [] project.servers)
                , renderResourceQuantity
                    context.localization.blockDevice
                    (FeatherIcons.hardDrive |> FeatherIcons.toHtml [] |> Element.html |> Element.el [])
                    (RDPP.withDefault [] project.volumes)
                , renderResourceQuantity
                    context.localization.floatingIpAddress
                    (Icon.ipAddress (SH.toElementColor context.palette.neutral.text.default) 24)
                    (RDPP.withDefault [] project.floatingIps)
                ]

        route =
            Route.toUrl context.urlPathPrefix
                (Route.ProjectRoute (GetterSetters.projectIdentifier project)
                    Route.ProjectOverview
                )
    in
    Element.link []
        { url = route
        , label =
            card context.palette [ title, cardBody ]
        }


card : Style.Types.ExoPalette -> List (Element.Element Msg) -> Element.Element Msg
card palette content =
    -- TODO: height shouldn't be hardcoded becuase it needs to be changed everytime content (and its spacing & padding) is changed
    clickableCardFixedSize palette 320 265 content
