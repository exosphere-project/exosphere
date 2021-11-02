module Page.Home exposing (Model, Msg, init, update, view)

import Element
import Element.Font as Font
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Helpers.Url as UrlHelpers
import Html.Attributes
import RemoteData
import Route
import Set
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Card exposing (exoCardFixedSize)
import Style.Widgets.Icon as Icon
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



-- TODO show, as separate cards, any unscoped providers that the user needs to choose projects for


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel _ =
    let
        uniqueKeystoneHostnames : List HelperTypes.KeystoneHostname
        uniqueKeystoneHostnames =
            sharedModel.projects
                |> List.map (.endpoints >> .keystone >> UrlHelpers.hostnameFromUrl)
                -- convert list to set and then back to remove duplicate values
                |> Set.fromList
                |> Set.toList
    in
    viewWithProjects context sharedModel uniqueKeystoneHostnames


viewWithProjects : View.Types.Context -> SharedModel -> List HelperTypes.KeystoneHostname -> Element.Element Msg
viewWithProjects context sharedModel uniqueKeystoneHostnames =
    let
        removeAllText =
            String.join " "
                [ "Remove All"
                , Helpers.String.toTitleCase
                    context.localization.unitOfTenancy
                    |> Helpers.String.pluralize
                ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 10
        , Element.spacing 20
        ]
        [ Element.row [ Element.width Element.fill, Element.spacing 25 ]
            [ Element.el
                (VH.heading2 context.palette
                    ++ [ Element.width Element.fill ]
                )
                (Element.text "Home")
            , if List.isEmpty uniqueKeystoneHostnames then
                Element.none

              else
                Element.el [ Element.alignRight ]
                    (Widget.iconButton
                        (SH.materialStyle context.palette).button
                        { icon =
                            Element.row [ Element.spacing 10 ]
                                [ Element.text removeAllText
                                , FeatherIcons.logOut |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                                ]
                        , text = removeAllText
                        , onPress =
                            Just Logout
                        }
                    )
            ]
        , if List.isEmpty uniqueKeystoneHostnames then
            Element.text <|
                String.join " "
                    [ "You are not logged into any"
                    , context.localization.unitOfTenancy |> Helpers.String.pluralize
                    , "yet."
                    ]

          else
            Element.none
        , Element.wrappedRow
            [ Element.spacing 24, Element.width (Element.maximum 900 Element.fill) ]
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
                    [ Element.centerX, Element.centerY, Element.spacing 24 ]
                    [ FeatherIcons.plusCircle
                        |> FeatherIcons.withSize 85
                        |> FeatherIcons.toHtml []
                        |> Element.html
                        |> Element.el
                            [ context.palette.muted
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
                [ Element.padding 10
                , Element.centerX
                , Font.bold
                ]
            <|
                Element.row [ Element.spacing 8 ]
                    [ Element.el
                        [ context.palette.muted
                            |> SH.toElementColor
                            |> Font.color
                        ]
                      <|
                        Element.text
                            (context.localization.unitOfTenancy
                                |> Helpers.String.toTitleCase
                            )
                    , Element.text <|
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
                ]
                (VH.ellipsizedText description)

        cloudSpecificConfig =
            GetterSetters.cloudSpecificConfigLookup context.cloudSpecificConfigs project

        ( friendlyName, friendlySubName ) =
            case cloudSpecificConfig of
                Nothing ->
                    ( UrlHelpers.hostnameFromUrl project.endpoints.keystone, Nothing )

                Just config ->
                    ( config.friendlyName, config.friendlySubName )

        renderCloudName =
            Element.wrappedRow
                [ Element.spacing 10
                , Element.centerX
                , Element.alignBottom
                ]
            <|
                case friendlySubName of
                    Just subName ->
                        [ Element.el
                            [ context.palette.muted
                                |> SH.toElementColor
                                |> Font.color
                            ]
                          <|
                            Element.text friendlyName
                        , Element.text subName
                        ]

                    Nothing ->
                        [ Element.text friendlyName ]

        title =
            Element.column
                [ Element.width Element.fill
                , Element.height (Element.px 110)
                , Element.paddingEach { top = 5, bottom = 10, left = 5, right = 5 }
                , Element.spacing 8
                ]
                [ renderProjectName
                , renderProjectDescription
                , renderCloudName
                ]

        renderResourceCount : String -> Element.Element Msg -> Int -> Element.Element Msg
        renderResourceCount resourceNameSingular icon count =
            Element.row [ Element.spacing 8 ]
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
                [ Element.height (Element.px 120), Element.padding 10, Element.spacing 12 ]
                [ renderResourceQuantity
                    context.localization.virtualComputer
                    (FeatherIcons.server |> FeatherIcons.toHtml [] |> Element.html |> Element.el [])
                    (RDPP.withDefault [] project.servers)
                , renderResourceQuantity
                    context.localization.blockDevice
                    (FeatherIcons.hardDrive |> FeatherIcons.toHtml [] |> Element.html |> Element.el [])
                    (RemoteData.withDefault [] project.volumes)
                , renderResourceQuantity
                    context.localization.floatingIpAddress
                    (Icon.ipAddress (SH.toElementColor context.palette.on.background) 24)
                    (RDPP.withDefault [] project.floatingIps)
                ]

        route =
            Route.toUrl context.urlPathPrefix
                (Route.ProjectRoute project.auth.project.uuid
                    Route.AllResourcesList
                )
    in
    Element.link []
        { url = route
        , label =
            card context.palette [ title, cardBody ]
        }


card : Style.Types.ExoPalette -> List (Element.Element Msg) -> Element.Element Msg
card palette content =
    exoCardFixedSize palette 320 250 content
