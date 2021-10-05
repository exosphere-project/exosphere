module Page.Home exposing (Model, Msg, init, update, view)

import Dict
import Element
import Element.Font as Font
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Helpers.Url as UrlHelpers
import RemoteData
import Route
import Set
import Style.Helpers as SH
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


init : Model
init =
    ()


update : Msg -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg model =
    ( model, Cmd.none, SharedMsg.NoOp )



-- TODO show, as separate cards, any unscoped providers that the user needs to choose projects for


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel model =
    let
        uniqueKeystoneHostnames : List HelperTypes.KeystoneHostname
        uniqueKeystoneHostnames =
            sharedModel.projects
                |> List.map .endpoints
                |> List.map .keystone
                |> List.map UrlHelpers.hostnameFromUrl
                -- convert list to set and then back to remove duplicate values
                |> Set.fromList
                |> Set.toList
    in
    if List.isEmpty sharedModel.projects then
        Element.column
            [ Element.padding 10, Element.spacing 24, Element.centerX ]
            [ Element.text <|
                String.join " "
                    [ "You are not logged into any"
                    , context.localization.unitOfTenancy
                        |> Helpers.String.pluralize
                    , "yet."
                    ]
            , Element.link
                [ Element.centerX ]
                { url =
                    Route.toUrl context.urlPathPrefix
                        (Route.defaultLoginPage
                            sharedModel.style.defaultLoginView
                        )
                , label =
                    Widget.textButton
                        (SH.materialStyle context.palette).button
                        { text = "Add " ++ context.localization.unitOfTenancy
                        , onPress = Just <| NoOp
                        }
                }
            ]

    else
        Element.column [ Element.padding 10, Element.spacing 10, Element.width Element.fill ]
            [ Element.el (VH.heading2 context.palette) <| Element.text "Clouds"
            , Element.column
                [ Element.padding 10, Element.spacingXY 0 60 ]
                (List.map (renderCloud context sharedModel) uniqueKeystoneHostnames)
            ]


renderCloud : View.Types.Context -> SharedModel -> HelperTypes.KeystoneHostname -> Element.Element Msg
renderCloud context sharedModel keystoneHostname =
    let
        projects =
            GetterSetters.projectsForCloud sharedModel keystoneHostname

        maybeCloudSpecificConfig =
            Dict.get keystoneHostname context.cloudSpecificConfigs

        friendlyCloudName =
            case maybeCloudSpecificConfig of
                Nothing ->
                    keystoneHostname

                Just cloudSpecificConfig ->
                    cloudSpecificConfig.friendlyName
                        ++ (case cloudSpecificConfig.friendlySubName of
                                Nothing ->
                                    ""

                                Just subName ->
                                    ": " ++ subName
                           )
    in
    Element.column
        [ Element.width Element.fill, Element.spacingXY 0 24 ]
    <|
        [ Element.row (VH.heading3 context.palette ++ [ Element.spacing 15 ])
            [ FeatherIcons.cloud |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Element.text friendlyCloudName
            ]
        , Element.wrappedRow [ Element.spacing 24 ] (projects |> List.map (renderProject context))
        ]


renderProject : View.Types.Context -> Project -> Element.Element Msg
renderProject context project =
    -- TODO make entire card clickable
    let
        cardBody =
            let
                renderResourceQuantity : String -> Element.Element Msg -> List a -> Element.Element Msg
                renderResourceQuantity resourceNameSingular icon resourceList =
                    let
                        resourceQuantity =
                            List.length resourceList
                    in
                    if resourceQuantity > 0 then
                        Element.row [ Element.spacing 8 ]
                            [ icon
                            , Element.text <|
                                String.join " "
                                    [ String.fromInt resourceQuantity
                                    , resourceNameSingular
                                        |> (if resourceQuantity > 1 then
                                                Helpers.String.pluralize

                                            else
                                                identity
                                           )
                                    ]
                            ]

                    else
                        Element.none
            in
            Element.column
                [ Element.spacing 12 ]
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
            Widget.column
                (SH.materialStyle context.palette).cardColumn
                [ Element.column
                    [ Element.centerX
                    , Element.paddingXY 10 15
                    , Element.spacing 15
                    ]
                  <|
                    [ Element.el
                        [ Element.centerX
                        , Font.bold
                        ]
                      <|
                        Element.text <|
                            String.join " "
                                [ context.localization.unitOfTenancy
                                    |> Helpers.String.toTitleCase
                                , project.auth.project.name
                                ]
                    ]
                , Element.column
                    [ Element.padding 10
                    , Element.spacing 10
                    , Element.centerX
                    ]
                    [ cardBody
                    ]
                ]
        }
