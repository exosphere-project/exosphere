module View.Nav exposing (navBar, navBarHeight, navMenu, navMenuWidth)

import Color
import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Framework.Color
import Helpers.Helpers as Helpers
import Style.Widgets.Icon as Icon
import Style.Widgets.MenuItem as MenuItem
import Types.Types
    exposing
        ( Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import View.Helpers as VH


navMenuWidth : Int
navMenuWidth =
    180


navBarHeight : Int
navBarHeight =
    70


navMenu : Model -> Element.Element Msg
navMenu model =
    let
        projectMenuItem : Project -> Element.Element Msg
        projectMenuItem project =
            let
                projectTitle =
                    projectTitleForNavMenu model project

                status =
                    case model.viewState of
                        ProjectView p _ ->
                            if p == Helpers.getProjectId project then
                                MenuItem.Active

                            else
                                MenuItem.Inactive

                        _ ->
                            MenuItem.Inactive
            in
            MenuItem.menuItem status projectTitle (Just (ProjectMsg (Helpers.getProjectId project) (SetProjectView ListProjectServers)))

        projectMenuItems : List Project -> List (Element.Element Msg)
        projectMenuItems projects =
            List.map projectMenuItem projects

        addProjectMenuItem =
            let
                active =
                    case model.viewState of
                        NonProjectView LoginPicker ->
                            MenuItem.Active

                        NonProjectView (LoginOpenstack _) ->
                            MenuItem.Active

                        NonProjectView (LoginJetstream _) ->
                            MenuItem.Active

                        _ ->
                            MenuItem.Inactive
            in
            MenuItem.menuItem active "Add Project" (Just (SetNonProjectView LoginPicker))
    in
    Element.column
        [ Background.color <| Color.toElementColor <| Framework.Color.black_ter
        , Font.color (Element.rgb255 209 209 209)
        , Element.width (Element.px navMenuWidth)
        , Element.height Element.shrink
        , Element.scrollbarY
        , Element.height Element.fill
        ]
        (projectMenuItems model.projects
            ++ [ addProjectMenuItem ]
        )


projectTitleForNavMenu : Model -> Project -> String
projectTitleForNavMenu model project =
    -- If we have multiple projects on the same provider then append the project name to the provider name
    let
        providerTitle =
            project.creds.authUrl
                |> Helpers.hostnameFromUrl
                |> Helpers.titleFromHostname

        multipleProjects =
            let
                projectCountOnSameProvider =
                    let
                        projectsOnSameProvider : Project -> Project -> Bool
                        projectsOnSameProvider proj1 proj2 =
                            Helpers.hostnameFromUrl proj1.creds.authUrl == Helpers.hostnameFromUrl proj2.creds.authUrl
                    in
                    List.filter (projectsOnSameProvider project) model.projects
                        |> List.length
            in
            projectCountOnSameProvider > 1
    in
    if multipleProjects then
        providerTitle ++ String.fromChar '\n' ++ "(" ++ project.creds.projectName ++ ")"

    else
        providerTitle


navBar : Model -> Element.Element Msg
navBar _ =
    let
        navBarContainerAttributes =
            [ Background.color (Element.rgb255 29 29 29)
            , Element.width Element.fill
            , Element.height (Element.px navBarHeight)
            ]

        -- TODO: Responsiveness - Depending on how wide the screen is, return Element.column for navBarContainerElement.
        -- https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#responsiveness
        navBarContainerElement =
            Element.row

        navBarBrand =
            Element.row
                [ Element.padding 10
                , Element.spacing 20
                ]
                [ Element.el
                    [ Region.heading 1
                    , Font.bold
                    , Font.size 26
                    , Font.color (Element.rgb 1 1 1)
                    ]
                    (Element.text "exosphere")
                , Element.image [ Element.height (Element.px 40) ] { src = "https://exosphere.gitlab.io/exosphere/assets/img/logo-alt.svg", description = "" }
                ]

        navBarRight =
            Element.row
                [ Element.alignRight, Element.paddingXY 20 0, Element.spacing 15 ]
                [ Element.el
                    [ Font.color (Element.rgb255 209 209 209)
                    ]
                    (Element.text "")
                , Element.el
                    [ Font.color (Element.rgb255 209 209 209)
                    ]
                    (Input.button
                        []
                        { onPress = Just (SetNonProjectView MessageLog)
                        , label =
                            Element.row
                                (VH.exoRowAttributes ++ [ Element.spacing 8 ])
                                [ Icon.bell Framework.Color.white 20
                                , Element.text "Messages"
                                ]
                        }
                    )
                , Element.el
                    [ Font.color (Element.rgb255 209 209 209)
                    ]
                    (Input.button
                        []
                        { onPress = Just (SetNonProjectView HelpAbout)
                        , label =
                            Element.row
                                (VH.exoRowAttributes ++ [ Element.spacing 8 ])
                                [ Icon.question Framework.Color.white 20
                                , Element.text "Help / About"
                                ]
                        }
                    )

                -- This is where the right-hand side menu would go
                ]

        navBarHeaderView =
            Element.row
                [ Element.padding 10
                , Element.spacing 10
                , Element.height (Element.px navBarHeight)
                , Element.width Element.fill
                ]
                [ navBarBrand
                , navBarRight
                ]
    in
    navBarContainerElement
        navBarContainerAttributes
        [ navBarHeaderView ]
