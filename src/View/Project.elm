module View.Project exposing (projectView)

import Element
import Framework.Button as Button
import Framework.Modifier as Modifier
import Helpers.Helpers as Helpers
import Maybe
import Types.Types exposing (..)
import View.CreateServer
import View.Helpers as VH
import View.Images
import View.Servers


projectView : Model -> Project -> ProjectViewConstructor -> Element.Element Msg
projectView model project viewConstructor =
    let
        v =
            case viewConstructor of
                ListImages ->
                    View.Images.viewImagesIfLoaded model.globalDefaults project model.imageFilterTag

                ListProjectServers ->
                    View.Servers.viewServers project

                ServerDetail serverUuid viewStateParams ->
                    View.Servers.viewServerDetail project serverUuid viewStateParams

                CreateServer createServerRequest ->
                    View.CreateServer.viewCreateServer project createServerRequest
    in
    Element.column
        (Element.width Element.fill
            :: VH.exoColumnAttributes
        )
        [ viewProjectNav project
        , v
        ]


viewProjectNav : Project -> Element.Element Msg
viewProjectNav project =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el
            VH.heading2
          <|
            Element.text <|
                Helpers.hostnameFromUrl project.creds.authUrl
                    ++ " - "
                    ++ project.creds.projectName
        , Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ Element.el
                []
                (Button.button
                    []
                    (Just <|
                        ProjectMsg (Helpers.getProjectId project) <|
                            SetProjectView ListProjectServers
                    )
                    "My Servers"
                )
            , Element.el []
                (Button.button
                    []
                    (Just <| ProjectMsg (Helpers.getProjectId project) <| SetProjectView ListImages)
                    "Create Server"
                )
            , Element.el
                [ Element.alignRight ]
                (Button.button [ Modifier.Muted ] (Just <| ProjectMsg (Helpers.getProjectId project) RemoveProject) "Remove Project")
            ]
        ]
