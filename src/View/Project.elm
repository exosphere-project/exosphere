module View.Project exposing (project)

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


project : Model -> Project -> ProjectViewConstructor -> Element.Element Msg
project model p viewConstructor =
    let
        v =
            case viewConstructor of
                ListImages ->
                    View.Images.imagesIfLoaded model.globalDefaults p model.imageFilterTag

                ListProjectServers ->
                    View.Servers.servers p

                ServerDetail serverUuid viewStateParams ->
                    View.Servers.serverDetail p serverUuid viewStateParams

                CreateServer createServerRequest ->
                    View.CreateServer.createServer p createServerRequest
    in
    Element.column
        (Element.width Element.fill
            :: VH.exoColumnAttributes
        )
        [ projectNav p
        , v
        ]


projectNav : Project -> Element.Element Msg
projectNav p =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el
            VH.heading2
          <|
            Element.text <|
                Helpers.hostnameFromUrl p.creds.authUrl
                    ++ " - "
                    ++ p.creds.projectName
        , Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ Element.el
                []
                (Button.button
                    []
                    (Just <|
                        ProjectMsg (Helpers.getProjectId p) <|
                            SetProjectView ListProjectServers
                    )
                    "My Servers"
                )
            , Element.el []
                (Button.button
                    []
                    (Just <| ProjectMsg (Helpers.getProjectId p) <| SetProjectView ListImages)
                    "Create Server"
                )
            , Element.el
                [ Element.alignRight ]
                (Button.button [ Modifier.Muted ] (Just <| ProjectMsg (Helpers.getProjectId p) RemoveProject) "Remove Project")
            ]
        ]
