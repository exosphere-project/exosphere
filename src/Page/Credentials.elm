module Page.Credentials exposing (Model, init, view)

import Element
import FeatherIcons as Icons
import Helpers.Credentials as Credentials
import Helpers.String
import Helpers.Url
import Style.Widgets.Code exposing (codeSpan, copyableCodeSpan)
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Link as Link
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.Project exposing (Project)
import View.Types


type alias Model =
    ()


init : Project -> Model
init _ =
    ()


view : View.Types.Context -> Project -> Model -> Element.Element msg
view context project _ =
    let
        openStackClient =
            Link.externalLink context.palette "https://docs.openstack.org/python-openstackclient/latest" "OpenStackClient"

        styledCodeSpan =
            codeSpan context.palette

        fileNames =
            { sh = Credentials.projectCloudName project ++ "-openrc.sh"
            , ps1 = Credentials.projectCloudName project ++ "-openrc.ps1"
            }
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing spacer.px24
        ]
        [ Element.row [ Element.width Element.fill ]
            [ Text.heading context.palette
                []
                (featherIcon [] Icons.terminal)
                (String.join " "
                    [ "Download"
                    , context.localization.credential |> Helpers.String.pluralize |> Helpers.String.toTitleCase
                    , "for"
                    , context.localization.commandDrivenTextInterface
                    , "access"
                    ]
                )
            ]
        , Element.paragraph [ Element.width Element.fill ]
            [ Element.text "Choose "
            , Element.text <| Helpers.String.indefiniteArticle context.localization.credential ++ " "
            , Element.text <| Helpers.String.pluralize context.localization.credential
            , Element.text " format to download and use with "
            , openStackClient
            , Element.text " or other tools."
            ]
        , Element.paragraph [ Element.width Element.fill ]
            [ Element.text "To use use OpenStackClient in your "
            , Element.text context.localization.commandDrivenTextInterface
            , Element.text ", you must "
            , Link.externalLink context.palette "https://docs.jetstream-cloud.org/ui/cli/clients/" "install it on your computer first."
            ]
        , Element.textColumn
            [ Element.spacing spacer.px8, Element.width Element.fill ]
            [ Element.paragraph []
                [ Element.downloadAs (Link.linkStyle context.palette)
                    { label = Text.strong "clouds.yaml"
                    , filename = "clouds.yaml"
                    , url = Helpers.Url.textDataUrl (Credentials.getCloudsYaml [ project ])
                    }
                , Element.text ", a modern "
                , Element.text <| Helpers.String.pluralize context.localization.credential
                , Element.text " format."
                ]
            , Element.paragraph [ Element.moveRight 24, Element.width Element.fill ]
                [ openStackClient
                , Element.text " will look for a clouds.yaml in the current working directory, followed by "
                , styledCodeSpan "~/.config/openstack"
                , Element.text " or "
                , styledCodeSpan "%LocalAppData%\\OpenStack\\openstack"
                , Element.text " on Windows."
                ]
            , Element.paragraph [ Element.moveRight 24, Element.width Element.fill ]
                [ Element.text "Place your clouds.yaml in one of these locations, and run "
                , copyableCodeSpan context.palette
                    ("openstack --os-cloud=" ++ Credentials.projectCloudName project)
                , Element.text " to get started."
                ]
            , Element.paragraph [ Element.moveRight 24, Element.width Element.fill ]
                [ Element.text "See the "
                , Link.externalLink context.palette "https://docs.openstack.org/python-openstackclient/latest/configuration/index.html#configuration-files" "OpenStack documentation"
                , Element.text " for more information."
                ]
            ]
        , Element.textColumn
            [ Element.spacing spacer.px8, Element.width Element.fill ]
            [ Element.paragraph []
                [ Element.downloadAs (Link.linkStyle context.palette)
                    { label = Text.strong "openrc.sh"
                    , filename = fileNames.sh
                    , url = Helpers.Url.textDataUrl (Credentials.getOpenRcSh project)
                    }
                , Element.text ", classic format for Linux and macOS"
                ]
            , Element.paragraph [ Element.moveRight 24, Element.width Element.fill ]
                [ Element.text ("In your " ++ context.localization.commandDrivenTextInterface ++ ", run ")
                , styledCodeSpan ("source " ++ fileNames.sh)
                , Element.text ", then any following "
                , styledCodeSpan "openstack"
                , Element.text (" commands in the same " ++ context.localization.commandDrivenTextInterface ++ " window will be authenticated")
                ]
            ]
        , Element.textColumn
            [ Element.spacing spacer.px8, Element.width Element.fill ]
            [ Element.paragraph []
                [ Element.downloadAs (Link.linkStyle context.palette)
                    { label = Text.strong "openrc.ps1"
                    , filename = fileNames.ps1
                    , url = Helpers.Url.textDataUrl (Credentials.getOpenRcPs1 project)
                    }
                , Element.text ", classic format for Windows PowerShell"
                ]
            , Element.paragraph [ Element.moveRight 24, Element.width Element.fill ]
                [ Element.text "In a powershell window, run "
                , styledCodeSpan (".\\" ++ fileNames.ps1)
                , Element.text ", then any following "
                , styledCodeSpan "openstack"
                , Element.text (" commands in the same " ++ context.localization.commandDrivenTextInterface ++ " window will be authenticated")
                ]
            ]
        ]
