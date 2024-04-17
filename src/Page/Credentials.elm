module Page.Credentials exposing (Model, Msg, init, update, view)

import Element
import Element.Background
import FeatherIcons as Icons
import Helpers.Credentials as Credentials
import Helpers.String
import Helpers.Url
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon as Icon
import Style.Widgets.Link as Link
import Style.Widgets.Popover.Popover exposing (dropdownItemStyle, popover)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Types
import Widget


type alias Model =
    ()


type Msg
    = SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Project -> Model
init _ =
    ()


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ _ =
    case msg of
        SharedMsg sharedMsg ->
            ( (), Cmd.none, sharedMsg )

        _ ->
            ( (), Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project _ =
    let
        openStackClient =
            Link.externalLink context.palette "https://docs.openstack.org/python-openstackclient/latest" "OpenStackClient"

        highlightedMonoText =
            Text.mono
                >> Element.el [ Element.Background.color <| SH.toElementColor <| context.palette.neutral.background.frontLayer ]

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
            [ Text.heading context.palette [] Element.none <|
                String.join " "
                    [ "Download"
                    , context.localization.credential |> Helpers.String.pluralize |> Helpers.String.toTitleCase
                    , "for"
                    , context.localization.commandDrivenTextInterface
                    , "access"
                    ]
            , Element.el [ Element.moveUp 1 ] <|
                credentialsDropdown context project
            ]
        , Element.paragraph [ Element.width Element.fill ]
            [ Element.text "Choose a "
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
                [ Text.strong "clouds.yaml"
                , Element.text ", a modern "
                , Element.text <| Helpers.String.pluralize context.localization.credential
                , Element.text " format."
                ]
            , Element.paragraph [ Element.moveRight 24, Element.width Element.fill ]
                [ openStackClient
                , Element.text " will look for a clouds.yaml in the current working directory, followed by "
                , highlightedMonoText "~/.config/openstack"
                , Element.text " or "
                , highlightedMonoText "%LocalAppData%\\OpenStack\\openstack"
                , Element.text " on Windows."
                ]
            , Element.paragraph [ Element.moveRight 24, Element.width Element.fill ]
                [ Element.text "Place your clouds.yaml in one of these locations, and run "
                , copyableText context.palette
                    [ Text.fontFamily Text.Mono
                    , Element.Background.color <| SH.toElementColor <| context.palette.neutral.background.frontLayer
                    ]
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
                [ Text.strong "openrc.sh"
                , Element.text ", classic format for Linux and macOS"
                ]
            , Element.paragraph [ Element.moveRight 24, Element.width Element.fill ]
                [ Element.text ("In your " ++ context.localization.commandDrivenTextInterface ++ ", run ")
                , highlightedMonoText ("source " ++ fileNames.sh)
                , Element.text ", then any following "
                , highlightedMonoText "openstack"
                , Element.text (" commands in the same " ++ context.localization.commandDrivenTextInterface ++ " window will be authenticated")
                ]
            ]
        , Element.textColumn
            [ Element.spacing spacer.px8, Element.width Element.fill ]
            [ Element.paragraph []
                [ Text.strong "openrc.ps1"
                , Element.text ", classic format for Windows PowerShell"
                ]
            , Element.paragraph [ Element.moveRight 24, Element.width Element.fill ]
                [ Element.text "In a powershell window, run "
                , highlightedMonoText (".\\" ++ fileNames.ps1)
                , Element.text ", then any following "
                , highlightedMonoText "openstack"
                , Element.text (" commands in the same " ++ context.localization.commandDrivenTextInterface ++ " window will be authenticated")
                ]
            ]
        ]


credentialsDropdown : View.Types.Context -> Project -> Element.Element Msg
credentialsDropdown context project =
    let
        dropdownTarget togglePopover popoverIsShown =
            Widget.iconButton (SH.materialStyle context.palette).button
                { text = "Download " ++ context.localization.credential |> Helpers.String.pluralize |> Helpers.String.toTitleCase
                , icon =
                    Element.row [ Element.spacing spacer.px4 ]
                        [ Element.text <|
                            "Download "
                                ++ (context.localization.credential |> Helpers.String.pluralize |> Helpers.String.toTitleCase)
                        , Icon.sizedFeatherIcon 18 <|
                            if popoverIsShown then
                                Icons.chevronUp

                            else
                                Icons.chevronDown
                        ]
                , onPress = Just togglePopover
                }

        dropdownContent closePopover =
            let
                buttonLabel text =
                    Widget.button (dropdownItemStyle context.palette)
                        { text = text
                        , icon = Icon.sizedFeatherIcon 18 Icons.download
                        , onPress = Just NoOp
                        }
            in
            Element.column []
                (List.map (Element.downloadAs [ Element.width Element.fill, closePopover ])
                    [ { label = buttonLabel "openrc.sh"
                      , filename = project.auth.project.name ++ "-openrc.sh"
                      , url = Helpers.Url.textDataUrl (Credentials.getOpenRcSh project)
                      }
                    , { label = buttonLabel "openrc.ps1"
                      , filename = project.auth.project.name ++ "-openrc.ps1"
                      , url = Helpers.Url.textDataUrl (Credentials.getOpenRcPs1 project)
                      }
                    , { label = buttonLabel "clouds.yaml"
                      , filename = "clouds.yaml"
                      , url = Helpers.Url.textDataUrl (Credentials.getCloudsYaml [ project ])
                      }
                    ]
                )
    in
    popover context
        (\credentialsButtonDropdownId -> SharedMsg <| SharedMsg.TogglePopover credentialsButtonDropdownId)
        { id = Helpers.String.hyphenate [ "credentialsBtnDropdown", project.auth.project.uuid ]
        , content = dropdownContent
        , contentStyleAttrs = []
        , position = ST.PositionBottomRight
        , distanceToTarget = Nothing
        , target = dropdownTarget
        , targetStyleAttrs = []
        }
