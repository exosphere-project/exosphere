module Page.Credentials exposing (Model, Msg, init, update, view)

import Element
import FeatherIcons as Icons
import Helpers.Credentials
import Helpers.String
import Helpers.Url
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Icon as Icon
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
    Element.column
        [ Element.width Element.fill
        , Element.spacing spacer.px12
        ]
        [ Element.row [ Element.width Element.fill ]
            [ Text.heading context.palette [] Element.none <|
                String.join " "
                    [ "Download"
                    , context.localization.credential |> Helpers.String.pluralize |> Helpers.String.toTitleCase
                    , "for command line access"
                    ]
            , Element.el [ Element.moveUp 1 ] <|
                credentialsDropdown context project
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
                      , url = Helpers.Url.textDataUrl (Helpers.Credentials.getOpenRcSh project)
                      }
                    , { label = buttonLabel "openrc.ps1"
                      , filename = project.auth.project.name ++ "-openrc.ps1"
                      , url = Helpers.Url.textDataUrl (Helpers.Credentials.getOpenRcPs1 project)
                      }
                    , { label = buttonLabel "clouds.yaml"
                      , filename = "clouds.yaml"
                      , url = Helpers.Url.textDataUrl (Helpers.Credentials.getCloudsYaml [ project ])
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
