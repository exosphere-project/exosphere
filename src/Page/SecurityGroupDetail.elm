module Page.SecurityGroupDetail exposing (Model, Msg(..), init, update, view)

import DateFormat.Relative
import Element
import Element.Font as Font
import FeatherIcons
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (humanCount)
import Helpers.GetterSetters as GetterSetters exposing (LoadingProgress(..), isDefaultSecurityGroup)
import Helpers.Helpers exposing (serverCreatorName)
import Helpers.String
import OpenStack.Types as OSTypes exposing (SecurityGroup, securityGroupExoTags, securityGroupTaggedAs)
import Page.SecurityGroupForm as SecurityGroupForm
import Page.SecurityGroupRulesTable exposing (rulesTable)
import Route
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon as Icon
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.StatusBadge as StatusBadge
import Style.Widgets.Tag exposing (tagNeutral, tagPositive)
import Style.Widgets.Text as Text
import Style.Widgets.Validation as Validation
import Time
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Forms as Forms
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { deletePendingConfirmation : Maybe OSTypes.SecurityGroupUuid
    , securityGroupUuid : OSTypes.SecurityGroupUuid
    , securityGroupForm : Maybe SecurityGroupForm.Model
    }


type Msg
    = GotDeleteNeedsConfirm (Maybe OSTypes.SecurityGroupUuid)
    | GotEditSecurityGroupForm
    | SecurityGroupFormMsg SecurityGroupForm.Msg
    | SharedMsg SharedMsg.SharedMsg


init : OSTypes.SecurityGroupUuid -> Model
init securityGroupUuid =
    { deletePendingConfirmation = Nothing
    , securityGroupUuid = securityGroupUuid
    , securityGroupForm = Nothing
    }


update : Msg -> SharedModel -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg sharedModel project model =
    case msg of
        GotDeleteNeedsConfirm securityGroupUuid ->
            ( { model | deletePendingConfirmation = securityGroupUuid }, Cmd.none, SharedMsg.NoOp )

        GotEditSecurityGroupForm ->
            let
                securityGroupForm =
                    GetterSetters.securityGroupLookup project model.securityGroupUuid
                        |> Maybe.map SecurityGroupForm.initWithSecurityGroup
            in
            ( { model | securityGroupForm = securityGroupForm }
            , Cmd.none
            , SharedMsg.NoOp
            )

        SecurityGroupFormMsg securityGroupFormMsg ->
            case securityGroupFormMsg of
                SecurityGroupForm.GotCancel ->
                    ( { model | securityGroupForm = Nothing }
                    , Cmd.none
                    , SharedMsg.NoOp
                    )

                _ ->
                    case model.securityGroupForm of
                        Just securityGroupForm ->
                            let
                                ( newSecurityGroupForm, securityGroupFormCmd, securityGroupFormSharedMsg ) =
                                    SecurityGroupForm.update securityGroupFormMsg sharedModel project securityGroupForm
                            in
                            ( { model | securityGroupForm = Just newSecurityGroupForm }
                            , Cmd.map SecurityGroupFormMsg securityGroupFormCmd
                            , securityGroupFormSharedMsg
                            )

                        Nothing ->
                            ( model, Cmd.none, SharedMsg.NoOp )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


popoverMsgMapper : PopoverId -> Msg
popoverMsgMapper popoverId =
    SharedMsg <| SharedMsg.TogglePopover popoverId


view : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> Element.Element Msg
view context project currentTimeAndZone model =
    VH.renderRDPP context
        project.securityGroups
        context.localization.securityGroup
        (\_ ->
            {- Attempt to look up a given security group uuid; if a security group is found, call render. -}
            case GetterSetters.securityGroupLookup project model.securityGroupUuid of
                Just securityGroup ->
                    render context project currentTimeAndZone model securityGroup

                Nothing ->
                    Element.text <|
                        String.join " "
                            [ "No"
                            , context.localization.securityGroup
                            , "found"
                            ]
        )


createdAgoEtc :
    View.Types.Context
    ->
        { ago : ( String, Element.Element msg )
        , rules : ( String, String )
        }
    -> Element.Element msg
createdAgoEtc context { ago, rules } =
    let
        ( agoWord, agoContents ) =
            ago

        ( numberOfRules, rulesUnit ) =
            rules

        subduedText =
            Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)
    in
    Element.wrappedRow
        [ Element.width Element.fill ]
    <|
        [ Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| agoWord ++ " ")
            , agoContents
            ]
        , Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| "·")
            ]
        , Element.row [ Element.padding spacer.px8 ]
            [ Element.text numberOfRules
            , Element.el [ subduedText ] (Element.text <| " " ++ rulesUnit)
            ]
        ]


securityGroupNameView : SecurityGroup -> Element.Element Msg
securityGroupNameView securityGroup =
    let
        name_ =
            VH.resourceName (Just securityGroup.name) securityGroup.uuid
    in
    Element.row
        [ Element.spacing spacer.px8 ]
        [ Text.text Text.ExtraLarge [] name_ ]


serversTable : View.Types.Context -> Project -> { servers : List Server, progress : LoadingProgress, currentTime : Time.Posix } -> Element.Element Msg
serversTable context project { servers, progress, currentTime } =
    let
        table =
            case List.length servers of
                0 ->
                    Element.text "(none)"

                _ ->
                    Element.table
                        [ Element.spacing spacer.px16
                        ]
                        { data = servers
                        , columns =
                            [ { header = VH.tableHeader "Name"
                              , width = Element.shrink
                              , view =
                                    \item ->
                                        Element.link [ Element.centerY ]
                                            { url =
                                                Route.toUrl context.urlPathPrefix
                                                    (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                                        Route.ServerDetail item.osProps.uuid
                                                    )
                                            , label =
                                                Element.el
                                                    [ Font.color (SH.toElementColor context.palette.primary), Element.width (Element.px 220) ]
                                                    (VH.ellipsizedText <|
                                                        VH.extendedResourceName
                                                            (Just item.osProps.name)
                                                            item.osProps.uuid
                                                            context.localization.virtualComputer
                                                    )
                                            }
                              }
                            , { header = VH.tableHeader "Created By"
                              , width = Element.shrink
                              , view =
                                    \item ->
                                        Text.text Text.Body [ Element.centerY ] (serverCreatorName project item)
                              }
                            , { header = VH.tableHeader "Created"
                              , width = Element.shrink
                              , view =
                                    \item ->
                                        Text.text Text.Body [ Element.centerY ] (DateFormat.Relative.relativeTime currentTime item.osProps.details.created)
                              }
                            , { header = VH.tableHeader ""
                              , width = Element.shrink
                              , view =
                                    \item ->
                                        VH.serverStatusBadge context.palette StatusBadge.Small item
                              }
                            ]
                        }
    in
    VH.renderProgress { progress = progress, items = servers } table


warningSecurityGroupAffectsServers : View.Types.Context -> Project -> OSTypes.SecurityGroupUuid -> Element.Element msg
warningSecurityGroupAffectsServers context project securityGroupUuid =
    case Forms.securityGroupAffectsServersWarning context project securityGroupUuid Nothing "deleting" of
        Just warning ->
            Element.el
                [ Element.width Element.shrink, Element.alignLeft ]
            <|
                Validation.warningMessage context.palette <|
                    warning

        Nothing ->
            Element.none


renderDeleteAction : View.Types.Context -> Project -> Model -> { preset : Bool, default : Bool, progress : LoadingProgress } -> Maybe Msg -> Maybe (Element.Attribute Msg) -> List (Element.Element Msg)
renderDeleteAction context project model { preset, default, progress } actionMsg closeActionsDropdown =
    [ case model.deletePendingConfirmation of
        Just _ ->
            let
                additionalBtnAttribs =
                    case closeActionsDropdown of
                        Just closeActionsDropdown_ ->
                            [ closeActionsDropdown_ ]

                        Nothing ->
                            []
            in
            VH.renderConfirmation
                context
                actionMsg
                (Just <|
                    GotDeleteNeedsConfirm Nothing
                )
                "Are you sure?"
                additionalBtnAttribs

        Nothing ->
            Element.row
                [ Element.spacing spacer.px12, Element.width (Element.fill |> Element.minimum 280) ]
                [ Element.text
                    (case ( preset, default, progress ) of
                        ( True, _, _ ) ->
                            "Preset " ++ (context.localization.securityGroup |> Helpers.String.pluralize) ++ " cannot be deleted."

                        ( _, True, _ ) ->
                            "Default " ++ (context.localization.securityGroup |> Helpers.String.pluralize) ++ " cannot be deleted."

                        ( False, False, Done ) ->
                            "Delete " ++ context.localization.securityGroup ++ "?"

                        ( False, False, _ ) ->
                            "Loading " ++ (context.localization.virtualComputer |> Helpers.String.pluralize) ++ "..."
                    )
                , Element.el
                    [ Element.alignRight ]
                  <|
                    Button.button
                        Button.Danger
                        context.palette
                        { text = "Delete"
                        , onPress =
                            if preset || default || progress /= Done then
                                Nothing

                            else
                                Just <| GotDeleteNeedsConfirm <| Just model.securityGroupUuid
                        }
                ]
    , warningSecurityGroupAffectsServers context project model.securityGroupUuid
    ]


securityGroupActionsDropdown : View.Types.Context -> Project -> Model -> SecurityGroup -> { preset : Bool, default : Bool, progress : LoadingProgress } -> Element.Element Msg
securityGroupActionsDropdown context project model securityGroup { preset, default, progress } =
    let
        dropdownId =
            [ "securityGroupActionsDropdown", project.auth.project.uuid, securityGroup.uuid ]
                |> List.intersperse "-"
                |> String.concat

        dropdownContent closeDropdown =
            Element.column [ Element.spacing spacer.px12 ] <|
                Element.row
                    [ Element.spacing spacer.px12, Element.width (Element.fill |> Element.minimum 280) ]
                    [ Element.text
                        (case ( default, preset ) of
                            ( True, _ ) ->
                                "This " ++ context.localization.securityGroup ++ " is always listed when creating " ++ Helpers.String.pluralize context.localization.virtualComputer ++ "."

                            ( _, True ) ->
                                "This " ++ context.localization.securityGroup ++ " is listed as a preset when creating " ++ Helpers.String.pluralize context.localization.virtualComputer ++ "."

                            ( _, False ) ->
                                "List this " ++ context.localization.securityGroup ++ " as a preset when creating " ++ Helpers.String.pluralize context.localization.virtualComputer ++ "?"
                        )
                    , if default then
                        Element.none

                      else
                        Element.el
                            [ Element.alignRight, closeDropdown ]
                        <|
                            Button.button
                                (if preset then
                                    Button.DangerSecondary

                                 else
                                    Button.Primary
                                )
                                context.palette
                                { text =
                                    if preset then
                                        "Remove"

                                    else
                                        "Add"
                                , onPress =
                                    Just <|
                                        SharedMsg <|
                                            (SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                                                SharedMsg.RequestUpdateSecurityGroupTags securityGroup.uuid
                                                    (if preset then
                                                        securityGroup.tags
                                                            |> List.filter (\tag -> tag /= securityGroupExoTags.preset)

                                                     else
                                                        securityGroup.tags ++ [ securityGroupExoTags.preset ]
                                                    )
                                            )
                                }
                    ]
                    :: renderDeleteAction context
                        project
                        model
                        { preset = preset, default = default, progress = progress }
                        (Just <|
                            SharedMsg <|
                                (SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                                    SharedMsg.RequestDeleteSecurityGroup securityGroup
                                )
                        )
                        (Just closeDropdown)

        dropdownTarget toggleDropdownMsg dropdownIsShown =
            Widget.iconButton
                (SH.materialStyle context.palette).button
                { text = "Actions"
                , icon =
                    Element.row
                        [ Element.spacing spacer.px4 ]
                        [ Element.text "Actions"
                        , Icon.sizedFeatherIcon 18 <|
                            if dropdownIsShown then
                                FeatherIcons.chevronUp

                            else
                                FeatherIcons.chevronDown
                        ]
                , onPress = Just toggleDropdownMsg
                }
    in
    popover context
        popoverMsgMapper
        { id = dropdownId
        , content = dropdownContent
        , contentStyleAttrs = [ Element.padding spacer.px24 ]
        , position = ST.PositionBottomRight
        , distanceToTarget = Nothing
        , target = dropdownTarget
        , targetStyleAttrs = []
        }


render : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> SecurityGroup -> Element.Element Msg
render context project ( currentTime, _ ) model securityGroup =
    let
        numberOfRulesString =
            let
                locale =
                    context.locale
            in
            humanCount
                { locale | decimals = Exact 0 }
                (List.length securityGroup.rules)

        ruleWord =
            "rule"

        rulesUnit =
            case securityGroup.rules of
                [ _ ] ->
                    ruleWord

                _ ->
                    ruleWord |> Helpers.String.pluralize

        description =
            case securityGroup.description of
                Just str ->
                    if String.isEmpty str then
                        Element.none

                    else
                        Element.row [ Element.padding spacer.px8 ]
                            [ Element.paragraph [ Element.width Element.fill ] <|
                                [ Element.text <| str ]
                            ]

                Nothing ->
                    Element.none

        preset =
            securityGroupTaggedAs securityGroupExoTags.preset securityGroup

        presetTag =
            if preset then
                tagPositive context.palette "preset"

            else
                Element.none

        default =
            isDefaultSecurityGroup context project securityGroup

        defaultTag =
            if default then
                tagNeutral context.palette "default"

            else
                Element.none

        serverLookup =
            GetterSetters.serversForSecurityGroup project securityGroup.uuid

        servers =
            serversTable
                context
                project
                { servers = serverLookup.servers
                , progress = serverLookup.progress
                , currentTime = currentTime
                }
    in
    Element.column [ Element.spacing spacer.px24, Element.width Element.fill ]
        [ Element.row (Text.headingStyleAttrs context.palette)
            [ FeatherIcons.shield |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Text.text Text.ExtraLarge
                []
                (context.localization.securityGroup
                    |> Helpers.String.toTitleCase
                )
            , securityGroupNameView securityGroup
            , defaultTag
            , presetTag
            , if default then
                Element.none

              else
                Widget.button
                    (SH.materialStyle context.palette).button
                    { text = "Edit"
                    , icon = Icon.sizedFeatherIcon 16 FeatherIcons.edit3
                    , onPress =
                        if model.securityGroupForm == Nothing then
                            Just GotEditSecurityGroupForm

                        else
                            Nothing
                    }
            , Element.row [ Element.alignRight, Text.fontSize Text.Body, Font.regular, Element.spacing spacer.px16 ]
                [ securityGroupActionsDropdown context project model securityGroup { preset = preset, default = default, progress = serverLookup.progress }
                ]
            ]
        , case model.securityGroupForm of
            Just securityGroupForm ->
                VH.tile
                    context
                    [ Element.text
                        (String.join " "
                            [ "Edit"
                            , context.localization.securityGroup
                                |> Helpers.String.toTitleCase
                            ]
                        )
                    ]
                    [ Element.column
                        [ Element.spacing spacer.px16, Element.width Element.fill ]
                        [ SecurityGroupForm.view
                            context
                            project
                            currentTime
                            securityGroupForm
                            Nothing
                            |> Element.map SecurityGroupFormMsg
                        ]
                    ]

            Nothing ->
                Element.none
        , VH.tile
            context
            [ FeatherIcons.list |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Element.text "Info"
            , Element.el
                [ Text.fontSize Text.Tiny
                , Font.color (SH.toElementColor context.palette.neutral.text.subdued)
                , Element.alignBottom
                ]
                (copyableText context.palette
                    [ Element.width (Element.shrink |> Element.minimum 240) ]
                    securityGroup.uuid
                )
            ]
            [ description
            , createdAgoEtc
                context
                { ago = ( "created", VH.whenCreated context project popoverMsgMapper currentTime securityGroup )
                , rules = ( numberOfRulesString, rulesUnit )
                }
            ]
        , case model.securityGroupForm of
            Just _ ->
                Element.none

            Nothing ->
                let
                    rules =
                        rulesTable
                            context
                            (GetterSetters.projectIdentifier project)
                            { rules = securityGroup.rules, securityGroupForUuid = GetterSetters.securityGroupLookup project }
                in
                VH.tile
                    context
                    [ FeatherIcons.lock
                        |> FeatherIcons.toHtml []
                        |> Element.html
                        |> Element.el []
                    , ruleWord
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                        |> Element.text
                    ]
                    [ rules
                    ]
        , VH.tile
            context
            [ FeatherIcons.server
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
            , context.localization.virtualComputer
                |> Helpers.String.pluralize
                |> Helpers.String.toTitleCase
                |> Element.text
            ]
            [ servers
            ]
        ]
