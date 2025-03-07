module Page.SecurityGroupList exposing (Model, Msg, init, update, view)

import Element
import Element.Font as Font
import FeatherIcons
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (humanCount)
import Helpers.GetterSetters as GetterSetters exposing (LoadingProgress(..), isDefaultSecurityGroup, sortedSecurityGroups)
import Helpers.ResourceList exposing (creationTimeFilterOptions, listItemColumnAttribs, onCreationTimeFilter)
import Helpers.String exposing (pluralizeCount)
import OpenStack.Types as OSTypes exposing (SecurityGroup, SecurityGroupUuid, securityGroupExoTags, securityGroupTaggedAs)
import Route
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.DataList as DataList
import Style.Widgets.DeleteButton exposing (deleteIconButton, deletePopconfirmContent)
import Style.Widgets.HumanTime exposing (relativeTimeElement)
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Tag exposing (tag, tagNeutral, tagPositive)
import Style.Widgets.Text as Text
import Style.Widgets.Validation as Validation
import Time
import Types.Project exposing (Project)
import Types.SecurityGroupActions as SecurityGroupActions
import Types.SharedMsg as SharedMsg
import View.Forms as Forms
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showHeading : Bool
    , dataListModel : DataList.Model
    }


type Msg
    = DataListMsg DataList.Msg
    | GotDeleteConfirm SecurityGroup
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Bool -> Model
init showHeading =
    Model showHeading (DataList.init <| DataList.getDefaultFilterOptions (filters (Time.millisToPosix 0)))


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        DataListMsg dataListMsg ->
            ( { model | dataListModel = DataList.update dataListMsg model.dataListModel }, Cmd.none, SharedMsg.NoOp )

        GotDeleteConfirm securityGroup ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestDeleteSecurityGroup securityGroup
            )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime model =
    let
        renderSuccessCase : List OSTypes.SecurityGroup -> Element.Element Msg
        renderSuccessCase securityGroups =
            DataList.view
                context.localization.securityGroup
                model.dataListModel
                DataListMsg
                context
                []
                (securityGroupView context project currentTime)
                (securityGroupRecords <| sortedSecurityGroups <| securityGroups)
                []
                (Just
                    { filters = filters currentTime
                    , dropdownMsgMapper = \dropdownId -> SharedMsg <| SharedMsg.TogglePopover dropdownId
                    }
                )
                Nothing
    in
    Element.column
        (VH.contentContainer ++ [ Element.spacing spacer.px32 ])
        [ if model.showHeading then
            Text.heading context.palette
                []
                (FeatherIcons.shield |> FeatherIcons.toHtml [] |> Element.html |> Element.el [])
                (context.localization.securityGroup
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )

          else
            Element.none

        -- TODO: Show security group quota usage.
        , VH.renderRDPP
            context
            project.securityGroups
            (Helpers.String.pluralize context.localization.securityGroup)
            renderSuccessCase
        ]


type alias SecurityGroupRecord =
    DataList.DataRecord
        { securityGroup : OSTypes.SecurityGroup
        }


securityGroupRecords : List OSTypes.SecurityGroup -> List SecurityGroupRecord
securityGroupRecords securityGroups =
    List.map
        (\securityGroup ->
            { id = securityGroup.uuid
            , selectable = False
            , securityGroup = securityGroup
            }
        )
        securityGroups


warningSecurityGroupAffectsServers : View.Types.Context -> Project -> SecurityGroupUuid -> Element.Element msg
warningSecurityGroupAffectsServers context project securityGroupUuid =
    case Forms.securityGroupAffectsServersWarning context project securityGroupUuid Nothing "deleting" of
        Just warning ->
            Element.el
                [ Element.width Element.shrink, Element.alignRight ]
            <|
                Validation.warningText context.palette <|
                    warning

        Nothing ->
            Element.none


securityGroupView : View.Types.Context -> Project -> Time.Posix -> SecurityGroupRecord -> Element.Element Msg
securityGroupView context project currentTime securityGroupRecord =
    let
        { locale } =
            context

        securityGroupLink =
            Element.link []
                { url =
                    Route.toUrl context.urlPathPrefix
                        (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                            Route.SecurityGroupDetail securityGroupRecord.id
                        )
                , label =
                    Element.el
                        (Text.typographyAttrs Text.Emphasized
                            ++ [ Font.color (SH.toElementColor context.palette.primary)
                               ]
                        )
                        (Element.text <|
                            VH.extendedResourceName
                                (Just securityGroupRecord.securityGroup.name)
                                securityGroupRecord.securityGroup.uuid
                                context.localization.securityGroup
                        )
                }

        numberOfRules =
            List.length securityGroupRecord.securityGroup.rules

        securityGroupRuleCount =
            Element.el []
                (Element.text
                    (String.join " "
                        [ humanCount
                            { locale | decimals = Exact 0 }
                            numberOfRules
                        , "rule" |> pluralizeCount numberOfRules
                        ]
                    )
                )

        { servers, progress } =
            GetterSetters.serversForSecurityGroup project securityGroupRecord.securityGroup.uuid

        numberOfServers =
            servers
                |> List.length

        unused =
            numberOfServers == 0

        preset =
            if securityGroupTaggedAs securityGroupExoTags.preset securityGroupRecord.securityGroup then
                tagPositive context.palette "preset"

            else
                Element.none

        default =
            if isDefaultSecurityGroup context project securityGroupRecord.securityGroup then
                tagNeutral context.palette "default"

            else
                Element.none

        tags =
            [ preset
            , default
            , case ( progress, unused ) of
                ( Done, True ) ->
                    tag context.palette "unused"

                _ ->
                    Element.none
            ]

        serverCount =
            Element.el []
                (Element.text
                    (String.join " "
                        [ if progress /= Done then
                            "loading"

                          else
                            humanCount
                                { locale | decimals = Exact 0 }
                                numberOfServers
                        , context.localization.virtualComputer |> pluralizeCount numberOfServers
                        ]
                    )
                )

        accentColor =
            context.palette.neutral.text.default |> SH.toElementColor

        accented =
            Element.el [ Font.color accentColor ]

        actions =
            [ case progress of
                -- Reveal the delete option once we know how many servers might be affected.
                Done ->
                    let
                        securityGroupAction =
                            GetterSetters.getSecurityGroupActions project (SecurityGroupActions.ExtantGroup securityGroupRecord.id)

                        deletionInProgress =
                            securityGroupAction
                                |> Maybe.map .pendingDeletion
                                |> Maybe.withDefault False
                    in
                    if deletionInProgress then
                        Widget.circularProgressIndicator
                            (SH.materialStyle context.palette).progressIndicator
                            Nothing

                    else
                        let
                            protected =
                                securityGroupTaggedAs securityGroupExoTags.preset securityGroupRecord.securityGroup
                                    || isDefaultSecurityGroup context project securityGroupRecord.securityGroup

                            deleteBtn togglePopconfirmMsg _ =
                                deleteIconButton
                                    context.palette
                                    False
                                    ("Delete " ++ context.localization.securityGroup)
                                    (if protected then
                                        Nothing

                                     else
                                        Just togglePopconfirmMsg
                                    )

                            deletePopconfirmId =
                                Helpers.String.hyphenate
                                    [ "securityGroupListDeletePopconfirm"
                                    , project.auth.project.uuid
                                    , securityGroupRecord.id
                                    ]
                        in
                        popover context
                            (\deletePopconfirmId_ -> SharedMsg <| SharedMsg.TogglePopover deletePopconfirmId_)
                            { id = deletePopconfirmId
                            , content =
                                \confirmEl ->
                                    Element.column [ Element.spacing spacer.px8, Element.padding spacer.px4 ] <|
                                        [ deletePopconfirmContent
                                            context.palette
                                            { confirmation =
                                                Element.column [ Element.spacingXY spacer.px4 spacer.px12 ]
                                                    [ Element.text <|
                                                        "Are you sure you want to delete this "
                                                            ++ context.localization.securityGroup
                                                            ++ "?"
                                                    , warningSecurityGroupAffectsServers context project securityGroupRecord.id
                                                    ]
                                            , buttonText = Nothing
                                            , onCancel = Just NoOp
                                            , onConfirm = Just <| GotDeleteConfirm securityGroupRecord.securityGroup
                                            }
                                            confirmEl
                                        ]
                            , contentStyleAttrs = []
                            , position = ST.PositionBottomRight
                            , distanceToTarget = Nothing
                            , target = deleteBtn
                            , targetStyleAttrs = []
                            }

                _ ->
                    Element.none
            ]
    in
    Element.column
        (listItemColumnAttribs context.palette)
        [ Element.row [ Element.spacing spacer.px12, Element.width Element.fill ]
            [ Element.column [ Element.spacing spacer.px12, Element.width Element.fill ]
                [ securityGroupLink
                , Element.row [ Element.spacing spacer.px8 ]
                    [ securityGroupRuleCount
                    , Element.text "·"
                    , Element.row [ Element.spacing spacer.px4 ]
                        [ Element.text "created "
                        , accented (relativeTimeElement currentTime securityGroupRecord.securityGroup.createdAt)
                        ]
                    , Element.text "·"
                    , serverCount
                    ]
                ]
            , Element.row [ Element.spacing spacer.px16, Element.alignRight, Element.alignTop ]
                (Element.row [ Element.spacing spacer.px4 ] tags :: actions)
            ]
        ]


filters : Time.Posix -> List (DataList.Filter { record | securityGroup : OSTypes.SecurityGroup })
filters currentTime =
    [ { id = "creationTime"
      , label = "Created within"
      , chipPrefix = "Created within "
      , filterOptions =
            \_ -> creationTimeFilterOptions
      , filterTypeAndDefaultValue =
            DataList.UniselectOption DataList.UniselectNoChoice
      , onFilter =
            \optionValue securityGroup ->
                onCreationTimeFilter optionValue securityGroup.securityGroup.createdAt currentTime
      }
    ]
