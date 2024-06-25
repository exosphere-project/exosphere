module Page.SecurityGroupList exposing (Model, Msg, init, update, view)

import Element
import Element.Font as Font
import FeatherIcons
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (humanCount)
import Helpers.GetterSetters as GetterSetters exposing (LoadingProgress(..))
import Helpers.ResourceList exposing (creationTimeFilterOptions, listItemColumnAttribs, onCreationTimeFilter)
import Helpers.String exposing (pluralizeCount)
import OpenStack.Types as OSTypes exposing (securityGroupExoTags, securityGroupTaggedAs)
import Route
import Style.Helpers as SH
import Style.Widgets.DataList as DataList
import Style.Widgets.HumanTime exposing (relativeTimeElement)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Tag exposing (tag, tagPositive)
import Style.Widgets.Text as Text
import Time
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { showHeading : Bool
    , dataListModel : DataList.Model
    }


type Msg
    = -- TODO: Delete security group (if not in use or protected).
      DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg


init : Bool -> Model
init showHeading =
    Model showHeading (DataList.init <| DataList.getDefaultFilterOptions (filters (Time.millisToPosix 0)))


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        DataListMsg dataListMsg ->
            ( { model | dataListModel = DataList.update dataListMsg model.dataListModel }, Cmd.none, SharedMsg.NoOp )


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
                (securityGroupRecords securityGroups)
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

        -- TODO: Add protected field based on "default", "exosphere", or tags.
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

        tags =
            [ preset
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
            [ Element.none ]
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
            , Element.row [ Element.spacing spacer.px4, Element.alignRight, Element.alignTop ]
                (tags ++ actions)
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
