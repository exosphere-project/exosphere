module Page.ShareList exposing (Model, Msg, init, update, view)

import Element
import Element.Font as Font
import FeatherIcons
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (Unit(..), humanNumber)
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers exposing (lookupUsername)
import Helpers.ResourceList exposing (creationTimeFilterOptions, creatorFilterOptions, listItemColumnAttribs, onCreationTimeFilter)
import Helpers.String
import OpenStack.Types as OSTypes exposing (ShareStatus(..))
import Page.QuotaUsage
import Route
import Style.Helpers as SH
import Style.Widgets.DataList as DataList
import Style.Widgets.HumanTime exposing (relativeTimeElement)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.Uuid exposing (uuidLabel)
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
    = NoOp
    | DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg
    | GotDeleteConfirm OSTypes.ShareUuid


init : Project -> Bool -> Model
init project showHeading =
    Model showHeading (DataList.init <| DataList.getDefaultFilterOptions (filters project (Time.millisToPosix 0)))


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        DataListMsg dataListMsg ->
            ( { model | dataListModel = DataList.update dataListMsg model.dataListModel }, Cmd.none, SharedMsg.NoOp )

        GotDeleteConfirm shareUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestDeleteShare shareUuid
            )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime model =
    let
        renderSuccessCase : List OSTypes.Share -> Element.Element Msg
        renderSuccessCase shares =
            DataList.view
                context.localization.share
                model.dataListModel
                DataListMsg
                context
                []
                (shareView context project currentTime)
                (shareRecords project shares)
                []
                (Just
                    { filters = filters project currentTime
                    , dropdownMsgMapper = \dropdownId -> SharedMsg <| SharedMsg.TogglePopover dropdownId
                    }
                )
                (Just <| searchByNameUuidFilter context)
    in
    Element.column
        (VH.contentContainer ++ [ Element.spacing spacer.px32 ])
        [ if model.showHeading then
            Text.heading context.palette
                []
                (FeatherIcons.share2 |> FeatherIcons.toHtml [] |> Element.html |> Element.el [])
                (context.localization.share
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )

          else
            Element.none
        , Page.QuotaUsage.view context Page.QuotaUsage.Full (Page.QuotaUsage.Share project.shareQuota)
        , VH.renderRDPP
            context
            project.shares
            (Helpers.String.pluralize context.localization.share)
            renderSuccessCase
        ]


type alias ShareRecord =
    DataList.DataRecord
        { share : OSTypes.Share
        , creator : String
        }


shareRecords : Project -> List OSTypes.Share -> List ShareRecord
shareRecords project shares =
    let
        creator : OSTypes.Share -> String
        creator share =
            lookupUsername project share.userUuid
                |> Maybe.withDefault "unknown user"
    in
    List.map
        (\share ->
            { id = share.uuid
            , selectable = False
            , share = share
            , creator = creator share
            }
        )
        shares


shareView : View.Types.Context -> Project -> Time.Posix -> ShareRecord -> Element.Element Msg
shareView context project currentTime shareRecord =
    let
        { locale } =
            context

        shareLink =
            Element.link []
                { url =
                    Route.toUrl context.urlPathPrefix
                        (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                            Route.ShareDetail shareRecord.id
                        )
                , label =
                    Element.el
                        (Text.typographyAttrs Text.Emphasized
                            ++ [ Font.color (SH.toElementColor context.palette.primary)
                               ]
                        )
                        (Element.text <|
                            VH.extendedResourceName
                                shareRecord.share.name
                                shareRecord.share.uuid
                                context.localization.share
                        )
                }

        ( shareBytes, shareBytesSuffix ) =
            humanNumber
                { locale | decimals = Exact 0 }
                GibiBytes
                shareRecord.share.size

        shareSize =
            Element.el [ Element.alignRight ]
                (Element.text (shareBytes ++ " " ++ shareBytesSuffix))

        accentColor =
            context.palette.neutral.text.default |> SH.toElementColor

        accented =
            Element.el [ Font.color accentColor ]

        actions =
            case shareRecord.share.status of
                ShareDeleting ->
                    Text.text Text.Body [ Font.italic ] "Deleting..."

                ShareErrorDeleting ->
                    Text.text Text.Body [ Font.italic ] ("Could not delete " ++ context.localization.share ++ ".")

                ShareAvailable ->
                    VH.deleteResourcePopconfirm
                        context
                        project
                        (SharedMsg << SharedMsg.TogglePopover)
                        { uuid = shareRecord.id, word = context.localization.share }
                        "shareListDeletePopconfirm"
                        (Just <| GotDeleteConfirm shareRecord.id)
                        (Just NoOp)

                _ ->
                    Element.none
    in
    Element.column
        (listItemColumnAttribs context.palette)
        [ Element.row [ Element.spacing spacer.px12, Element.width Element.fill ]
            [ Element.column [ Element.spacing spacer.px12, Element.width Element.fill ]
                [ shareLink
                ]
            , Element.el [ Element.alignRight, Element.alignTop ]
                actions
            ]
        , Element.row [ Element.spacing spacer.px8, Element.width Element.fill ]
            [ shareSize
            , Element.text "·"
            , Element.paragraph [ Element.spacing spacer.px8 ]
                [ Element.text "created "
                , accented (relativeTimeElement currentTime shareRecord.share.createdAt)
                , Element.text " by "
                , accented (Element.text shareRecord.creator)
                ]
            , uuidLabel context.palette shareRecord.id
            ]
        ]


searchByNameUuidFilter : View.Types.Context -> DataList.SearchFilter { record | share : OSTypes.Share }
searchByNameUuidFilter context =
    { label = "Search:"
    , placeholder =
        Just <|
            "Enter "
                ++ context.localization.share
                ++ " name or UUID"
    , textToSearch = \record -> Maybe.withDefault "" record.share.name ++ " " ++ record.id
    }


filters :
    Project
    -> Time.Posix
    -> List (DataList.Filter { record | share : OSTypes.Share, creator : String })
filters project currentTime =
    [ { id = "creator"
      , label = "Creator"
      , chipPrefix = "Created by "
      , filterOptions = \records -> creatorFilterOptions project (List.map .creator records)
      , filterTypeAndDefaultValue = DataList.UniselectOption (DataList.UniselectHasChoice project.auth.user.name)
      , onFilter = \optionValue share -> share.creator == optionValue
      }
    , { id = "creationTime"
      , label = "Created within"
      , chipPrefix = "Created within "
      , filterOptions =
            \_ -> creationTimeFilterOptions
      , filterTypeAndDefaultValue =
            DataList.UniselectOption DataList.UniselectNoChoice
      , onFilter =
            \optionValue share ->
                onCreationTimeFilter optionValue share.share.createdAt currentTime
      }
    ]
