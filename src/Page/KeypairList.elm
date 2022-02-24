module Page.KeypairList exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.ResourceList exposing (listItemColumnAttribs)
import Helpers.String
import Html.Attributes
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import RemoteData
import Route
import Set
import Style.Helpers as SH
import Style.Widgets.CopyableText
import Style.Widgets.DataList as DataList
import Style.Widgets.DeleteButton exposing (deleteIconButton, deletePopconfirm)
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showHeading : Bool
    , expandedPublicKeys : Set.Set OSTypes.KeypairIdentifier
    , shownDeletePopconfirm : Maybe OSTypes.KeypairIdentifier
    , dataListModel : DataList.Model
    }


type Msg
    = GotExpandPublicKey OSTypes.KeypairIdentifier Bool
    | GotDeleteConfirm OSTypes.KeypairIdentifier
    | ShowDeletePopconfirm OSTypes.KeypairIdentifier Bool
    | DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Bool -> Model
init showHeading =
    Model showHeading
        Set.empty
        Nothing
        (DataList.init <| DataList.getDefaultFilterOptions [])


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotExpandPublicKey keypairId expanded ->
            ( { model
                | expandedPublicKeys =
                    if expanded then
                        Set.insert keypairId model.expandedPublicKeys

                    else
                        Set.remove keypairId model.expandedPublicKeys
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotDeleteConfirm keypairId ->
            ( { model | shownDeletePopconfirm = Nothing }
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <| SharedMsg.RequestDeleteKeypair keypairId
            )

        ShowDeletePopconfirm keypairId toBeShown ->
            ( { model
                | shownDeletePopconfirm =
                    if toBeShown then
                        Just keypairId

                    else
                        Nothing
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        DataListMsg dataListMsg ->
            ( { model
                | dataListModel =
                    DataList.update dataListMsg model.dataListModel
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        renderKeypairs : List OSTypes.Keypair -> Element.Element Msg
        renderKeypairs keypairs =
            if List.isEmpty keypairs then
                Element.column
                    (VH.exoColumnAttributes ++ [ Element.paddingXY 10 0 ])
                    [ Element.text <|
                        String.join " "
                            [ "You don't have any"
                            , context.localization.pkiPublicKeyForSsh
                                |> Helpers.String.pluralize
                            , "yet, go upload one!"
                            ]
                    , let
                        text =
                            String.concat [ "Upload a new ", context.localization.pkiPublicKeyForSsh ]
                      in
                      Element.link []
                        { url =
                            Route.toUrl context.urlPathPrefix <|
                                Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                    Route.KeypairCreate
                        , label =
                            Widget.iconButton
                                (SH.materialStyle context.palette).button
                                { text = text
                                , icon =
                                    Element.row
                                        [ Element.spacing 5 ]
                                        [ Element.text text
                                        , Element.el []
                                            (FeatherIcons.chevronRight
                                                |> FeatherIcons.toHtml []
                                                |> Element.html
                                            )
                                        ]
                                , onPress =
                                    Just <| NoOp
                                }
                        }
                    ]

            else
                Element.column
                    VH.contentContainer
                    [ DataList.view
                        model.dataListModel
                        DataListMsg
                        context.palette
                        []
                        (keypairView model context)
                        (keypairRecords keypairs)
                        []
                        []
                    ]

        keypairsUsedCount =
            project.keypairs
                |> RemoteData.withDefault []
                |> List.length
    in
    Element.column
        [ Element.spacing 20, Element.width Element.fill ]
        [ if model.showHeading then
            Element.row (VH.heading2 context.palette ++ [ Element.spacing 15 ])
                [ FeatherIcons.key |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                , Element.text
                    (context.localization.pkiPublicKeyForSsh
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    )
                ]

          else
            Element.none
        , Element.column VH.contentContainer
            [ Page.QuotaUsage.view context Page.QuotaUsage.Full (Page.QuotaUsage.Keypair project.computeQuota keypairsUsedCount)
            , VH.renderWebData
                context
                project.keypairs
                (Helpers.String.pluralize context.localization.pkiPublicKeyForSsh)
                renderKeypairs
            ]
        ]


type alias KeypairRecord =
    DataList.DataRecord { keypair : OSTypes.Keypair }


keypairRecords : List OSTypes.Keypair -> List KeypairRecord
keypairRecords keypairs =
    List.map
        (\keypair ->
            { id = keypair.fingerprint -- doesn't matter if non-unique since bulk selection is not being used
            , selectable = False
            , keypair = keypair
            }
        )
        keypairs


keypairView : Model -> View.Types.Context -> KeypairRecord -> Element.Element Msg
keypairView model context keypairRecord =
    let
        keypairId =
            ( keypairRecord.keypair.name, keypairRecord.keypair.fingerprint )

        showDeletePopconfirm =
            case model.shownDeletePopconfirm of
                Just shownDeletePopconfirmKeypairId ->
                    shownDeletePopconfirmKeypairId == keypairId

                Nothing ->
                    False

        deleteKeypairButton =
            Element.el
                (if showDeletePopconfirm then
                    [ Element.below <|
                        deletePopconfirm context.palette
                            { confirmationText =
                                "Are you sure you want to delete this "
                                    ++ context.localization.pkiPublicKeyForSsh
                                    ++ "?"
                            , onConfirm = Just <| GotDeleteConfirm keypairId
                            , onCancel = Just <| ShowDeletePopconfirm keypairId False
                            }
                    ]

                 else
                    []
                )
                (deleteIconButton
                    context.palette
                    False
                    ("Delete " ++ context.localization.pkiPublicKeyForSsh)
                    (Just <| ShowDeletePopconfirm keypairId True)
                )

        ( publicKeyLabelStyle, publicKeyValue ) =
            if Set.member keypairId model.expandedPublicKeys then
                ( Element.alignTop
                , Element.row [ Element.width Element.fill ]
                    [ Element.el [ Element.width Element.fill ]
                        (Style.Widgets.CopyableText.copyableText
                            context.palette
                            [ Html.Attributes.style "word-break" "break-all"
                                |> Element.htmlAttribute
                            ]
                            keypairRecord.keypair.publicKey
                        )
                    , Element.Input.button
                        [ Element.alignBottom
                        , Element.alignRight
                        ]
                        { label =
                            Element.el
                                [ Font.size 14
                                , Font.color <| SH.toElementColor context.palette.primary
                                ]
                                (Element.text "(show less)")
                        , onPress =
                            Just <|
                                GotExpandPublicKey keypairId False
                        }
                    ]
                )

            else
                ( Element.centerY
                , Element.row [ Element.width Element.fill ]
                    [ Element.el
                        [ -- FIXME: this should come dynamically as the space left after putting "show more"
                          Element.width <| Element.px 640
                        , Element.htmlAttribute <| Html.Attributes.style "min-width" "0"
                        ]
                        (VH.ellipsizedText keypairRecord.keypair.publicKey)
                    , Element.Input.button [ Element.alignRight, Element.width Element.shrink ]
                        { label =
                            Element.el
                                [ Font.size 14
                                , Font.color <| SH.toElementColor context.palette.primary
                                ]
                                (Element.text "(show more)")
                        , onPress =
                            Just <|
                                GotExpandPublicKey keypairId True
                        }
                    ]
                )
    in
    Element.column (listItemColumnAttribs context.palette ++ [ Element.spacing 12 ])
        [ Element.row [ Element.width Element.fill ]
            [ Element.el
                [ Font.size 18
                , Font.color (SH.toElementColor context.palette.on.background)
                ]
                (Element.text keypairRecord.keypair.name)
            , Element.el [ Element.alignRight ] deleteKeypairButton
            ]
        , Element.row []
            [ Element.el
                [ Element.width <| Element.minimum 120 Element.shrink ]
                (Element.text "Fingerprint:")
            , Element.el []
                (Style.Widgets.CopyableText.copyableText
                    context.palette
                    []
                    keypairRecord.keypair.fingerprint
                )
            ]
        , Element.row
            [ Element.width Element.fill
            , Element.paddingEach { top = 6, right = 0, bottom = 0, left = 0 }
            ]
            [ Element.el
                [ Element.width <| Element.minimum 120 Element.shrink
                , publicKeyLabelStyle
                ]
                (Element.text "Public Key:")
            , publicKeyValue
            ]
        ]
