module Page.KeypairList exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input
import FeatherIcons as Icons
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ResourceList exposing (listItemColumnAttribs)
import Helpers.String
import Html.Attributes
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Route
import Set
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.CopyableText
import Style.Widgets.DataList as DataList
import Style.Widgets.DeleteButton exposing (deleteIconButton, deletePopconfirm)
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showHeading : Bool
    , expandedPublicKeys : Set.Set OSTypes.KeypairIdentifier
    , dataListModel : DataList.Model
    }


type Msg
    = GotExpandPublicKey OSTypes.KeypairIdentifier Bool
    | GotDeleteConfirm OSTypes.KeypairIdentifier
    | DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Bool -> Model
init showHeading =
    Model showHeading
        Set.empty
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
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <| SharedMsg.RequestDeleteKeypair keypairId
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
                    [ Element.spacing spacer.px12 ]
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
                                        [ Element.spacing spacer.px4 ]
                                        [ Element.text text
                                        , featherIcon [] Icons.chevronRight
                                        ]
                                , onPress =
                                    Just <| NoOp
                                }
                        }
                    ]

            else
                DataList.view
                    context.localization.pkiPublicKeyForSsh
                    model.dataListModel
                    DataListMsg
                    context
                    []
                    (keypairView model context project)
                    (keypairRecords keypairs)
                    []
                    Nothing
                    Nothing

        keypairsUsedCount =
            project.keypairs
                |> RDPP.withDefault []
                |> List.length
    in
    Element.column
        (VH.contentContainer ++ [ Element.spacing spacer.px32 ])
        [ if model.showHeading then
            Text.heading context.palette
                []
                (featherIcon [] Icons.key)
                (context.localization.pkiPublicKeyForSsh
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )

          else
            Element.none
        , Page.QuotaUsage.view context Page.QuotaUsage.Full (Page.QuotaUsage.Keypair project.computeQuota keypairsUsedCount)
        , VH.renderRDPP
            context
            project.keypairs
            (Helpers.String.pluralize context.localization.pkiPublicKeyForSsh)
            renderKeypairs
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


keypairView : Model -> View.Types.Context -> Project -> KeypairRecord -> Element.Element Msg
keypairView model context project keypairRecord =
    let
        keypairId =
            ( keypairRecord.keypair.name, keypairRecord.keypair.fingerprint )

        deletePopconfirmId =
            Helpers.String.hyphenate
                [ "keypairListDeletePopconfirm"
                , project.auth.project.uuid
                , keypairRecord.keypair.name
                , keypairRecord.keypair.fingerprint
                ]

        deleteKeypairButton togglePopconfirmMsg _ =
            deleteIconButton
                context.palette
                False
                ("Delete " ++ context.localization.pkiPublicKeyForSsh)
                (Just togglePopconfirmMsg)

        deleteKeypairBtnWithPopconfirm =
            deletePopconfirm context
                (\deletePopconfirmId_ -> SharedMsg <| SharedMsg.TogglePopover deletePopconfirmId_)
                deletePopconfirmId
                { confirmation =
                    Element.text <|
                        "Are you sure you want to delete this "
                            ++ context.localization.pkiPublicKeyForSsh
                            ++ "?"
                , buttonText = Nothing
                , onConfirm = Just <| GotDeleteConfirm keypairId
                , onCancel = Just NoOp
                }
                ST.PositionBottomRight
                deleteKeypairButton

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
                                [ Text.fontSize Text.Small
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
                                [ Text.fontSize Text.Small
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
    Element.column (listItemColumnAttribs context.palette ++ [ Element.spacing spacer.px16 ])
        [ Element.row [ Element.width Element.fill ]
            [ Element.el
                (Text.typographyAttrs Text.Emphasized ++ [ Font.color (SH.toElementColor context.palette.neutral.text.default) ])
                (Element.text keypairRecord.keypair.name)
            , Element.el [ Element.alignRight ] deleteKeypairBtnWithPopconfirm
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
            ]
            [ Element.el
                [ Element.width <| Element.minimum 120 Element.shrink
                , publicKeyLabelStyle
                ]
                (Element.text "Public Key:")
            , publicKeyValue
            ]
        ]
