module Page.KeypairList exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import FeatherIcons
import Helpers.String
import Html.Attributes
import OpenStack.Types as OSTypes
import Set
import Style.Helpers as SH
import Style.Widgets.Card as Card
import Style.Widgets.CopyableText
import Style.Widgets.Icon as Icon
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showHeading : Bool
    , expandedKeypairs : Set.Set OSTypes.KeypairIdentifier
    , deleteConfirmations : Set.Set OSTypes.KeypairIdentifier
    }


type Msg
    = GotExpandCard OSTypes.KeypairIdentifier Bool
    | GotDeleteNeedsConfirm OSTypes.KeypairIdentifier
    | GotDeleteConfirm OSTypes.KeypairIdentifier
    | GotDeleteCancel OSTypes.KeypairIdentifier
    | SharedMsg SharedMsg.SharedMsg


init : Bool -> Model
init showHeading =
    Model showHeading Set.empty Set.empty


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotExpandCard keypairId expanded ->
            ( { model
                | expandedKeypairs =
                    if expanded then
                        Set.insert keypairId model.expandedKeypairs

                    else
                        Set.remove keypairId model.expandedKeypairs
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotDeleteNeedsConfirm keypairId ->
            ( { model
                | deleteConfirmations =
                    Set.insert
                        keypairId
                        model.deleteConfirmations
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotDeleteConfirm keypairId ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg project.auth.project.uuid <| SharedMsg.RequestDeleteKeypair keypairId
            )

        GotDeleteCancel keypairId ->
            ( { model
                | deleteConfirmations =
                    Set.remove
                        keypairId
                        model.deleteConfirmations
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        renderKeypairs : List OSTypes.Keypair -> Element.Element Msg
        renderKeypairs keypairs_ =
            if List.isEmpty keypairs_ then
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
                            Just <|
                                SharedMsg <|
                                    NavigateToView <|
                                        SharedMsg.ProjectPage project.auth.project.uuid <|
                                            SharedMsg.KeypairCreate
                        }
                    ]

            else
                Element.column
                    VH.contentContainer
                    (List.map
                        (renderKeypairCard context model)
                        keypairs_
                    )
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
        , VH.renderWebData
            context
            project.keypairs
            (Helpers.String.pluralize context.localization.pkiPublicKeyForSsh)
            renderKeypairs
        ]


renderKeypairCard : View.Types.Context -> Model -> OSTypes.Keypair -> Element.Element Msg
renderKeypairCard context model keypair =
    let
        cardBody =
            Element.column
                VH.exoColumnAttributes
                [ VH.compactKVRow "Public Key" <|
                    Style.Widgets.CopyableText.copyableText context.palette
                        [ Font.family [ Font.monospace ]
                        , Html.Attributes.style "word-break" "break-all" |> Element.htmlAttribute
                        ]
                        keypair.publicKey
                , VH.compactKVRow "Fingerprint" <|
                    Style.Widgets.CopyableText.copyableText context.palette
                        [ Font.family [ Font.monospace ]
                        , Html.Attributes.style "word-break" "break-all" |> Element.htmlAttribute
                        ]
                        keypair.fingerprint
                , actionButtons context model keypair
                ]

        expanded =
            Set.member (toIdentifier keypair) model.expandedKeypairs
    in
    Card.expandoCard
        context.palette
        expanded
        (GotExpandCard (toIdentifier keypair))
        (VH.possiblyUntitledResource keypair.name context.localization.pkiPublicKeyForSsh
            |> Element.text
        )
        (if expanded then
            Element.none

         else
            Element.el [ Font.family [ Font.monospace ] ] (Element.text keypair.fingerprint)
        )
        cardBody


actionButtons : View.Types.Context -> Model -> OSTypes.Keypair -> Element.Element Msg
actionButtons context model keypair =
    let
        confirmationNeeded =
            Set.member (toIdentifier keypair) model.deleteConfirmations

        deleteButton =
            if confirmationNeeded then
                Element.row [ Element.spacing 10 ]
                    [ Element.text "Confirm delete?"
                    , Widget.iconButton
                        (SH.materialStyle context.palette).dangerButton
                        { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                        , text = "Delete"
                        , onPress = Just <| GotDeleteConfirm (toIdentifier keypair)
                        }
                    , Widget.textButton
                        (SH.materialStyle context.palette).button
                        { text = "Cancel"
                        , onPress = Just <| GotDeleteCancel (toIdentifier keypair)
                        }
                    ]

            else
                Widget.iconButton
                    (SH.materialStyle context.palette).dangerButton
                    { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                    , text = "Delete"
                    , onPress =
                        Just <| GotDeleteNeedsConfirm (toIdentifier keypair)
                    }
    in
    Element.row
        [ Element.width Element.fill ]
        [ Element.el [ Element.alignRight ] deleteButton ]


toIdentifier : OSTypes.Keypair -> OSTypes.KeypairIdentifier
toIdentifier keypair =
    ( keypair.name, keypair.fingerprint )
