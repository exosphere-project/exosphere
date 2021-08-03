module View.ListKeypairs exposing (listKeypairs)

import Element
import Element.Font as Font
import FeatherIcons
import Helpers.String
import Html.Attributes
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Style.Widgets.Card as Card
import Style.Widgets.CopyableText
import Style.Widgets.Icon as Icon
import Types.Msg exposing (Msg(..), ProjectSpecificMsgConstructor(..))
import Types.Project exposing (Project)
import Types.View exposing (KeypairListViewParams, ProjectViewConstructor(..))
import View.Helpers as VH
import View.Types
import Widget


listKeypairs :
    View.Types.Context
    -> Bool
    -> Project
    -> KeypairListViewParams
    -> (KeypairListViewParams -> Msg)
    -> Element.Element Msg
listKeypairs context showHeading project viewParams toMsg =
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
                                ProjectMsg project.auth.project.uuid <|
                                    SetProjectView <|
                                        CreateKeypair "" ""
                        }
                    ]

            else
                Element.column
                    VH.contentContainer
                    (List.map
                        (renderKeypairCard context project viewParams toMsg)
                        keypairs_
                    )
    in
    Element.column
        [ Element.spacing 20, Element.width Element.fill ]
        [ if showHeading then
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


renderKeypairCard :
    View.Types.Context
    -> Project
    -> KeypairListViewParams
    -> (KeypairListViewParams -> Msg)
    -> OSTypes.Keypair
    -> Element.Element Msg
renderKeypairCard context project viewParams toMsg keypair =
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
                , actionButtons context project toMsg viewParams keypair
                ]

        expanded =
            List.member ( keypair.name, keypair.fingerprint ) viewParams.expandedKeypairs
    in
    Card.expandoCard
        context.palette
        expanded
        (\bool ->
            toMsg
                { viewParams
                    | expandedKeypairs =
                        if bool then
                            ( keypair.name, keypair.fingerprint ) :: viewParams.expandedKeypairs

                        else
                            List.filter (\k -> ( keypair.name, keypair.fingerprint ) /= k) viewParams.expandedKeypairs
                }
        )
        (VH.possiblyUntitledResource keypair.name context.localization.pkiPublicKeyForSsh
            |> Element.text
        )
        (if expanded then
            Element.none

         else
            Element.el [ Font.family [ Font.monospace ] ] (Element.text keypair.fingerprint)
        )
        cardBody


actionButtons : View.Types.Context -> Project -> (KeypairListViewParams -> Msg) -> KeypairListViewParams -> OSTypes.Keypair -> Element.Element Msg
actionButtons context project toMsg viewParams keypair =
    let
        confirmationNeeded =
            List.member ( keypair.name, keypair.fingerprint ) viewParams.deleteConfirmations

        deleteButton =
            if confirmationNeeded then
                Element.row [ Element.spacing 10 ]
                    [ Element.text "Confirm delete?"
                    , Widget.iconButton
                        (SH.materialStyle context.palette).dangerButton
                        { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                        , text = "Delete"
                        , onPress =
                            Just <|
                                ProjectMsg
                                    project.auth.project.uuid
                                    (RequestDeleteKeypair keypair.name)
                        }
                    , Widget.textButton
                        (SH.materialStyle context.palette).button
                        { text = "Cancel"
                        , onPress =
                            Just <|
                                toMsg
                                    { viewParams
                                        | deleteConfirmations =
                                            List.filter
                                                ((/=) ( keypair.name, keypair.fingerprint ))
                                                viewParams.deleteConfirmations
                                    }
                        }
                    ]

            else
                Widget.iconButton
                    (SH.materialStyle context.palette).dangerButton
                    { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                    , text = "Delete"
                    , onPress =
                        Just <|
                            toMsg
                                { viewParams
                                    | deleteConfirmations =
                                        ( keypair.name, keypair.fingerprint )
                                            :: viewParams.deleteConfirmations
                                }
                    }
    in
    Element.row
        [ Element.width Element.fill ]
        [ Element.el [ Element.alignRight ] deleteButton ]
