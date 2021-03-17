module View.Keypairs exposing (listKeypairs)

import Element
import Element.Font as Font
import Helpers.String
import Html.Attributes
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Style.Widgets.Button
import Style.Widgets.Card as Card
import Style.Widgets.CopyableText
import Types.Types
    exposing
        ( DeleteKeypairConfirmation
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        )
import View.Helpers as VH
import View.Types
import Widget
import Widget.Style.Material


listKeypairs : View.Types.Context -> Project -> List DeleteKeypairConfirmation -> Element.Element Msg
listKeypairs context project deleteConfirmations =
    let
        renderKeypairs : List OSTypes.Keypair -> Element.Element Msg
        renderKeypairs keypairs_ =
            Element.column
                VH.exoColumnAttributes
                [ Element.el VH.heading2 <|
                    Element.text
                        (context.localization.pkiPublicKeyForSsh
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        )
                , Element.column
                    VH.exoColumnAttributes
                    (List.map
                        (renderKeypairCard context project deleteConfirmations)
                        keypairs_
                    )
                ]
    in
    VH.renderWebData
        context
        project.keypairs
        (Helpers.String.pluralize context.localization.pkiPublicKeyForSsh)
        renderKeypairs


renderKeypairCard : View.Types.Context -> Project -> List DeleteKeypairConfirmation -> OSTypes.Keypair -> Element.Element Msg
renderKeypairCard context project deleteConfirmations keypair =
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
                , actionButtons context project ListKeypairs deleteConfirmations keypair
                ]
    in
    Card.exoCard
        context.palette
        (VH.possiblyUntitledResource keypair.name context.localization.pkiPublicKeyForSsh)
        ""
        cardBody


actionButtons : View.Types.Context -> Project -> (List DeleteKeypairConfirmation -> ProjectViewConstructor) -> List DeleteKeypairConfirmation -> OSTypes.Keypair -> Element.Element Msg
actionButtons context project toProjectViewConstructor deleteConfirmations keypair =
    let
        confirmationNeeded =
            List.member keypair.fingerprint deleteConfirmations

        deleteButton =
            if confirmationNeeded then
                Element.row [ Element.spacing 10 ]
                    [ Element.text "Confirm delete?"
                    , Widget.textButton
                        (Style.Widgets.Button.dangerButton context.palette)
                        { text = "Delete"
                        , onPress =
                            Just <|
                                ProjectMsg
                                    project.auth.project.uuid
                                    (RequestDeleteKeypair keypair.name)
                        }
                    , Widget.textButton
                        (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                        { text = "Cancel"
                        , onPress =
                            Just <|
                                ProjectMsg
                                    project.auth.project.uuid
                                    (SetProjectView <|
                                        toProjectViewConstructor (deleteConfirmations |> List.filter ((/=) keypair.fingerprint))
                                    )
                        }
                    ]

            else
                Widget.textButton
                    (Style.Widgets.Button.dangerButton context.palette)
                    { text = "Delete"
                    , onPress =
                        Just <|
                            ProjectMsg
                                project.auth.project.uuid
                                (SetProjectView <| toProjectViewConstructor [ keypair.fingerprint ])
                    }
    in
    Element.row
        [ Element.width Element.fill ]
        [ Element.el [ Element.alignRight ] deleteButton ]
