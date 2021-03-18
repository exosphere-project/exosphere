module View.Keypairs exposing (createKeypair, listKeypairs)

import Element
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
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


createKeypair : View.Types.Context -> Project -> String -> String -> Element.Element Msg
createKeypair context project name publicKey =
    Element.column
        VH.exoColumnAttributes
        [ Element.el VH.heading2 <|
            Element.text <|
                String.join " "
                    [ "Upload"
                    , context.localization.pkiPublicKeyForSsh
                        |> Helpers.String.toTitleCase
                    ]
        , Input.text
            (VH.inputItemAttributes context.palette.background)
            { text = name
            , placeholder =
                Just
                    (Input.placeholder []
                        (Element.text <|
                            String.join " "
                                [ "My", context.localization.pkiPublicKeyForSsh ]
                        )
                    )
            , onChange =
                \newName ->
                    ProjectMsg project.auth.project.uuid <|
                        SetProjectView <|
                            CreateKeypair
                                newName
                                publicKey
            , label = Input.labelAbove [] (Element.text "Name")
            }
        , Input.multiline
            (VH.inputItemAttributes context.palette.background
                ++ [ Element.width (Element.px 500)
                   , Element.height (Element.px 400)
                   , Element.padding 7
                   , Element.spacing 5
                   , Html.Attributes.style "word-break" "break-all" |> Element.htmlAttribute
                   , Font.family [ Font.monospace ]
                   , Font.size 12
                   ]
            )
            { text = publicKey
            , placeholder = Just (Input.placeholder [] (Element.text "ssh-rsa ..."))
            , onChange =
                \newPublicKey ->
                    ProjectMsg project.auth.project.uuid <|
                        SetProjectView <|
                            CreateKeypair
                                name
                                newPublicKey
            , label =
                Input.labelAbove
                    [ Element.paddingXY 0 10
                    , Font.family [ Font.sansSerif ]
                    , Font.size 17
                    ]
                    (Element.text "Public Key Value")
            , spellcheck = False
            }
        , Element.el [ Element.alignRight ] <|
            Widget.textButton
                (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                { text = "Create"
                , onPress = Just <| ProjectMsg project.auth.project.uuid <| RequestCreateKeypair name publicKey
                }
        ]


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
                , if List.isEmpty keypairs_ then
                    Element.column
                        VH.exoColumnAttributes
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
                            (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
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
            List.member ( keypair.name, keypair.fingerprint ) deleteConfirmations

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
                                        toProjectViewConstructor
                                            (deleteConfirmations
                                                |> List.filter
                                                    ((/=) ( keypair.name, keypair.fingerprint ))
                                            )
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
                                (SetProjectView <|
                                    toProjectViewConstructor
                                        (( keypair.name, keypair.fingerprint )
                                            :: deleteConfirmations
                                        )
                                )
                    }
    in
    Element.row
        [ Element.width Element.fill ]
        [ Element.el [ Element.alignRight ] deleteButton ]
