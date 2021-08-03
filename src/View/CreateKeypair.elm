module View.CreateKeypair exposing (createKeypair)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.String
import Html.Attributes
import Style.Helpers as SH
import Style.Widgets.FormValidation as FormValidation
import Types.Types
    exposing
        ( Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        )
import Types.View exposing (ProjectViewConstructor(..))
import View.Helpers as VH
import View.Types
import Widget


createKeypair : View.Types.Context -> Project -> String -> String -> Element.Element Msg
createKeypair context project name publicKey =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el (VH.heading2 context.palette) <|
            Element.text <|
                String.join " "
                    [ "Upload"
                    , context.localization.pkiPublicKeyForSsh
                        |> Helpers.String.toTitleCase
                    ]
        , Element.column VH.formContainer
            ([ Input.text
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
                    ++ [ Element.width Element.fill
                       , Element.height (Element.px 300)
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
             ]
                ++ createKeyPairButton context project name publicKey
            )
        ]


createKeyPairButton : View.Types.Context -> Project -> String -> String -> List (Element.Element Msg)
createKeyPairButton context project name publicKey =
    let
        isValid =
            List.all
                identity
                [ String.length name > 0
                , String.length publicKey > 0
                ]

        ( maybeCmd, validation ) =
            if isValid then
                ( Just <|
                    ProjectMsg project.auth.project.uuid <|
                        RequestCreateKeypair name publicKey
                , Element.none
                )

            else
                ( Nothing
                , FormValidation.renderValidationError context "All fields are required"
                )
    in
    [ Element.el [ Element.alignRight ] <|
        Widget.textButton
            (SH.materialStyle context.palette).primaryButton
            { text = "Create"
            , onPress = maybeCmd
            }
    , Element.el [ Element.alignRight ] <|
        validation
    ]
