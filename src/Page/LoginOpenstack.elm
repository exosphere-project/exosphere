module Page.LoginOpenstack exposing (CredentialFileError, EntryType, Model, Msg, defaultCreds, headerView, init, selectedClouds, update, view)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icons
import File
import File.Select
import Helpers.String
import Html.Events
import Json.Decode
import OpenStack.CloudsYaml
import OpenStack.CredentialFile
import OpenStack.OpenRc
import OpenStack.Types as OSTypes
import Set
import Style.Helpers as SH
import Style.Widgets.Button as Button
import Style.Widgets.Icon exposing (sizedFeatherIcon)
import Style.Widgets.Link as Link
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Tag as Tag
import Style.Widgets.Text as Text
import Task
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { creds : OSTypes.OpenstackLogin
    , appCredentialAuthUrl : OSTypes.KeystoneUrl
    , appCredential : OSTypes.ApplicationCredential
    , credentialFileText : String
    , entryType : EntryType
    , credentialFileError : Maybe CredentialFileError
    , fileIsOverDropZone : Bool
    , clouds : List OpenStack.CloudsYaml.CloudEntry
    , selectedClouds : Set.Set String
    }


type EntryType
    = CredsEntry
    | AppCredEntry
    | CredentialFileEntry
    | CloudSelectEntry


type CredentialFileError
    = UnrecognizedFile
    | NoApplicationCredentials


type Msg
    = GotAuthUrl String
    | GotAppCredAuthUrl String
    | GotUserDomain String
    | GotUsername String
    | GotPassword String
    | GotAppCredentialId String
    | GotAppCredentialSecret String
    | GotCredentialFileText String
    | GotSelectCredentialFileInput
    | GotSelectAppCredInput
    | GotSelectCredsInput
    | GotBrowseForCredentialFile
    | GotCredentialFile File.File
    | GotCredentialFileContents String
    | GotFileOverDropZone Bool
    | GotProcessCredentialFile
    | GotCloudChecked String Bool
    | GotAllCloudsChecked Bool
    | SharedMsg SharedMsg.SharedMsg


init : Maybe OSTypes.OpenstackLogin -> Model
init maybeCreds =
    let
        creds =
            Maybe.withDefault defaultCreds maybeCreds
    in
    { creds = creds
    , appCredentialAuthUrl = creds.authUrl
    , appCredential = defaultAppCredential
    , credentialFileText = ""
    , entryType = CredsEntry
    , credentialFileError = Nothing
    , fileIsOverDropZone = False
    , clouds = []
    , selectedClouds = Set.empty
    }


defaultCreds : OSTypes.OpenstackLogin
defaultCreds =
    { authUrl = ""
    , userDomain = ""
    , username = ""
    , password = ""
    }


defaultAppCredential : OSTypes.ApplicationCredential
defaultAppCredential =
    { uuid = ""
    , secret = ""
    }


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    let
        oldCreds =
            model.creds

        updateCreds : Model -> OSTypes.OpenstackLogin -> Model
        updateCreds model_ newCreds =
            { model_ | creds = newCreds }
    in
    case msg of
        GotAuthUrl authUrl ->
            ( { model
                | creds = { oldCreds | authUrl = authUrl }
                , appCredentialAuthUrl = authUrl
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotAppCredAuthUrl authUrl ->
            ( { model
                | creds = { oldCreds | authUrl = authUrl }
                , appCredentialAuthUrl = authUrl
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotUserDomain userDomain ->
            ( updateCreds model { oldCreds | userDomain = userDomain }, Cmd.none, SharedMsg.NoOp )

        GotUsername username ->
            ( updateCreds model { oldCreds | username = username }, Cmd.none, SharedMsg.NoOp )

        GotPassword password ->
            ( updateCreds model { oldCreds | password = password }, Cmd.none, SharedMsg.NoOp )

        GotAppCredentialId appCredId ->
            ( { model
                | appCredential =
                    { uuid = appCredId
                    , secret = model.appCredential.secret
                    }
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotAppCredentialSecret appCredSecret ->
            ( { model
                | appCredential =
                    { uuid = model.appCredential.uuid
                    , secret = appCredSecret
                    }
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotCredentialFileText text ->
            ( { model | credentialFileText = text, credentialFileError = Nothing }, Cmd.none, SharedMsg.NoOp )

        GotSelectCredentialFileInput ->
            ( { model | entryType = CredentialFileEntry, credentialFileError = Nothing }, Cmd.none, SharedMsg.NoOp )

        GotSelectAppCredInput ->
            ( { model | entryType = AppCredEntry }, Cmd.none, SharedMsg.NoOp )

        GotSelectCredsInput ->
            ( { model | entryType = CredsEntry }, Cmd.none, SharedMsg.NoOp )

        GotBrowseForCredentialFile ->
            -- No MIME type filter, because these files are downloaded under many names and
            -- browsers report inconsistent types for them.
            ( model, File.Select.file [] GotCredentialFile, SharedMsg.NoOp )

        GotCredentialFile file ->
            ( { model | fileIsOverDropZone = False }
            , Task.perform GotCredentialFileContents (File.toString file)
            , SharedMsg.NoOp
            )

        GotCredentialFileContents contents ->
            -- Reading a file processes it right away, so a drop is one gesture rather than two.
            ( processCredentialFile { model | credentialFileText = contents }, Cmd.none, SharedMsg.NoOp )

        GotFileOverDropZone isOver ->
            ( { model | fileIsOverDropZone = isOver }, Cmd.none, SharedMsg.NoOp )

        GotProcessCredentialFile ->
            ( processCredentialFile model, Cmd.none, SharedMsg.NoOp )

        GotCloudChecked cloudName checked ->
            ( { model
                | selectedClouds =
                    model.selectedClouds
                        |> (if checked then
                                Set.insert cloudName

                            else
                                Set.remove cloudName
                           )
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotAllCloudsChecked checked ->
            ( { model
                | selectedClouds =
                    if checked then
                        model.clouds |> List.map .name |> Set.fromList

                    else
                        Set.empty
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


{-| Reads whatever the user pasted or dropped, and routes it by what the text contains rather
than by where it came from, so pasting and dropping behave identically.
-}
processCredentialFile : Model -> Model
processCredentialFile model =
    case OpenStack.CredentialFile.detect model.credentialFileText of
        OpenStack.CredentialFile.OpenRcFile ->
            processOpenRc model

        OpenStack.CredentialFile.CloudsYamlFile ->
            processCloudsYaml model

        OpenStack.CredentialFile.UnrecognizedFile ->
            { model | credentialFileError = Just UnrecognizedFile }


processOpenRc : Model -> Model
processOpenRc model =
    let
        newCreds =
            OpenStack.OpenRc.processOpenRc model.creds model.credentialFileText

        maybeAppCredential =
            OpenStack.OpenRc.parseOpenRcAppCredential model.credentialFileText

        entryTypeFromOpenRc =
            case maybeAppCredential of
                Just _ ->
                    AppCredEntry

                Nothing ->
                    if OpenStack.OpenRc.openRcUsesAppCredentialAuth model.credentialFileText then
                        AppCredEntry

                    else
                        CredsEntry
    in
    { model
        | creds = newCreds
        , appCredentialAuthUrl = newCreds.authUrl
        , appCredential = Maybe.withDefault model.appCredential maybeAppCredential
        , entryType = entryTypeFromOpenRc
        , credentialFileError = Nothing
    }


processCloudsYaml : Model -> Model
processCloudsYaml model =
    case OpenStack.CloudsYaml.parse model.credentialFileText of
        Err OpenStack.CloudsYaml.NoAppCredentials ->
            { model | credentialFileError = Just NoApplicationCredentials }

        Err OpenStack.CloudsYaml.NotCloudsYaml ->
            { model | credentialFileError = Just UnrecognizedFile }

        Ok [ cloud ] ->
            let
                oldCreds =
                    model.creds
            in
            -- A single cloud is just an application credential login, so hand the user the form
            -- they would have filled in by hand, already filled in.
            { model
                | creds = { oldCreds | authUrl = cloud.authUrl }
                , appCredentialAuthUrl = cloud.authUrl
                , appCredential = cloud.appCredential
                , entryType = AppCredEntry
                , credentialFileError = Nothing
            }

        Ok clouds ->
            { model
                | clouds = clouds
                , selectedClouds = clouds |> List.map .name |> Set.fromList
                , entryType = CloudSelectEntry
                , credentialFileError = Nothing
            }


headerView : View.Types.Context -> Element.Element msg
headerView context =
    Text.heading context.palette
        VH.headerHeadingAttributes
        Element.none
        "Add an OpenStack Account"


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context _ model =
    let
        allCredsEntered =
            -- These fields must be populated before login can be attempted
            [ model.creds.authUrl
            , model.creds.userDomain
            , model.creds.username
            , model.creds.password
            ]
                |> List.any (\x -> String.isEmpty x)
                |> not

        allAppCredentialFieldsEntered =
            [ model.appCredentialAuthUrl
            , model.appCredential.uuid
            , model.appCredential.secret
            ]
                |> List.any String.isEmpty
                |> not

        credentialFileButtonText =
            "Use OpenRC or clouds.yaml"
    in
    Element.column (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ case model.entryType of
            CredsEntry ->
                loginOpenstackCredsEntry context model allCredsEntered

            AppCredEntry ->
                loginOpenstackAppCredEntry context model allAppCredentialFieldsEntered

            CredentialFileEntry ->
                loginOpenstackCredentialFileEntry context model

            CloudSelectEntry ->
                loginOpenstackCloudSelectEntry context model
        , Element.row
            [ Element.width Element.fill
            , Element.paddingXY 0 spacer.px16 -- so that it looks separate from form fields
            , Element.spacing spacer.px12
            ]
            (case model.entryType of
                CredsEntry ->
                    [ Element.el []
                        (VH.loginPickerButton context
                            |> Element.map SharedMsg
                        )
                    , Button.default
                        context.palette
                        { text = credentialFileButtonText
                        , onPress = Just GotSelectCredentialFileInput
                        }
                    , Button.default
                        context.palette
                        { text = "Use Application " ++ Helpers.String.toTitleCase context.localization.credential
                        , onPress = Just GotSelectAppCredInput
                        }
                    , Element.el [ Element.alignRight ]
                        (Button.primary
                            context.palette
                            { text = "Log In"
                            , onPress =
                                if allCredsEntered then
                                    Just (SharedMsg <| SharedMsg.RequestUnscopedToken model.creds)

                                else
                                    Nothing
                            }
                        )
                    ]

                AppCredEntry ->
                    [ Element.el []
                        (VH.loginPickerButton context
                            |> Element.map SharedMsg
                        )
                    , Button.default
                        context.palette
                        { text = "Use Username and Password"
                        , onPress = Just GotSelectCredsInput
                        }
                    , Button.default
                        context.palette
                        { text = credentialFileButtonText
                        , onPress = Just GotSelectCredentialFileInput
                        }
                    , Element.el [ Element.alignRight ]
                        (Button.primary
                            context.palette
                            { text = "Log In"
                            , onPress =
                                if allAppCredentialFieldsEntered then
                                    Just
                                        (SharedMsg <|
                                            SharedMsg.RequestProjectScopedTokenWithAppCredential
                                                model.appCredentialAuthUrl
                                                model.appCredential
                                        )

                                else
                                    Nothing
                            }
                        )
                    ]

                CredentialFileEntry ->
                    [ Element.el []
                        (Button.default
                            context.palette
                            { text = "Cancel"
                            , onPress = Just GotSelectCredsInput
                            }
                        )
                    , Element.el [ Element.alignRight ]
                        (Button.primary
                            context.palette
                            { text = "Log In"
                            , onPress =
                                if String.isEmpty (String.trim model.credentialFileText) then
                                    Nothing

                                else
                                    Just GotProcessCredentialFile
                            }
                        )
                    ]

                CloudSelectEntry ->
                    [ Element.el []
                        (Button.default
                            context.palette
                            { text = "Cancel"
                            , onPress = Just GotSelectCredentialFileInput
                            }
                        )
                    , Element.el [ Element.alignRight ]
                        (Button.primary
                            context.palette
                            { text = addCloudsButtonText context model
                            , onPress =
                                case selectedClouds model of
                                    [] ->
                                        Nothing

                                    clouds ->
                                        Just
                                            (SharedMsg <|
                                                SharedMsg.Batch <|
                                                    List.map
                                                        (\cloud ->
                                                            SharedMsg.RequestProjectScopedTokenWithAppCredential
                                                                cloud.authUrl
                                                                cloud.appCredential
                                                        )
                                                        clouds
                                            )
                            }
                        )
                    ]
            )
        ]


loginOpenstackCredsEntry : View.Types.Context -> Model -> Bool -> Element.Element Msg
loginOpenstackCredsEntry context model allCredsEntered =
    let
        creds =
            model.creds

        textField text placeholderText onChange labelText =
            Input.text
                (VH.inputItemAttributes context.palette)
                { text = text
                , placeholder = Just (Input.placeholder [] (Element.text placeholderText))
                , onChange = onChange
                , label = Input.labelAbove [ Text.fontSize Text.Small ] (Element.text labelText)
                }
    in
    Element.column
        (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ Element.el [] (Element.text <| "Enter your " ++ Helpers.String.pluralize context.localization.credential)
        , textField
            creds.authUrl
            "OS_AUTH_URL e.g. https://mycloud.net:5000/v3"
            GotAuthUrl
            "Keystone auth URL"
        , textField
            creds.userDomain
            "User domain e.g. default"
            GotUserDomain
            "User Domain (name or ID)"
        , textField
            creds.username
            "User name e.g. demo"
            GotUsername
            "User Name"
        , Input.currentPassword
            (VH.inputItemAttributes context.palette)
            { text = creds.password
            , placeholder = Just (Input.placeholder [] (Element.text "Password"))
            , show = False
            , onChange = GotPassword
            , label = Input.labelAbove [ Text.fontSize Text.Small ] (Element.text "Password")
            }
        , if allCredsEntered then
            Element.none

          else
            Element.el
                [ Element.alignRight
                , Font.color (context.palette.danger.textOnNeutralBG |> SH.toElementColor)
                ]
                (Element.text "All fields are required.")
        ]


loginOpenstackAppCredEntry : View.Types.Context -> Model -> Bool -> Element.Element Msg
loginOpenstackAppCredEntry context model allAppCredentialFieldsEntered =
    let
        appCredentialText =
            "application " ++ context.localization.credential

        appCredentialLabel =
            "Application " ++ Helpers.String.toTitleCase context.localization.credential

        textField text placeholderText onChange labelText =
            Input.text
                (VH.inputItemAttributes context.palette)
                { text = text
                , placeholder = Just (Input.placeholder [] (Element.text placeholderText))
                , onChange = onChange
                , label = Input.labelAbove [ Text.fontSize Text.Small ] (Element.text labelText)
                }
    in
    Element.column
        (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ Element.el [] (Element.text ("Enter your " ++ appCredentialText ++ "."))
        , textField
            model.appCredentialAuthUrl
            "OS_AUTH_URL e.g. https://mycloud.net:5000/v3"
            GotAppCredAuthUrl
            "Keystone auth URL"
        , textField
            model.appCredential.uuid
            (appCredentialLabel ++ " ID")
            GotAppCredentialId
            (appCredentialLabel ++ " ID")
        , Input.currentPassword
            (VH.inputItemAttributes context.palette)
            { text = model.appCredential.secret
            , placeholder = Just (Input.placeholder [] (Element.text (appCredentialText ++ " secret")))
            , show = False
            , onChange = GotAppCredentialSecret
            , label = Input.labelAbove [ Text.fontSize Text.Small ] (Element.text (appCredentialLabel ++ " Secret"))
            }
        , if allAppCredentialFieldsEntered then
            Element.none

          else
            Element.el
                [ Element.alignRight
                , Font.color (context.palette.danger.textOnNeutralBG |> SH.toElementColor)
                ]
                (Element.text "All fields are required.")
        ]


loginOpenstackCredentialFileEntry : View.Types.Context -> Model -> Element.Element Msg
loginOpenstackCredentialFileEntry context model =
    Element.column
        (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ fileDropZone context model
        , orPasteSeparator context
        , Input.multiline
            (VH.inputItemAttributes context.palette
                ++ [ Element.width Element.fill
                   , Element.height (Element.px 200)
                   , Text.fontSize Text.Tiny
                   ]
            )
            { onChange = GotCredentialFileText
            , text = model.credentialFileText
            , placeholder = Nothing
            , label = Input.labelHidden "Paste an OpenRC or clouds.yaml file"
            , spellcheck = False
            }
        , case model.credentialFileError of
            Nothing ->
                Element.none

            Just error ->
                Element.paragraph
                    [ Element.width Element.fill
                    , Font.color (context.palette.danger.textOnNeutralBG |> SH.toElementColor)
                    ]
                    [ Element.text (credentialFileErrorText context error) ]
        ]


credentialFileErrorText : View.Types.Context -> CredentialFileError -> String
credentialFileErrorText context error =
    case error of
        UnrecognizedFile ->
            "This does not look like an openrc or clouds.yaml file."

        NoApplicationCredentials ->
            String.concat
                [ "This clouds.yaml file has no application "
                , Helpers.String.pluralize context.localization.credential
                , "."
                ]


fileDropZone : View.Types.Context -> Model -> Element.Element Msg
fileDropZone context model =
    let
        borderColor =
            if model.fileIsOverDropZone then
                context.palette.primary

            else
                context.palette.neutral.border
    in
    Element.column
        ([ Element.width Element.fill
         , Element.paddingXY spacer.px16 spacer.px32
         , Element.spacing spacer.px8
         , Border.width 2
         , Border.dashed
         , Border.rounded 6
         , Border.color (borderColor |> SH.toElementColor)
         , Background.color (context.palette.neutral.background.frontLayer |> SH.toElementColor)
         ]
            ++ dropZoneEventAttributes
        )
        [ Element.el [ Element.centerX, Font.color (context.palette.neutral.icon |> SH.toElementColor) ]
            (sizedFeatherIcon 24 Icons.upload)
        , Element.row [ Element.centerX, Element.spacing spacer.px4 ]
            [ Element.text "Drop your file here or"
            , Element.el
                (Link.linkStyle context.palette
                    ++ [ Element.Events.onClick GotBrowseForCredentialFile ]
                )
                (Element.text "browse")
            ]
        , Element.el
            [ Element.centerX
            , Text.fontSize Text.Small
            , Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)
            ]
            (Element.text "openrc.sh or clouds.yaml")
        ]


{-| A dropped file only reaches us when the browser's own drag handling is prevented, so every
one of these events must call preventDefault.
-}
dropZoneEventAttributes : List (Element.Attribute Msg)
dropZoneEventAttributes =
    [ hijackOn "dragover" (Json.Decode.succeed (GotFileOverDropZone True))
    , hijackOn "dragleave" (Json.Decode.succeed (GotFileOverDropZone False))
    , hijackOn "drop" droppedFileDecoder
    ]


hijackOn : String -> Json.Decode.Decoder Msg -> Element.Attribute Msg
hijackOn event decoder =
    Element.htmlAttribute <|
        Html.Events.preventDefaultOn event (Json.Decode.map (\msg -> ( msg, True )) decoder)


droppedFileDecoder : Json.Decode.Decoder Msg
droppedFileDecoder =
    Json.Decode.at [ "dataTransfer", "files" ]
        (Json.Decode.oneOrMore (\file _ -> GotCredentialFile file) File.decoder)


orPasteSeparator : View.Types.Context -> Element.Element Msg
orPasteSeparator context =
    let
        line =
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 1)
                , Background.color (context.palette.neutral.border |> SH.toElementColor)
                ]
                Element.none
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing spacer.px12
        , Element.centerY
        ]
        [ line
        , Element.el
            [ Text.fontSize Text.Small
            , Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)
            ]
            (Element.text "or paste it")
        , line
        ]


{-| The clouds the user has ticked, in the order they appear in the file, which is both the
count on the confirm button and the set of logins the confirm button fires.
-}
selectedClouds : Model -> List OpenStack.CloudsYaml.CloudEntry
selectedClouds model =
    model.clouds
        |> List.filter (\cloud -> Set.member cloud.name model.selectedClouds)


addCloudsButtonText : View.Types.Context -> Model -> String
addCloudsButtonText context model =
    let
        count =
            List.length (selectedClouds model)
    in
    String.concat
        [ "Add "
        , String.fromInt count
        , " "
        , Helpers.String.pluralizeCount count context.localization.unitOfTenancy
        ]


loginOpenstackCloudSelectEntry : View.Types.Context -> Model -> Element.Element Msg
loginOpenstackCloudSelectEntry context model =
    let
        allSelected =
            List.length model.clouds == Set.size model.selectedClouds
    in
    Element.column
        (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ Element.paragraph []
            [ Element.text
                (String.concat
                    [ "This clouds.yaml file has "
                    , String.fromInt (List.length model.clouds)
                    , " "
                    , Helpers.String.pluralize context.localization.unitOfTenancy
                    , ". Choose the ones to add."
                    ]
                )
            ]
        , Input.checkbox []
            { checked = allSelected
            , onChange = GotAllCloudsChecked
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (Text.strong "Select all")
            }
        , Element.column
            [ Element.width Element.fill
            , Element.height (Element.maximum 320 Element.shrink)
            , Element.scrollbarY
            , Element.spacing spacer.px12
            , Element.paddingXY 0 spacer.px4
            ]
            (List.map (cloudCheckbox context model.selectedClouds) model.clouds)
        ]


cloudCheckbox : View.Types.Context -> Set.Set String -> OpenStack.CloudsYaml.CloudEntry -> Element.Element Msg
cloudCheckbox context selection cloud =
    Input.checkbox [ Element.width Element.fill ]
        { checked = Set.member cloud.name selection
        , onChange = GotCloudChecked cloud.name
        , icon = Input.defaultCheckbox
        , label =
            Input.labelRight [ Element.width Element.fill ]
                (Element.row [ Element.width Element.fill, Element.spacing spacer.px8 ]
                    [ Element.paragraph [] [ Element.text cloud.name ]
                    , case cloud.regionName of
                        Nothing ->
                            Element.none

                        Just regionName ->
                            Element.el [ Element.alignRight ] (Tag.tagNeutral context.palette regionName)
                    ]
                )
        }
