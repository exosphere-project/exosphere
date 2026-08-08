module Page.LoginOpenstack exposing (CredentialFileError(..), CredentialFileOutcome(..), EntryType, FoundCredential, Model, Msg, defaultCreds, headerView, init, maxCredentialFileBytes, readCredentialFile, selectedClouds, update, view)

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
import Helpers.Url
import Html.Events
import Json.Decode
import OpenStack.CloudsYaml
import OpenStack.CredentialFile
import OpenStack.OpenRc
import OpenStack.Types as OSTypes
import Set
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Alert as Alert
import Style.Widgets.Button as Button
import Style.Widgets.Icon exposing (sizedFeatherIcon)
import Style.Widgets.Link as Link
import Style.Widgets.Popover.Popover as Popover
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
    , credentialFileText : String
    , entryType : EntryType
    , credentialFileError : Maybe CredentialFileError
    , foundCredential : Maybe FoundCredential
    , fileIsOverDropZone : Bool
    , clouds : List OpenStack.CloudsYaml.CloudEntry
    , selectedClouds : Set.Set String
    }


type EntryType
    = CredsEntry
    | CredentialFileEntry
    | CloudSelectEntry


type CredentialFileError
    = UnrecognizedFile
    | NoApplicationCredentials
    | IncompleteAppCredential
    | FileTooLarge


{-| A single application credential read out of a file, ready to log in with. There is no form
to edit it in: an application credential is only ever machine generated, so retyping one by hand
is a way to get it wrong rather than a way to get it right.
-}
type alias FoundCredential =
    { name : String
    , authUrl : OSTypes.KeystoneUrl
    , appCredential : OSTypes.ApplicationCredential
    , regionName : Maybe OSTypes.RegionId
    }


{-| Everything a credential file can turn out to be. Kept separate from the model so that the
reading is a pure function of the text, and so that the model update below is only bookkeeping.
-}
type CredentialFileOutcome
    = LogInWith FoundCredential
    | ChooseAmongClouds (List OpenStack.CloudsYaml.CloudEntry)
    | FillInPasswordForm OSTypes.OpenstackLogin
    | CredentialFileProblem CredentialFileError


type Msg
    = GotAuthUrl String
    | GotUserDomain String
    | GotUsername String
    | GotPassword String
    | GotCredentialFileText String
    | GotSelectCredentialFileInput
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
    { creds = Maybe.withDefault defaultCreds maybeCreds
    , credentialFileText = ""
    , entryType = CredsEntry
    , credentialFileError = Nothing
    , foundCredential = Nothing
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


credentialFileHelpPopoverId : String
credentialFileHelpPopoverId =
    "loginOpenstackCredentialFileHelp"


{-| A real openrc.sh or clouds.yaml is a few hundred bytes. This is generous enough to never
reject a genuine one and small enough that a mistaken drop is caught before it is read.
-}
maxCredentialFileBytes : Int
maxCredentialFileBytes =
    256 * 1024


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
            ( updateCreds model { oldCreds | authUrl = authUrl }, Cmd.none, SharedMsg.NoOp )

        GotUserDomain userDomain ->
            ( updateCreds model { oldCreds | userDomain = userDomain }, Cmd.none, SharedMsg.NoOp )

        GotUsername username ->
            ( updateCreds model { oldCreds | username = username }, Cmd.none, SharedMsg.NoOp )

        GotPassword password ->
            ( updateCreds model { oldCreds | password = password }, Cmd.none, SharedMsg.NoOp )

        GotCredentialFileText text ->
            -- Editing the text invalidates whatever the last read of it found.
            ( { model
                | credentialFileText = text
                , credentialFileError = Nothing
                , foundCredential = Nothing
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotSelectCredentialFileInput ->
            ( { model | entryType = CredentialFileEntry, credentialFileError = Nothing }, Cmd.none, SharedMsg.NoOp )

        GotSelectCredsInput ->
            ( { model | entryType = CredsEntry }, Cmd.none, SharedMsg.NoOp )

        GotBrowseForCredentialFile ->
            -- No MIME type filter, because these files are downloaded under many names and
            -- browsers report inconsistent types for them.
            ( model, File.Select.file [] GotCredentialFile, SharedMsg.NoOp )

        GotCredentialFile file ->
            if File.size file > maxCredentialFileBytes then
                -- Refuse before reading, so a wrong file cannot be pulled into memory or into
                -- the paste box at whatever size the user happened to drop.
                ( { model
                    | fileIsOverDropZone = False
                    , credentialFileError = Just FileTooLarge
                    , foundCredential = Nothing
                  }
                , Cmd.none
                , SharedMsg.NoOp
                )

            else
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
readCredentialFile : OSTypes.OpenstackLogin -> String -> CredentialFileOutcome
readCredentialFile existingCreds contents =
    case OpenStack.CredentialFile.detect contents of
        OpenStack.CredentialFile.OpenRcFile ->
            readOpenRc existingCreds contents

        OpenStack.CredentialFile.CloudsYamlFile ->
            readCloudsYaml contents

        OpenStack.CredentialFile.UnrecognizedFile ->
            CredentialFileProblem UnrecognizedFile


readOpenRc : OSTypes.OpenstackLogin -> String -> CredentialFileOutcome
readOpenRc existingCreds contents =
    let
        newCreds =
            OpenStack.OpenRc.processOpenRc existingCreds contents
    in
    case OpenStack.OpenRc.parseOpenRcAppCredential contents of
        Just appCredential ->
            LogInWith
                { name =
                    OpenStack.OpenRc.parseOpenRcProjectName contents
                        |> Maybe.withDefault (Helpers.Url.hostnameFromUrl newCreds.authUrl)
                , authUrl = newCreds.authUrl
                , appCredential = appCredential

                -- OS_REGION_NAME is deliberately not read here. Unlike a clouds.yaml entry, an
                -- OpenRC file is one login that the user may well want in several regions.
                , regionName = Nothing
                }

        Nothing ->
            if OpenStack.OpenRc.openRcUsesAppCredentialAuth contents then
                -- The file says it authenticates with an application credential but does not
                -- carry a whole one, usually because the secret is only shown once and was lost.
                -- Falling back to the password form would silently ask for the wrong thing.
                CredentialFileProblem IncompleteAppCredential

            else
                FillInPasswordForm newCreds


readCloudsYaml : String -> CredentialFileOutcome
readCloudsYaml contents =
    case OpenStack.CloudsYaml.parse contents of
        Err OpenStack.CloudsYaml.NoAppCredentials ->
            CredentialFileProblem NoApplicationCredentials

        Err OpenStack.CloudsYaml.NotCloudsYaml ->
            CredentialFileProblem UnrecognizedFile

        Ok [ cloud ] ->
            LogInWith
                { name = cloud.name
                , authUrl = cloud.authUrl
                , appCredential = cloud.appCredential
                , regionName = cloud.regionName
                }

        Ok clouds ->
            ChooseAmongClouds clouds


{-| Applies an outcome to the model. Bookkeeping only: every decision was made above.
-}
processCredentialFile : Model -> Model
processCredentialFile model =
    case readCredentialFile model.creds model.credentialFileText of
        LogInWith found ->
            { model | foundCredential = Just found, credentialFileError = Nothing }

        ChooseAmongClouds clouds ->
            { model
                | clouds = clouds
                , selectedClouds = clouds |> List.map .name |> Set.fromList
                , entryType = CloudSelectEntry
                , foundCredential = Nothing
                , credentialFileError = Nothing
            }

        FillInPasswordForm creds ->
            { model
                | creds = creds
                , entryType = CredsEntry
                , foundCredential = Nothing
                , credentialFileError = Nothing
            }

        CredentialFileProblem error ->
            { model | credentialFileError = Just error, foundCredential = Nothing }


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
    in
    Element.column (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ case model.entryType of
            CredsEntry ->
                loginOpenstackCredsEntry context model allCredsEntered

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
                        { text = "Use OpenRC or clouds.yaml"
                        , onPress = Just GotSelectCredentialFileInput
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
                            , onPress = credentialFileLoginMsg model
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


{-| One button does both jobs on the credential file screen: read what has been given, and then
log in with what was read.
-}
credentialFileLoginMsg : Model -> Maybe Msg
credentialFileLoginMsg model =
    case model.foundCredential of
        Just found ->
            Just
                (SharedMsg <|
                    SharedMsg.RequestProjectScopedTokenWithAppCredential found.authUrl found.appCredential
                )

        Nothing ->
            if String.isEmpty (String.trim model.credentialFileText) then
                Nothing

            else
                Just GotProcessCredentialFile


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
            , placeholder = Just (Input.placeholder [] (Element.text "Password e.g. correct-horse-battery-staple"))
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


loginOpenstackCredentialFileEntry : View.Types.Context -> Model -> Element.Element Msg
loginOpenstackCredentialFileEntry context model =
    Element.column
        (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ fileDropZone context model
        , credentialFileStatus context model
        , credentialFileHelp context
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
            , placeholder = Just (Input.placeholder [] pastePlaceholder)
            , label = Input.labelHidden "Paste an OpenRC or clouds.yaml file"
            , spellcheck = False
            }
        ]


pastePlaceholder : Element.Element Msg
pastePlaceholder =
    Element.column
        [ Element.spacing spacer.px4, Element.width Element.fill ]
        (List.map Element.text
            [ "export OS_AUTH_URL=https://mycloud.net:5000/v3"
            , "export OS_APPLICATION_CREDENTIAL_ID=e.g. 6ee7d4f9c0a24b1f"
            , "export OS_APPLICATION_CREDENTIAL_SECRET=e.g. hunter2"
            ]
        )


{-| What the last read of the pasted or dropped text found, whether that is a credential to log
in with or a reason it cannot be used.
-}
credentialFileStatus : View.Types.Context -> Model -> Element.Element Msg
credentialFileStatus context model =
    case ( model.foundCredential, model.credentialFileError ) of
        ( Just found, _ ) ->
            Alert.alert [ Element.width Element.fill ]
                context.palette
                { state = Alert.Success
                , showIcon = True
                , showContainer = True
                , content =
                    Element.column [ Element.spacing spacer.px4, Element.width Element.fill ]
                        [ Element.paragraph []
                            [ Element.text
                                (String.concat
                                    [ "Found an application "
                                    , context.localization.credential
                                    , " for "
                                    , found.name
                                    , "."
                                    ]
                                )
                            ]
                        , Element.el [ Text.fontSize Text.Small ] (Element.text found.authUrl)
                        ]
                }

        ( Nothing, Just error ) ->
            Alert.alert [ Element.width Element.fill ]
                context.palette
                { state = Alert.Danger
                , showIcon = True
                , showContainer = True
                , content =
                    Element.paragraph [] [ Element.text (credentialFileErrorText context error) ]
                }

        ( Nothing, Nothing ) ->
            Element.none


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

        IncompleteAppCredential ->
            String.concat
                [ "This openrc file is set up for application "
                , Helpers.String.pluralize context.localization.credential
                , " but does not contain an ID and a secret."
                ]

        FileTooLarge ->
            "This file is too large to be an openrc or clouds.yaml."


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


credentialFileHelp : View.Types.Context -> Element.Element Msg
credentialFileHelp context =
    let
        content _ =
            Element.column
                [ -- A nearby element has no width of its own to shrink to, so give it one.
                  Element.width (Element.px 360)
                , Element.spacing spacer.px12
                ]
                [ Element.paragraph []
                    [ Element.text
                        (String.concat
                            [ "Horizon, your "
                            , context.localization.openstackWithOwnKeystone
                            , " dashboard: open Identity, then Application "
                            , context.localization.credential
                                |> Helpers.String.pluralize
                                |> Helpers.String.toTitleCase
                            , ", create one, and download the file it offers."
                            ]
                        )
                    ]
                , Element.paragraph []
                    [ Element.text
                        (String.concat
                            [ "Another Exosphere, like jetstream2.exosphere.app: open your "
                            , context.localization.unitOfTenancy
                            , ", choose "
                            , context.localization.credential
                                |> Helpers.String.pluralize
                                |> Helpers.String.toTitleCase
                            , ", then Download."
                            ]
                        )
                    ]
                ]

        target togglePopoverMsg _ =
            Element.el
                (Link.linkStyle context.palette
                    ++ [ Element.Events.onClick togglePopoverMsg ]
                )
                (Element.text "How do I get this file?")
    in
    Popover.popover context
        (SharedMsg << SharedMsg.TogglePopover)
        { id = credentialFileHelpPopoverId
        , content = content
        , contentStyleAttrs = []
        , position = ST.PositionBottomLeft
        , distanceToTarget = Nothing
        , target = target
        , targetStyleAttrs = []
        }


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
                    [ Element.text cloud.name
                    , case cloud.regionName of
                        Nothing ->
                            Element.none

                        Just regionName ->
                            Tag.tagNeutral context.palette regionName
                    ]
                )
        }
