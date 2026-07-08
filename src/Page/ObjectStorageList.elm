module Page.ObjectStorageList exposing (Model, Msg(..), bulkDeletePopconfirmId, deletePopconfirmId, init, update, view)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icons
import Helpers.Formatting exposing (humanBytes, humanCount)
import Helpers.GetterSetters as GetterSetters
import Helpers.ResourceList exposing (listItemColumnAttribs)
import Helpers.String exposing (pluralize, pluralizeCount, toTitleCase)
import Html.Attributes
import OpenStack.ObjectStorage as ObjectStorage
import OpenStack.Types as OSTypes
import Route
import Set
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.DataList as DataList
import Style.Widgets.DeleteButton as DeleteButton
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.IconButton as IconButton
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { showHeading : Bool
    , dataListModel : DataList.Model
    , newContainerName : String
    , s3SecretRevealed : Bool
    }


type Msg
    = NoOp
    | DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg
    | GotNewContainerName String
    | GotCreateContainer ObjectStorage.ContainerName
      -- The Bool is `recursive` (True for a non-empty container: delete its objects first).
    | GotDeleteConfirm ObjectStorage.ContainerName Bool
    | GotS3SecretRevealed Bool
    | GotCreateEc2Credential


init : Bool -> Model
init showHeading =
    { showHeading = showHeading
    , dataListModel = DataList.init <| DataList.getDefaultFilterOptions []
    , newContainerName = ""
    , s3SecretRevealed = False
    }


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        DataListMsg dataListMsg ->
            ( { model | dataListModel = DataList.update dataListMsg model.dataListModel }, Cmd.none, SharedMsg.NoOp )

        GotNewContainerName name ->
            ( { model | newContainerName = name }, Cmd.none, SharedMsg.NoOp )

        GotCreateContainer name ->
            ( { model | newContainerName = "" }
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestCreateContainer name
            )

        GotDeleteConfirm name recursive ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestDeleteContainer name recursive
            )

        GotS3SecretRevealed revealed ->
            ( { model | s3SecretRevealed = revealed }, Cmd.none, SharedMsg.NoOp )

        GotCreateEc2Credential ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project)
                SharedMsg.RequestCreateEc2Credential
            )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        word =
            context.localization.objectStoreContainer

        heading =
            if model.showHeading then
                Text.heading context.palette
                    []
                    (featherIcon [] Icons.archive)
                    (word |> pluralize |> toTitleCase)

            else
                Element.none
    in
    Element.column
        (VH.contentContainer ++ [ Element.spacing spacer.px32 ])
        [ heading
        , case project.endpoints.swift of
            Nothing ->
                Text.p []
                    [ Text.body <|
                        String.join " "
                            [ "Object storage is not available for this"
                            , context.localization.unitOfTenancy ++ "."
                            ]
                    ]

            Just _ ->
                VH.renderRDPP
                    context
                    project.objectStorageContainers
                    (pluralize word)
                    (renderSuccessCase context project model)
        , s3ConnectSection context project model
        ]


s3ConnectSection : View.Types.Context -> Project -> Model -> Element.Element Msg
s3ConnectSection context project model =
    VH.tile
        context
        [ featherIcon [] Icons.hardDrive
        , Element.text "Connect with an S3 client"
        ]
        [ Element.column
            [ Element.spacing spacer.px16
            , Element.width Element.fill
            , Font.color (SH.toElementColor context.palette.neutral.text.default)
            ]
            (case project.endpoints.s3 of
                Just url ->
                    s3ConnectContents context project model url

                Nothing ->
                    let
                        cloudWord =
                            context.localization.openstackWithOwnKeystone
                    in
                    [ Text.p []
                        [ Text.body <|
                            String.join " "
                                [ "S3 access is not enabled on this"
                                , cloudWord ++ "."
                                , "Ask your"
                                , cloudWord
                                , "administrator whether it offers an S3-compatible API (for example, s3api or Ceph RGW)."
                                ]
                        ]
                    ]
            )
        ]


s3ConnectContents : View.Types.Context -> Project -> Model -> String -> List (Element.Element Msg)
s3ConnectContents context project model url =
    let
        containerWord =
            context.localization.objectStoreContainer

        tenancyWord =
            context.localization.unitOfTenancy
    in
    [ Text.p []
        [ Text.body "Use these values to connect an S3-compatible client, such as rclone or Cyberduck, to your object storage." ]
    , VH.compactKVRow "Endpoint URL" (copyableText context.palette [] url)
    , VH.compactKVRow {- @nonlocalized -} "Region" (regionValue context)
    , VH.renderRDPP
        context
        project.ec2Credentials
        (pluralize context.localization.credential)
        (s3CredentialsView context project model url)
    , Text.p []
        [ Text.body <|
            String.join " "
                [ "Each"
                , containerWord
                , "in this"
                , tenancyWord
                , "is an S3 bucket with the same name."
                ]
        ]
    , Text.p []
        [ Text.body "Use path-style addressing (most clients, including rclone, handle this automatically)." ]
    ]


regionValue : View.Types.Context -> Element.Element Msg
regionValue context =
    Element.el [ Font.color (SH.toElementColor context.palette.neutral.text.default) ]
        (Text.body {- @nonlocalized -} "us-east-1 (S3 clients require a region; this value works here).")


s3CredentialsView : View.Types.Context -> Project -> Model -> String -> List OSTypes.Ec2Credential -> Element.Element Msg
s3CredentialsView context _ model url creds =
    case List.head creds of
        Just cred ->
            Element.column [ Element.spacing spacer.px16, Element.width Element.fill ]
                [ VH.compactKVRow "Access key" (copyableText context.palette [] cred.access)
                , VH.compactKVRow "Secret key" (s3SecretValue context model cred.secret)
                , rcloneConfigBlock context url cred
                ]

        Nothing ->
            let
                credentialsWord =
                    pluralize context.localization.credential
            in
            Element.column [ Element.spacing spacer.px16, Element.width Element.fill ]
                [ Text.p []
                    [ Text.body <|
                        String.join " "
                            [ "No S3"
                            , credentialsWord
                            , "yet. Create an access key and secret key pair to connect an S3 client."
                            ]
                    ]
                , Button.primary context.palette
                    { text = "Create " ++ credentialsWord
                    , onPress = Just GotCreateEc2Credential
                    }
                ]


rcloneConfigBlock : View.Types.Context -> String -> OSTypes.Ec2Credential -> Element.Element Msg
rcloneConfigBlock context url cred =
    let
        realSnippet =
            ObjectStorage.rcloneConfigSnippet
                { endpoint = url, access = cred.access, secret = cred.secret }

        maskedSnippet =
            ObjectStorage.rcloneConfigSnippet
                { endpoint = url, access = cred.access, secret = String.repeat 8 "•" }
    in
    Element.column [ Element.spacing spacer.px8, Element.width Element.fill ]
        [ Text.strong "rclone config"
        , maskedScriptBlock context { display = maskedSnippet, clipboard = realSnippet }
        , Element.el
            [ Font.color (SH.toElementColor context.palette.neutral.text.subdued)
            , Text.fontSize Text.Small
            ]
            (Element.text "Copying includes your real secret key.")
        ]


maskedScriptBlock : View.Types.Context -> { display : String, clipboard : String } -> Element.Element Msg
maskedScriptBlock context { display, clipboard } =
    Element.el
        [ Element.inFront <|
            Element.el
                [ Element.alignRight
                , Element.moveLeft (toFloat spacer.px4)
                , Element.moveDown (toFloat spacer.px4)
                ]
                (clipboardCopyButton context
                    { icon = Icons.clipboard
                    , accessibilityLabel = "Copy rclone config"
                    , textToCopy = clipboard
                    }
                )
        , Element.width Element.fill
        , Border.solid
        , Border.width 1
        , Border.rounded 3
        , Element.padding spacer.px8
        , Background.color (SH.toElementColor context.palette.neutral.background.frontLayer)
        , Border.color (SH.toElementColor context.palette.neutral.border)
        ]
        (Element.column
            ([ Element.spacing spacer.px4, Text.fontFamily Text.Mono ]
                ++ Text.typographyAttrs Text.Small
            )
            (display
                |> String.split "\n"
                |> List.map Element.text
            )
        )


s3SecretValue : View.Types.Context -> Model -> String -> Element.Element Msg
s3SecretValue context model secret =
    if model.s3SecretRevealed then
        Element.row [ Element.spacing spacer.px8, Element.width Element.fill ]
            [ copyableText context.palette [] secret
            , rowActionIcon context
                { icon = Icons.eyeOff
                , accessibilityLabel = "Hide secret key"
                , onClick = Just (GotS3SecretRevealed False)
                , hoverColor = context.palette.primary |> SH.toElementColor
                }
            ]

    else
        Element.row [ Element.spacing spacer.px8 ]
            [ Element.text (String.repeat 12 "•")
            , rowActionIcon context
                { icon = Icons.eye
                , accessibilityLabel = "Reveal secret key"
                , onClick = Just (GotS3SecretRevealed True)
                , hoverColor = context.palette.primary |> SH.toElementColor
                }
            ]


renderSuccessCase : View.Types.Context -> Project -> Model -> List ObjectStorage.Container -> Element.Element Msg
renderSuccessCase context project model containers =
    let
        word =
            context.localization.objectStoreContainer
    in
    Element.column
        [ Element.spacing spacer.px32, Element.width Element.fill ]
        [ createContainerForm context model containers
        , if List.isEmpty containers then
            Element.text <|
                String.join " "
                    [ "No"
                    , pluralize word
                    , "yet. Create one above."
                    ]

          else
            DataList.view
                word
                model.dataListModel
                DataListMsg
                context
                []
                (containerView context project)
                (containerRecords containers)
                [ deletionAction context project containers ]
                Nothing
                (Just <| searchByNameFilter context)
        ]


newContainerError : View.Types.Context -> List ObjectStorage.Container -> String -> Maybe String
newContainerError context containers name =
    if String.isEmpty name then
        Nothing

    else
        case ObjectStorage.containerNameError name of
            Just err ->
                Just err

            Nothing ->
                if List.any (\c -> c.name == name) containers then
                    Just <| "A " ++ context.localization.objectStoreContainer ++ " with that name already exists."

                else
                    Nothing


canCreateContainer : View.Types.Context -> List ObjectStorage.Container -> String -> Bool
canCreateContainer context containers name =
    not (String.isEmpty name) && newContainerError context containers name == Nothing


createContainerForm : View.Types.Context -> Model -> List ObjectStorage.Container -> Element.Element Msg
createContainerForm context model containers =
    let
        word =
            context.localization.objectStoreContainer

        name =
            model.newContainerName

        maybeError =
            newContainerError context containers name

        canCreate =
            canCreateContainer context containers name
    in
    Element.column
        [ Element.spacing spacer.px12, Element.width Element.fill ]
        [ Element.row
            [ Element.spacing spacer.px12, Element.width Element.fill ]
            [ Input.text
                (VH.inputItemAttributes context.palette)
                { text = name
                , placeholder = Just (Input.placeholder [] (Element.text "my-container"))
                , onChange = GotNewContainerName
                , label =
                    Input.labelAbove []
                        (VH.requiredLabel context.palette (Element.text ("New " ++ toTitleCase word)))
                }
            , Element.el [ Element.alignBottom ] <|
                Button.primary context.palette
                    { text = "Create"
                    , onPress =
                        if canCreate then
                            Just (GotCreateContainer name)

                        else
                            Nothing
                    }
            ]
        , Element.el
            [ Font.color <| SH.toElementColor context.palette.neutral.text.subdued ]
            (Element.text "Names can't be changed after creation.")
        , case maybeError of
            Just err ->
                Element.el
                    [ Font.color <| SH.toElementColor context.palette.danger.textOnNeutralBG ]
                    (Element.text err)

            Nothing ->
                Element.none
        ]


type alias ContainerRecord =
    DataList.DataRecord { container : ObjectStorage.Container }


containerRecords : List ObjectStorage.Container -> List ContainerRecord
containerRecords containers =
    List.map
        (\container ->
            { id = container.name
            , selectable = True
            , container = container
            }
        )
        containers


searchByNameFilter : View.Types.Context -> DataList.SearchFilter { record | container : ObjectStorage.Container }
searchByNameFilter context =
    { label = "Search:"
    , placeholder = Just <| "Enter " ++ context.localization.objectStoreContainer ++ " name"
    , textToSearch = \record -> record.container.name
    }


rowActionIcon : View.Types.Context -> { icon : Icons.Icon, accessibilityLabel : String, onClick : Maybe Msg, hoverColor : Element.Color } -> Element.Element Msg
rowActionIcon context { icon, accessibilityLabel, onClick, hoverColor } =
    IconButton.sizedClickableIcon []
        { icon = icon
        , accessibilityLabel = accessibilityLabel
        , onClick = onClick
        , color = context.palette.neutral.icon |> SH.toElementColor
        , hoverColor = hoverColor
        , size = 18
        }


rowActionIconLink : View.Types.Context -> { icon : Icons.Icon, accessibilityLabel : String, url : String, hoverColor : Element.Color } -> Element.Element Msg
rowActionIconLink context { icon, accessibilityLabel, url, hoverColor } =
    Element.link
        [ Element.htmlAttribute (Html.Attributes.attribute "aria-label" accessibilityLabel)
        , Element.htmlAttribute (Html.Attributes.attribute "title" accessibilityLabel)
        ]
        { url = url
        , label =
            featherIcon
                [ Element.paddingXY spacer.px4 0
                , Font.color (context.palette.neutral.icon |> SH.toElementColor)
                , Element.mouseOver [ Font.color hoverColor ]
                ]
                (icon |> Icons.withSize 18)
        }


clipboardCopyButton : View.Types.Context -> { icon : Icons.Icon, accessibilityLabel : String, textToCopy : String } -> Element.Element Msg
clipboardCopyButton context { icon, accessibilityLabel, textToCopy } =
    Element.el
        [ Element.htmlAttribute (Html.Attributes.class "copy-button")
        , Element.htmlAttribute (Html.Attributes.attribute "data-clipboard-text" textToCopy)
        ]
        (rowActionIcon context
            { icon = icon
            , accessibilityLabel = accessibilityLabel
            , onClick = Just NoOp
            , hoverColor = context.palette.primary |> SH.toElementColor
            }
        )


containerView : View.Types.Context -> Project -> ContainerRecord -> Element.Element Msg
containerView context project containerRecord =
    let
        { locale } =
            context

        container =
            containerRecord.container

        ( sizeNum, sizeUnit ) =
            humanBytes locale container.bytes

        accentColor =
            context.palette.neutral.text.subdued |> SH.toElementColor
    in
    Element.column
        (listItemColumnAttribs context.palette)
        [ Element.row [ Element.spacing spacer.px12, Element.width Element.fill ]
            [ Element.row [ Element.width Element.fill, Element.spacing spacer.px4 ]
                [ Element.link []
                    { url =
                        Route.toUrl context.urlPathPrefix
                            (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                Route.ObjectStorageContainerDetail container.name Nothing
                            )
                    , label =
                        Element.el
                            (Text.typographyAttrs Text.Emphasized
                                ++ [ Font.color (SH.toElementColor context.palette.primary) ]
                            )
                            (Element.text container.name)
                    }
                , clipboardCopyButton context
                    { icon = Icons.clipboard
                    , accessibilityLabel = "Copy name"
                    , textToCopy = container.name
                    }
                ]
            , Element.row [ Element.alignRight, Element.alignTop, Element.spacing spacer.px8 ]
                [ rowActionIconLink context
                    { icon = Icons.settings
                    , accessibilityLabel = "Sharing & access"
                    , url =
                        Route.toUrl context.urlPathPrefix
                            (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                Route.ObjectStorageContainerDetail container.name Nothing
                            )
                    , hoverColor = context.palette.primary |> SH.toElementColor
                    }
                , deleteContainerPopconfirm context project container
                ]
            ]
        , Element.row [ Element.spacing spacer.px8 ]
            [ Element.el [ Font.color accentColor ]
                (Element.text <|
                    humanCount locale container.count
                        ++ " "
                        ++ pluralizeCount container.count "object"
                )
            , Element.el [ Font.color accentColor ] (Element.text "·")
            , Element.el [ Font.color accentColor ] (Element.text (sizeNum ++ " " ++ sizeUnit))
            ]
        ]


deletePopconfirmId : String -> ObjectStorage.ContainerName -> String
deletePopconfirmId projectUuid containerName =
    Helpers.String.hyphenate
        [ "objectStorageListDeletePopconfirm"
        , projectUuid
        , containerName
        ]


bulkDeletePopconfirmId : String -> String
bulkDeletePopconfirmId projectUuid =
    Helpers.String.hyphenate
        [ "objectStorageListBulkDeletePopconfirm"
        , projectUuid
        ]


{-| Single-row delete popconfirm. For a non-empty container the confirmation spells out the object
count and warns that large objects (SLO/DLO) are NOT detected — their segments may be orphaned; the
recursive delete only handles ordinary objects.
-}
deleteContainerPopconfirm : View.Types.Context -> Project -> ObjectStorage.Container -> Element.Element Msg
deleteContainerPopconfirm context project container =
    let
        word =
            context.localization.objectStoreContainer

        recursive =
            container.count > 0

        popconfirmId =
            deletePopconfirmId project.auth.project.uuid container.name

        confirmation =
            if recursive then
                Element.column [ Element.spacing spacer.px8, Element.width (Element.px 360) ]
                    [ Element.paragraph []
                        [ Element.text <|
                            String.join " "
                                [ "This"
                                , word
                                , "contains"
                                , humanCount context.locale container.count
                                , pluralizeCount container.count "object" ++ "."
                                , "Deleting it will permanently delete every ordinary object inside it, then the"
                                , word ++ "."
                                ]
                        ]
                    , Element.paragraph []
                        [ Element.text <|
                            String.join " "
                                [ "Large objects (SLO/DLO) are NOT detected. Their segments may be left behind."
                                , "Use the CLI or rclone to clean up large objects."
                                ]
                        ]
                    , Element.text "Are you sure?"
                    ]

            else
                Element.text <| "Are you sure you want to delete this " ++ word ++ "?"
    in
    VH.dangerPopconfirm context
        (SharedMsg << SharedMsg.TogglePopover)
        popconfirmId
        { confirmation = confirmation
        , buttonText = "Delete"
        , buttonVariant = Button.Danger
        , onCancel = Just NoOp
        , onConfirm = Just (GotDeleteConfirm container.name recursive)
        }
        ST.PositionBottomRight
        (\togglePopoverMsg _ ->
            rowActionIcon context
                { icon = Icons.trash2
                , accessibilityLabel = "Delete " ++ word
                , onClick = Just togglePopoverMsg
                , hoverColor = context.palette.danger.textOnNeutralBG |> SH.toElementColor
                }
        )


deletionAction :
    View.Types.Context
    -> Project
    -> List ObjectStorage.Container
    -> Set.Set ObjectStorage.ContainerName
    -> Element.Element Msg
deletionAction context project containers containerNames =
    let
        word =
            context.localization.objectStoreContainer

        selectedCount =
            Set.size containerNames

        recursiveFor : ObjectStorage.ContainerName -> Bool
        recursiveFor name =
            List.any (\c -> c.name == name && c.count > 0) containers

        nonEmptySelected =
            containers
                |> List.filter (\c -> Set.member c.name containerNames && c.count > 0)

        totalObjectCount =
            nonEmptySelected |> List.map .count |> List.sum

        popconfirmId =
            bulkDeletePopconfirmId project.auth.project.uuid

        confirmation =
            Element.column [ Element.spacing spacer.px8, Element.width (Element.px 360) ]
                (Element.text
                    (String.join " "
                        [ "Delete"
                        , humanCount context.locale selectedCount
                        , pluralizeCount selectedCount word ++ "?"
                        ]
                    )
                    :: (if totalObjectCount > 0 then
                            -- same SLO/DLO caveat the single-row confirm carries.
                            [ Element.paragraph []
                                [ Element.text <|
                                    String.join " "
                                        [ String.fromInt (List.length nonEmptySelected)
                                        , "of them contain"
                                        , humanCount context.locale totalObjectCount
                                        , pluralizeCount totalObjectCount "object" ++ "."
                                        , "Deleting will permanently delete every ordinary object inside, then the"
                                        , pluralizeCount selectedCount word ++ "."
                                        ]
                                ]
                            , Element.paragraph []
                                [ Element.text <|
                                    String.join " "
                                        [ "Large objects (SLO/DLO) are NOT detected. Their segments may be left behind."
                                        , "Use the CLI or rclone to clean up large objects."
                                        ]
                                ]
                            , Element.text "Are you sure?"
                            ]

                        else
                            []
                       )
                )

        onConfirm =
            SharedMsg <|
                (containerNames
                    |> Set.toList
                    |> List.map
                        (\name ->
                            SharedMsg.ProjectMsg
                                (GetterSetters.projectIdentifier project)
                                (SharedMsg.RequestDeleteContainer name (recursiveFor name))
                        )
                    |> SharedMsg.Batch
                )
    in
    VH.dangerPopconfirm context
        (SharedMsg << SharedMsg.TogglePopover)
        popconfirmId
        { confirmation = confirmation
        , buttonText = "Delete"
        , buttonVariant = Button.Danger
        , onCancel = Just NoOp
        , onConfirm = Just onConfirm
        }
        ST.PositionBottomRight
        (\togglePopoverMsg _ ->
            DeleteButton.deleteIconButtonWithDisabledHint context.palette
                True
                (DeleteButton.Enabled ("Delete " ++ pluralize word))
                (Just togglePopoverMsg)
        )
