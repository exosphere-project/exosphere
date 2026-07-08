module Page.ObjectStorageContainerDetail exposing (CopyMoveForm, CopyMoveMode(..), Model, Msg(..), crumbs, init, update, uploadStatusLabel, uploadsForLevel, view)

import Dict
import Element
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icons
import File exposing (File)
import File.Select
import FormatNumber.Locales exposing (Locale)
import Helpers.Formatting exposing (humanBytes, humanCount)
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ResourceList exposing (listItemColumnAttribs)
import Helpers.String exposing (pluralize, pluralizeCount, toTitleCase)
import Html.Attributes
import OpenStack.ObjectStorage as ObjectStorage
import Route
import Set
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.DataList as DataList
import Style.Widgets.HumanTime exposing (relativeTimeElement)
import Style.Widgets.Icon as Icon exposing (featherIcon)
import Style.Widgets.IconButton as IconButton
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.Select as Select
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Spinner as Spinner
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip as ToggleTip
import Time
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { containerName : ObjectStorage.ContainerName
    , prefix : Maybe ObjectStorage.Prefix
    , dataListModel : DataList.Model

    -- The copy/move destination form (page-local): `Nothing` when closed, `Just` while the user is
    -- picking a destination for a single object. One shared form serves both Copy and Move (the mode
    -- flag distinguishes them).
    , copyMove : Maybe CopyMoveForm

    -- The "New folder" inline form (page-local): `Nothing` when closed, `Just currentInput` while open.
    , newFolder : Maybe String
    }


{-| Copy vs. move — the two modes of the shared destination form.
-}
type CopyMoveMode
    = ModeCopy
    | ModeMove


{-| The copy/move destination form's page-local state. `sourceObjectName` is the FULL source object
name; `destContainer`/`destObjectName` are the user-editable destination (prefilled with the current
container + full object name).
-}
type alias CopyMoveForm =
    { mode : CopyMoveMode
    , sourceObjectName : ObjectStorage.ObjectName
    , destContainer : ObjectStorage.ContainerName
    , destObjectName : String
    }


type Msg
    = NoOp
    | DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg
      -- Browser file-picker affordances (not Rest): the button opens the picker; GotFiles carries the
      -- chosen File values (File.Select.files is File -> List File -> msg) on to a SharedMsg upload.
    | SelectFilesClicked
    | GotFiles File (List File)
      -- Copy/move destination form (page-local). CopyMoveClicked opens the shared form prefilled for
      -- one object; the Got* edits update the destination; CopyMoveSubmit clears the form and emits the
      -- RequestCopyObject SharedMsg; CopyMoveCancel just closes it.
    | CopyMoveClicked CopyMoveMode ObjectStorage.ObjectName
    | GotCopyMoveDestContainer ObjectStorage.ContainerName
    | GotCopyMoveDestObject String
    | CopyMoveSubmit
    | CopyMoveCancel
      -- "New folder" inline form (page-local). NewFolderClicked opens it; GotNewFolderInput edits the
      -- name; NewFolderSubmit clears the form + emits RequestCreateFolder; NewFolderCancel closes it.
    | NewFolderClicked
    | GotNewFolderInput String
    | NewFolderSubmit
    | NewFolderCancel


init : ObjectStorage.ContainerName -> Maybe ObjectStorage.Prefix -> Model
init containerName maybePrefix =
    { containerName = containerName
    , prefix = maybePrefix
    , dataListModel = DataList.init <| DataList.getDefaultFilterOptions []
    , copyMove = Nothing
    , newFolder = Nothing
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

        SelectFilesClicked ->
            ( model, File.Select.files [] GotFiles, SharedMsg.NoOp )

        GotFiles firstFile otherFiles ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestUploadObjects model.containerName model.prefix (firstFile :: otherFiles)
            )

        CopyMoveClicked mode objectName ->
            ( { model
                | copyMove =
                    Just
                        { mode = mode
                        , sourceObjectName = objectName
                        , destContainer = model.containerName
                        , destObjectName = objectName
                        }
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotCopyMoveDestContainer container ->
            ( { model | copyMove = Maybe.map (\form -> { form | destContainer = container }) model.copyMove }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotCopyMoveDestObject objectName ->
            ( { model | copyMove = Maybe.map (\form -> { form | destObjectName = objectName }) model.copyMove }
            , Cmd.none
            , SharedMsg.NoOp
            )

        CopyMoveCancel ->
            ( { model | copyMove = Nothing }, Cmd.none, SharedMsg.NoOp )

        NewFolderClicked ->
            ( { model | newFolder = Just "" }, Cmd.none, SharedMsg.NoOp )

        GotNewFolderInput name ->
            ( { model | newFolder = Just name }, Cmd.none, SharedMsg.NoOp )

        NewFolderCancel ->
            ( { model | newFolder = Nothing }, Cmd.none, SharedMsg.NoOp )

        NewFolderSubmit ->
            case model.newFolder of
                Just name ->
                    if ObjectStorage.folderNameError name == Nothing then
                        ( { model | newFolder = Nothing }
                        , Cmd.none
                        , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                            SharedMsg.RequestCreateFolder
                                model.containerName
                                model.prefix
                                (ObjectStorage.folderPlaceholderObjectName model.prefix name)
                        )

                    else
                        ( model, Cmd.none, SharedMsg.NoOp )

                Nothing ->
                    ( model, Cmd.none, SharedMsg.NoOp )

        CopyMoveSubmit ->
            case model.copyMove of
                Just form ->
                    if copyMoveDestError model form == Nothing then
                        ( { model | copyMove = Nothing }
                        , Cmd.none
                        , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                            SharedMsg.RequestCopyObject
                                model.containerName
                                model.prefix
                                form.sourceObjectName
                                form.destContainer
                                form.destObjectName
                                (form.mode == ModeMove)
                        )

                    else
                        ( model, Cmd.none, SharedMsg.NoOp )

                Nothing ->
                    ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> Element.Element Msg
view context project ( currentTime, _ ) model =
    Element.column
        (VH.contentContainer ++ [ Element.spacing spacer.px32 ])
        [ containerHeader context project model
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
                Element.column [ Element.spacing spacer.px24, Element.width Element.fill ]
                    [ newFolderForm context model
                    , copyMoveForm context project model
                    , uploadQueuePanel context project model

                    -- Breadcrumbs sit with the objects list they navigate (below the cards, per the
                    -- ServerDetail-style layout), not up by the page header. At the container ROOT
                    -- the trail would be just the container name — redundant with the page header —
                    -- so it renders only while inside a pseudo-folder.
                    , if model.prefix == Nothing then
                        Element.none

                      else
                        breadcrumbTrail context project model
                    , VH.renderRDPP
                        context
                        (GetterSetters.projectLookupObjectStorageListing model.containerName model.prefix project)
                        (pluralize context.localization.objectStoreContainer)
                        (renderSuccessCase context project currentTime model)
                    ]
        ]


{-| The instance-detail-style page header: the container icon + name. There is deliberately NO
rename/edit affordance: Swift containers cannot be renamed.
-}
containerHeader : View.Types.Context -> Project -> Model -> Element.Element Msg
containerHeader context project model =
    Element.row (Text.headingStyleAttrs context.palette)
        [ featherIcon [] Icons.archive
        , Text.text Text.ExtraLarge [] (model.containerName |> toTitleCase)
        ]


uploadButton : View.Types.Context -> Element.Element Msg
uploadButton context =
    Button.primary context.palette
        { text = "Upload files"
        , onPress = Just SelectFilesClicked
        }


newFolderButton : View.Types.Context -> Element.Element Msg
newFolderButton context =
    Button.button Button.Secondary
        context.palette
        { text = "New folder"
        , onPress = Just NewFolderClicked
        }


newFolderForm : View.Types.Context -> Model -> Element.Element Msg
newFolderForm context model =
    case model.newFolder of
        Nothing ->
            Element.none

        Just name ->
            let
                nameError =
                    ObjectStorage.folderNameError name

                confirmMsg =
                    if nameError == Nothing then
                        Just NewFolderSubmit

                    else
                        Nothing
            in
            Element.column
                (VH.formContainer
                    ++ [ Element.spacing spacer.px16
                       , Element.padding spacer.px16
                       , Border.width 1
                       , Border.rounded 4
                       , Border.color (context.palette.neutral.border |> SH.toElementColor)
                       ]
                )
                [ Text.subheading context.palette [] (featherIcon [] Icons.folderPlus) "New folder"
                , Input.text (VH.inputItemAttributes context.palette)
                    { text = name
                    , placeholder = Just (Input.placeholder [] (Element.text "folder name"))
                    , onChange = GotNewFolderInput
                    , label = Input.labelAbove [] (Text.body "Folder name")
                    }
                , case nameError of
                    Just err ->
                        if String.isEmpty name then
                            Element.none

                        else
                            Element.el [ Font.color (context.palette.danger.textOnNeutralBG |> SH.toElementColor) ]
                                (Element.text err)

                    Nothing ->
                        Element.none
                , Element.row [ Element.spacing spacer.px12 ]
                    [ Button.primary context.palette
                        { text = "Create", onPress = confirmMsg }
                    , Button.button Button.Secondary
                        context.palette
                        { text = "Cancel", onPress = Just NewFolderCancel }
                    ]
                ]


uploadQueuePanel : View.Types.Context -> Project -> Model -> Element.Element Msg
uploadQueuePanel context project model =
    let
        entries =
            uploadsForLevel model.containerName model.prefix project.objectStorageUploads
    in
    if List.isEmpty entries then
        Element.none

    else
        let
            header =
                if List.any ObjectStorage.uploadIsFinished entries then
                    Element.row [ Element.spacing spacer.px12, Element.width Element.fill ]
                        [ Element.el [ Element.width Element.fill ] (Text.strong "Uploads")
                        , Button.button Button.Text
                            context.palette
                            { text = "Clear finished"
                            , onPress =
                                Just <|
                                    SharedMsg <|
                                        SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) SharedMsg.ClearFinishedUploads
                            }
                        ]

                else
                    Text.strong "Uploads"
        in
        Element.column
            [ Element.spacing spacer.px8, Element.width Element.fill ]
            (header :: List.map (uploadRowView context model) entries)


uploadRowView : View.Types.Context -> Model -> ObjectStorage.Upload -> Element.Element Msg
uploadRowView context model upload =
    let
        ( sizeNum, sizeUnit ) =
            humanBytes context.locale upload.sizeBytes

        subduedColor =
            context.palette.neutral.text.subdued |> SH.toElementColor
    in
    Element.row
        (listItemColumnAttribs context.palette ++ [ Element.spacing spacer.px12, Element.width Element.fill ])
        [ Element.el [ Element.width Element.fill ]
            (Element.text (ObjectStorage.stripPrefix model.prefix upload.objectName))
        , Element.el [ Font.color subduedColor ] (Element.text (sizeNum ++ " " ++ sizeUnit))
        , Element.el [ Element.alignRight ] (uploadStatusView context upload.status)
        ]


uploadStatusView : View.Types.Context -> ObjectStorage.UploadStatus -> Element.Element Msg
uploadStatusView context status =
    let
        subduedColor =
            context.palette.neutral.text.subdued |> SH.toElementColor

        dangerColor =
            context.palette.danger.textOnNeutralBG |> SH.toElementColor
    in
    case status of
        ObjectStorage.Queued ->
            Element.el [ Font.color subduedColor ] (Element.text (uploadStatusLabel status))

        ObjectStorage.Uploading ->
            Element.row [ Element.spacing spacer.px8 ]
                [ Spinner.small context.palette
                , Element.el [ Font.color subduedColor ] (Element.text (uploadStatusLabel status))
                ]

        ObjectStorage.Succeeded ->
            Element.el [ Font.color (context.palette.success.textOnNeutralBG |> SH.toElementColor) ] (Element.text (uploadStatusLabel status))

        ObjectStorage.Failed _ ->
            Element.el [ Font.color dangerColor ] (Element.text (uploadStatusLabel status))

        ObjectStorage.Rejected _ ->
            Element.paragraph [ Font.color dangerColor, Element.width (Element.px 320) ] [ Element.text (uploadStatusLabel status) ]


uploadStatusLabel : ObjectStorage.UploadStatus -> String
uploadStatusLabel status =
    case status of
        ObjectStorage.Queued ->
            "Queued"

        ObjectStorage.Uploading ->
            "Uploading"

        ObjectStorage.Succeeded ->
            "Uploaded"

        ObjectStorage.Failed reason ->
            "Failed: " ++ reason

        ObjectStorage.Rejected reason ->
            reason


uploadsForLevel : ObjectStorage.ContainerName -> Maybe ObjectStorage.Prefix -> List ObjectStorage.Upload -> List ObjectStorage.Upload
uploadsForLevel containerName maybePrefix uploads =
    uploads
        |> List.filter (\upload -> upload.containerName == containerName && upload.prefix == maybePrefix)


crumbs : ObjectStorage.ContainerName -> Maybe ObjectStorage.Prefix -> List ( String, Maybe ObjectStorage.Prefix )
crumbs containerName maybePrefix =
    ( containerName, Nothing )
        :: List.map (\( label, prefix ) -> ( label, Just prefix ))
            (ObjectStorage.breadcrumbSegments maybePrefix)


breadcrumbTrail : View.Types.Context -> Project -> Model -> Element.Element Msg
breadcrumbTrail context project model =
    let
        trail : List ( String, Maybe ObjectStorage.Prefix )
        trail =
            crumbs model.containerName model.prefix

        lastIndex =
            List.length trail - 1

        crumbElement index ( label, maybePrefix ) =
            if index == lastIndex then
                Element.el
                    [ Font.color (SH.toElementColor context.palette.neutral.text.default)
                    , Element.width Element.shrink
                    ]
                    (Element.text label)

            else
                Element.link [ Element.width Element.shrink ]
                    { url =
                        Route.toUrl context.urlPathPrefix
                            (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                Route.ObjectStorageContainerDetail model.containerName maybePrefix
                            )
                    , label =
                        Element.el
                            [ Font.color (SH.toElementColor context.palette.primary) ]
                            (Element.text label)
                    }

        upButton =
            case model.prefix of
                Nothing ->
                    Element.none

                Just prefix ->
                    rowActionIconLink context
                        { icon = Icons.cornerLeftUp
                        , accessibilityLabel = "Up one level"
                        , url =
                            Route.toUrl context.urlPathPrefix
                                (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                    Route.ObjectStorageContainerDetail model.containerName (ObjectStorage.parentPrefix prefix)
                                )
                        , hoverColor = context.palette.primary |> SH.toElementColor
                        }
    in
    Element.row [ Element.spacing spacer.px8, Element.width Element.fill ]
        [ upButton
        , Element.el
            [ Element.width (Element.fill |> Element.maximum 600)
            , Element.scrollbarX
            , Element.clipX
            ]
            (Element.row [ Element.spacing spacer.px8 ]
                (trail
                    |> List.indexedMap crumbElement
                    |> List.intersperse (Element.el [ Font.color (SH.toElementColor context.palette.neutral.text.subdued) ] (Element.text "/"))
                )
            )
        ]


renderSuccessCase : View.Types.Context -> Project -> Time.Posix -> Model -> ObjectStorage.ObjectListing -> Element.Element Msg
renderSuccessCase context project currentTime model listing =
    let
        records =
            listingRecords listing

        listToolbar =
            Element.row [ Element.alignRight, Element.spacing spacer.px12 ]
                [ newFolderButton context
                , uploadButton context
                ]
    in
    if List.isEmpty records then
        Element.column
            [ Element.spacing spacer.px24, Element.width Element.fill ]
            [ listToolbar
            , Element.text "No objects at this level."
            ]

    else
        let
            loadMore =
                case ObjectStorage.nextListingMarker ObjectStorage.listingPageLimit listing of
                    Just marker ->
                        loadMoreAffordance context project model listing marker

                    Nothing ->
                        Element.none
        in
        Element.column
            [ Element.spacing spacer.px24, Element.width Element.fill ]
            [ listToolbar
            , DataList.viewHidingNonSelectableLock
                "object"
                model.dataListModel
                DataListMsg
                context
                []
                (rowView context project currentTime model)
                records
                [ deletionAction context project model ]
                Nothing
                (Just <| searchByNameFilter context model)
            , loadMore
            ]


loadMoreAffordance : View.Types.Context -> Project -> Model -> ObjectStorage.ObjectListing -> String -> Element.Element Msg
loadMoreAffordance context project model listing marker =
    let
        shownCount =
            List.length listing.objects + List.length listing.subdirs
    in
    Element.row [ Element.spacing spacer.px12 ]
        [ Element.el [ Font.color (SH.toElementColor context.palette.neutral.text.subdued) ]
            (Element.text ("Showing " ++ humanCount context.locale shownCount ++ " " ++ pluralizeCount shownCount "row"))
        , Button.default context.palette
            { text = "Load more"
            , onPress =
                Just <|
                    SharedMsg <|
                        SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                            SharedMsg.RequestObjectListingPage model.containerName model.prefix (Just marker)
            }
        ]


type RowKind
    = FolderRow ObjectStorage.Prefix
    | ObjectRow ObjectStorage.SwiftObject


type alias ListingRecord =
    DataList.DataRecord { kind : RowKind }


listingRecords : ObjectStorage.ObjectListing -> List ListingRecord
listingRecords listing =
    let
        folderRecords =
            List.map
                (\prefix ->
                    { id = prefix
                    , selectable = False
                    , kind = FolderRow prefix
                    }
                )
                listing.subdirs

        objectRecords =
            List.map
                (\object ->
                    { id = object.name
                    , selectable = True
                    , kind = ObjectRow object
                    }
                )
                listing.objects
    in
    folderRecords ++ objectRecords


rowDisplayName : Model -> RowKind -> String
rowDisplayName model kind =
    case kind of
        FolderRow prefix ->
            ObjectStorage.stripPrefix model.prefix prefix

        ObjectRow object ->
            ObjectStorage.stripPrefix model.prefix object.name


searchByNameFilter : View.Types.Context -> Model -> DataList.SearchFilter { record | kind : RowKind }
searchByNameFilter context model =
    { label = "Search:"
    , placeholder = Just <| "Enter a name in this " ++ context.localization.objectStoreContainer
    , textToSearch = \record -> rowDisplayName model record.kind
    }


rowView : View.Types.Context -> Project -> Time.Posix -> Model -> ListingRecord -> Element.Element Msg
rowView context project currentTime model record =
    case record.kind of
        FolderRow prefix ->
            folderRowView context project model prefix

        ObjectRow object ->
            objectRowView context project currentTime model object


folderRowView : View.Types.Context -> Project -> Model -> ObjectStorage.Prefix -> Element.Element Msg
folderRowView context project model prefix =
    Element.row
        (listItemColumnAttribs context.palette ++ [ Element.spacing spacer.px12 ])
        [ featherIcon [] Icons.folder
        , Element.link []
            { url =
                Route.toUrl context.urlPathPrefix
                    (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                        Route.ObjectStorageContainerDetail model.containerName (Just prefix)
                    )
            , label =
                Element.el
                    (Text.typographyAttrs Text.Emphasized
                        ++ [ Font.color (SH.toElementColor context.palette.primary) ]
                    )
                    (Element.text (ObjectStorage.stripPrefix model.prefix prefix))
            }
        ]


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


objectNameWithCopy : View.Types.Context -> Model -> ObjectStorage.SwiftObject -> Element.Element Msg
objectNameWithCopy context model object =
    let
        displayName =
            ObjectStorage.stripPrefix model.prefix object.name
    in
    Element.row [ Element.spacing spacer.px4, Element.width Element.fill ]
        [ Element.paragraph
            (Text.typographyAttrs Text.Emphasized
                ++ [ Font.color (SH.toElementColor context.palette.neutral.text.default) ]
            )
            [ Element.text displayName ]
        , clipboardCopyButton context
            { icon = Icons.clipboard
            , accessibilityLabel = "Copy name"
            , textToCopy = displayName
            }
        ]


objectRowView : View.Types.Context -> Project -> Time.Posix -> Model -> ObjectStorage.SwiftObject -> Element.Element Msg
objectRowView context project currentTime model object =
    let
        { locale } =
            context

        ( sizeNum, sizeUnit ) =
            humanBytes locale object.bytes

        accentColor =
            context.palette.neutral.text.subdued |> SH.toElementColor
    in
    Element.column
        (listItemColumnAttribs context.palette)
        [ Element.row [ Element.spacing spacer.px12, Element.width Element.fill ]
            [ Element.el [ Element.width Element.fill ]
                (objectNameWithCopy context model object)
            , Element.row [ Element.alignRight, Element.alignTop, Element.spacing spacer.px8 ]
                [ downloadObjectAffordance context project model object
                , copyMoveAffordance context ModeCopy object
                , copyMoveAffordance context ModeMove object
                , deleteObjectPopconfirm context project model object
                ]
            ]
        , Element.row [ Element.spacing spacer.px8 ]
            [ Element.el [ Font.color accentColor ] (Element.text (sizeNum ++ " " ++ sizeUnit))
            , Element.el [ Font.color accentColor ] (Element.text "·")
            , Element.paragraph [ Element.spacing spacer.px8 ]
                [ Element.el [ Font.color accentColor ] (Element.text "modified ")
                , Element.el [ Font.color accentColor ] (relativeTimeElement currentTime object.lastModified)
                ]
            ]
        ]


{-| Private download affordance: an icon button that emits a SharedMsg so `State.State` fires the
credentialed `GET` (via the proxy) and hands the returned bytes to `File.Download.bytes`. A plain
anchor cannot be used for a private object — it cannot carry the auth token + proxy headers.
-}
downloadObjectAffordance : View.Types.Context -> Project -> Model -> ObjectStorage.SwiftObject -> Element.Element Msg
downloadObjectAffordance context project model object =
    rowActionIcon context
        { icon = Icons.download
        , accessibilityLabel = "Download object"
        , onClick =
            Just <|
                SharedMsg <|
                    SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                        SharedMsg.RequestDownloadObject model.containerName object.name
        , hoverColor = context.palette.primary |> SH.toElementColor
        }


copyMoveAffordance : View.Types.Context -> CopyMoveMode -> ObjectStorage.SwiftObject -> Element.Element Msg
copyMoveAffordance context mode object =
    let
        ( icon, label ) =
            case mode of
                ModeCopy ->
                    ( Icons.copy, "Copy object to…" )

                ModeMove ->
                    ( Icons.move, "Move object to…" )
    in
    rowActionIcon context
        { icon = icon
        , accessibilityLabel = label
        , onClick = Just (CopyMoveClicked mode object.name)
        , hoverColor = context.palette.primary |> SH.toElementColor
        }


copyMoveDestError : Model -> CopyMoveForm -> Maybe String
copyMoveDestError model form =
    case ObjectStorage.objectNameError form.destObjectName of
        Just err ->
            Just err

        Nothing ->
            if form.mode == ModeMove && form.destContainer == model.containerName && form.destObjectName == form.sourceObjectName then
                Just "Choose a different destination. A move cannot target the source object."

            else
                Nothing


{-| The shared copy/move destination form (inline panel), rendered only while `model.copyMove` is
`Just`. Destination container is a `Select` fed from the cached container list; destination object path
is a text input prefilled with the source's full name. A static footer warns that copying a large
object (SLO/DLO) copies ONLY its manifest, not its segments (we do not HEAD each object to rule it
out). The confirm button is disabled while the destination is invalid.
-}
copyMoveForm : View.Types.Context -> Project -> Model -> Element.Element Msg
copyMoveForm context project model =
    case model.copyMove of
        Nothing ->
            Element.none

        Just form ->
            let
                word =
                    context.localization.objectStoreContainer

                modeLabel =
                    case form.mode of
                        ModeCopy ->
                            "Copy"

                        ModeMove ->
                            "Move"

                containerNames =
                    RDPP.withDefault [] project.objectStorageContainers
                        |> List.map .name

                destError =
                    copyMoveDestError model form

                confirmMsg =
                    if destError == Nothing then
                        Just CopyMoveSubmit

                    else
                        Nothing
            in
            Element.column
                (VH.formContainer
                    ++ [ Element.spacing spacer.px16
                       , Element.width Element.fill
                       , Element.padding spacer.px16
                       , Border.width 1
                       , Border.rounded 4
                       , Border.color (context.palette.neutral.border |> SH.toElementColor)
                       ]
                )
                [ Text.subheading context.palette
                    []
                    (featherIcon []
                        (case form.mode of
                            ModeCopy ->
                                Icons.copy

                            ModeMove ->
                                Icons.move
                        )
                    )
                    (modeLabel ++ " object")
                , Element.paragraph []
                    [ Element.text ("Source: " ++ form.sourceObjectName) ]
                , Select.select []
                    context.palette
                    { onChange = \maybeValue -> GotCopyMoveDestContainer (Maybe.withDefault model.containerName maybeValue)
                    , options = List.map (\name -> ( name, name )) containerNames
                    , selected = Just form.destContainer
                    , label = "Destination " ++ word
                    }
                , Input.text (VH.inputItemAttributes context.palette)
                    { text = form.destObjectName
                    , placeholder = Just (Input.placeholder [] (Element.text "path/to/object"))
                    , onChange = GotCopyMoveDestObject
                    , label = Input.labelAbove [] (Text.body "Destination object path")
                    }
                , case destError of
                    Just err ->
                        Element.el [ Font.color (context.palette.danger.textOnNeutralBG |> SH.toElementColor) ]
                            (Element.text err)

                    Nothing ->
                        Element.none
                , Element.paragraph [ Font.color (context.palette.neutral.text.subdued |> SH.toElementColor) ]
                    [ Element.text "Note: copying a large object (SLO/DLO) copies only its manifest, not its segments. Use the CLI or rclone to duplicate large objects." ]
                , Element.row [ Element.spacing spacer.px12 ]
                    [ Button.primary context.palette
                        { text = modeLabel, onPress = confirmMsg }
                    , Button.button Button.Secondary
                        context.palette
                        { text = "Cancel", onPress = Just CopyMoveCancel }
                    ]
                ]


deleteObjectPopconfirm : View.Types.Context -> Project -> Model -> ObjectStorage.SwiftObject -> Element.Element Msg
deleteObjectPopconfirm context project model object =
    let
        popconfirmId =
            Helpers.String.hyphenate
                [ "objectStorageObjectDeletePopconfirm"
                , project.auth.project.uuid
                , model.containerName
                , object.name
                ]

        confirmation =
            Element.column [ Element.spacing spacer.px8, Element.width (Element.px 360) ]
                [ Element.paragraph []
                    [ Element.text ("Delete " ++ ObjectStorage.stripPrefix model.prefix object.name ++ "?") ]
                , Element.paragraph []
                    [ Element.text "Deleting a large object's manifest may leave its segments behind (use the CLI or rclone for large objects)." ]
                ]
    in
    VH.dangerPopconfirm context
        (SharedMsg << SharedMsg.TogglePopover)
        popconfirmId
        { confirmation = confirmation
        , buttonText = "Delete"
        , buttonVariant = Button.Danger
        , onCancel = Just NoOp
        , onConfirm =
            Just <|
                SharedMsg <|
                    SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                        SharedMsg.RequestDeleteObject model.containerName model.prefix object.name
        }
        ST.PositionBottomRight
        (\togglePopoverMsg _ ->
            rowActionIcon context
                { icon = Icons.trash2
                , accessibilityLabel = "Delete object"
                , onClick = Just togglePopoverMsg
                , hoverColor = context.palette.danger.textOnNeutralBG |> SH.toElementColor
                }
        )


deletionAction :
    View.Types.Context
    -> Project
    -> Model
    -> Set.Set ObjectStorage.ObjectName
    -> Element.Element Msg
deletionAction context project model objectNames =
    VH.deleteBulkResourcePopconfirm
        context
        project
        (SharedMsg << SharedMsg.TogglePopover)
        { count = Set.size objectNames, word = "object" }
        "objectStorageObjectBulkDeletePopconfirm"
        (Just <|
            SharedMsg <|
                SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                    SharedMsg.RequestBulkDeleteObjects model.containerName model.prefix (Set.toList objectNames)
        )
        (Just NoOp)
