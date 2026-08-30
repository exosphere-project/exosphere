module Page.ObjectStorageContainerDetail exposing (CopyMoveForm, CopyMoveMode(..), Model, Msg(..), aclFieldValue, containerUsageLabel, crumbs, init, update, uploadStatusLabel, uploadsForLevel, view)

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
    , readAclInput : Maybe String
    , writeAclInput : Maybe String

    -- `.rlistings` toggle, and the raw X-Container-Read/Write ACL editor. The plain-language
    , showAdvancedAccess : Bool

    -- user directory — Keystone forbids user enumeration — so both are free-text IDs). `grantWrite`
    , grantProjectInput : String
    , grantUserInput : String
    , grantWrite : Bool
    , copyMove : Maybe CopyMoveForm
    , newFolder : Maybe String
    }


type CopyMoveMode
    = ModeCopy
    | ModeMove


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
    | SelectFilesClicked
    | GotFiles File (List File)
    | GotReadAclInput String
    | GotWriteAclInput String
    | GotShowAdvancedAccess Bool
    | GotGrantProjectInput String
    | GotGrantUserInput String
    | GotGrantWrite Bool
      -- Submit an add-grant: clears the form AND posts the merged (no-clobber) ACL change. The
    | GotAddGrant ObjectStorage.ContainerAclUpdate
    | CopyMoveClicked CopyMoveMode ObjectStorage.ObjectName
    | GotCopyMoveDestContainer ObjectStorage.ContainerName
    | GotCopyMoveDestObject String
    | CopyMoveSubmit
    | CopyMoveCancel
    | NewFolderClicked
    | GotNewFolderInput String
    | NewFolderSubmit
    | NewFolderCancel


init : ObjectStorage.ContainerName -> Maybe ObjectStorage.Prefix -> Model
init containerName maybePrefix =
    { containerName = containerName
    , prefix = maybePrefix
    , dataListModel = DataList.init <| DataList.getDefaultFilterOptions []
    , readAclInput = Nothing
    , writeAclInput = Nothing
    , showAdvancedAccess = False
    , grantProjectInput = ""
    , grantUserInput = ""
    , grantWrite = False
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

        GotReadAclInput raw ->
            ( { model | readAclInput = Just raw }, Cmd.none, SharedMsg.NoOp )

        GotWriteAclInput raw ->
            ( { model | writeAclInput = Just raw }, Cmd.none, SharedMsg.NoOp )

        GotShowAdvancedAccess show ->
            ( { model | showAdvancedAccess = show }, Cmd.none, SharedMsg.NoOp )

        GotGrantProjectInput raw ->
            ( { model | grantProjectInput = raw }, Cmd.none, SharedMsg.NoOp )

        GotGrantUserInput raw ->
            ( { model | grantUserInput = raw }, Cmd.none, SharedMsg.NoOp )

        GotGrantWrite write ->
            ( { model | grantWrite = write }, Cmd.none, SharedMsg.NoOp )

        GotAddGrant aclUpdate ->
            -- Clear the form, then POST the merged (no-clobber) ACL via the existing container-metadata
            ( { model | grantProjectInput = "", grantUserInput = "", grantWrite = False }
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestSetContainerAcl model.containerName aclUpdate
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
                    [ -- The Info strip describes the container itself, so it renders (like the
                      if model.prefix == Nothing then
                        VH.renderRDPP
                            context
                            (GetterSetters.projectLookupObjectStorageContainerMetadata model.containerName project)
                            (context.localization.objectStoreContainer ++ " details")
                            (containerInfoStrip context project currentTime model.containerName)

                      else
                        Element.none
                    , newFolderForm context model
                    , copyMoveForm context project model
                    , uploadQueuePanel context project model
                    , if model.prefix == Nothing then
                        Element.none

                      else
                        breadcrumbTrail context project model
                    , VH.renderRDPP
                        context
                        (GetterSetters.projectLookupObjectStorageListing model.containerName model.prefix project)
                        (pluralize "object")
                        (renderSuccessCase context project currentTime model)
                    , if model.prefix == Nothing then
                        manageAccessSection context project model

                      else
                        Element.none
                    ]
        ]


containerHeader : View.Types.Context -> Project -> Model -> Element.Element Msg
containerHeader context project model =
    Element.row (Text.headingStyleAttrs context.palette)
        [ featherIcon [] Icons.archive
        , Text.text Text.ExtraLarge [] (model.containerName |> toTitleCase)
        , case project.endpoints.swift of
            Just swiftUrl ->
                let
                    maybeMetadata =
                        GetterSetters.projectLookupObjectStorageContainerMetadata model.containerName project
                            |> RDPP.toMaybe
                in
                Element.row
                    [ Element.alignRight, Text.fontSize Text.Body, Font.regular, Element.spacing spacer.px16 ]
                    [ containerInfoToggleTip context project model maybeMetadata
                    , actionsDropdown context project model maybeMetadata swiftUrl
                    ]

            Nothing ->
                Element.none
        ]


containerInfoToggleTip : View.Types.Context -> Project -> Model -> Maybe ObjectStorage.ContainerMetadata -> Element.Element Msg
containerInfoToggleTip context project model maybeMetadata =
    let
        contents =
            case maybeMetadata of
                Just metadata ->
                    Element.column [ Element.spacing spacer.px8, Element.padding spacer.px4 ]
                        (List.filterMap identity
                            [ metadata.storagePolicy
                                |> Maybe.map
                                    (\policy ->
                                        Element.paragraph [ Element.width (Element.px 320) ]
                                            [ Element.text
                                                ("Storage policy, how the "
                                                    ++ context.localization.openstackWithOwnKeystone
                                                    ++ " stores and replicates this data: "
                                                    ++ policy
                                                )
                                            ]
                                    )
                            , Just (aclSummary context metadata)

                            -- When the cloud advertises an S3 endpoint this Swift container is also
                            -- reachable as an S3 bucket of the same name (RGW/s3api share the namespace).
                            , case project.endpoints.s3 of
                                Just _ ->
                                    Just (Element.text ("S3 bucket: " ++ model.containerName))

                                Nothing ->
                                    Nothing
                            ]
                        )

                Nothing ->
                    Element.text "Loading…"

        toggleTipId =
            Helpers.String.hyphenate
                [ "objectStorageContainerInfoToggleTip"
                , project.auth.project.uuid
                , model.containerName
                ]
    in
    ToggleTip.toggleTip
        context
        (SharedMsg << SharedMsg.TogglePopover)
        toggleTipId
        contents
        ST.PositionBottomRight


actionsDropdown : View.Types.Context -> Project -> Model -> Maybe ObjectStorage.ContainerMetadata -> String -> Element.Element Msg
actionsDropdown context project model maybeMetadata swiftUrl =
    let
        dropdownId =
            Helpers.String.hyphenate
                [ "objectStorageContainerActionsDropdown"
                , project.auth.project.uuid
                , model.containerName
                ]

        dropdownContent closeDropdown =
            Element.column [ Element.spacing spacer.px16, Element.width Element.fill ]
                (List.filterMap identity
                    [ Just
                        (copyContainerUrlItem context
                            model
                            ("Copy " ++ context.localization.objectStoreContainer ++ " URL")
                            swiftUrl
                            closeDropdown
                        )
                    , if containerIsPublic project model.containerName then
                        Just
                            (copyContainerUrlItem context
                                model
                                ("Copy public " ++ context.localization.objectStoreContainer ++ " URL")
                                swiftUrl
                                closeDropdown
                            )

                      else
                        Nothing
                    , maybeMetadata |> Maybe.map (makePublicOrPrivateItem context project model)
                    , maybeMetadata |> Maybe.map (deleteContainerDropdownItem context project model)
                    ]
                )

        dropdownTarget toggleDropdownMsg dropdownIsShown =
            Widget.iconButton
                (SH.materialStyle context.palette).button
                { text = "Actions"
                , icon =
                    Element.row [ Element.spacing spacer.px4 ]
                        [ Element.text "Actions"
                        , Icon.sizedFeatherIcon 18 <|
                            if dropdownIsShown then
                                Icons.chevronUp

                            else
                                Icons.chevronDown
                        ]
                , onPress = Just toggleDropdownMsg
                }
    in
    popover context
        (SharedMsg << SharedMsg.TogglePopover)
        { id = dropdownId
        , content = dropdownContent
        , contentStyleAttrs = [ Element.padding spacer.px24 ]
        , position = ST.PositionBottomRight
        , distanceToTarget = Nothing
        , target = dropdownTarget
        , targetStyleAttrs = []
        }


copyContainerUrlItem : View.Types.Context -> Model -> String -> String -> Element.Attribute Msg -> Element.Element Msg
copyContainerUrlItem context model buttonText swiftUrl closeDropdown =
    Element.el
        [ Element.htmlAttribute (Html.Attributes.class "copy-button")
        , Element.htmlAttribute
            (Html.Attributes.attribute "data-clipboard-text"
                (ObjectStorage.publicContainerUrl swiftUrl model.containerName)
            )
        , closeDropdown
        ]
        (Button.button Button.Text
            context.palette
            { text = buttonText
            , onPress = Just NoOp
            }
        )


makePublicOrPrivateItem : View.Types.Context -> Project -> Model -> ObjectStorage.ContainerMetadata -> Element.Element Msg
makePublicOrPrivateItem context project model metadata =
    if ObjectStorage.readAclIsPublic metadata.readAcl then
        makePrivatePopconfirm context
            project
            model
            (setAclMsg project
                model
                (ObjectStorage.aclToChange (ObjectStorage.setAclPublicRead False (currentReadAcl metadata)))
                ObjectStorage.LeaveAcl
            )

    else
        makePublicPopconfirm context
            project
            model
            (setAclMsg project
                model
                (ObjectStorage.aclToChange (ObjectStorage.setAclPublicRead True (currentReadAcl metadata)))
                ObjectStorage.LeaveAcl
            )


{-| The "Delete container" dropdown item: a strengthened popconfirm that spells out that deleting the
container permanently deletes ALL objects inside it (not just the container), keeping the SLO/DLO
caveat. `recursive` (delete the objects first, then the container) follows the object count; when the
count is unknown it defaults to recursive so a non-empty container is never left undeleted.
-}
deleteContainerDropdownItem : View.Types.Context -> Project -> Model -> ObjectStorage.ContainerMetadata -> Element.Element Msg
deleteContainerDropdownItem context project model metadata =
    let
        word =
            context.localization.objectStoreContainer

        recursive =
            metadata.objectCount |> Maybe.map (\n -> n > 0) |> Maybe.withDefault True

        contentsPhrase =
            case metadata.objectCount of
                Just n ->
                    if n > 0 then
                        "all " ++ humanCount context.locale n ++ " " ++ pluralizeCount n "object" ++ " inside it"

                    else
                        "every object inside it"

                Nothing ->
                    "every object inside it"

        popconfirmId =
            Helpers.String.hyphenate
                [ "objectStorageContainerDetailDeletePopconfirm"
                , project.auth.project.uuid
                , model.containerName
                ]

        confirmation =
            Element.column [ Element.spacing spacer.px8, Element.width (Element.px 360) ]
                [ Element.paragraph []
                    [ Element.text ("Delete this " ++ word ++ "?") ]
                , Element.paragraph []
                    [ Element.text
                        ("This permanently deletes "
                            ++ contentsPhrase
                            ++ ", then the "
                            ++ word
                            ++ " itself, not just the "
                            ++ word
                            ++ "."
                        )
                    ]
                , Element.paragraph []
                    [ Element.text "Large objects (SLO/DLO) are NOT detected. Their segments may be left behind. Use the CLI or rclone to clean up large objects." ]
                , Element.text "Are you sure?"
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
                        SharedMsg.RequestDeleteContainer model.containerName recursive
        }
        ST.PositionBottomLeft
        (\togglePopoverMsg _ ->
            Button.button Button.DangerSecondary
                context.palette
                { text = "Delete " ++ word, onPress = Just togglePopoverMsg }
        )


containerInfoStrip : View.Types.Context -> Project -> Time.Posix -> ObjectStorage.ContainerName -> ObjectStorage.ContainerMetadata -> Element.Element Msg
containerInfoStrip context project currentTime containerName metadata =
    let
        subdued =
            Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)

        fact children =
            Element.row [ Element.padding spacer.px8 ] children

        createdFact =
            metadata.createdAt
                |> Maybe.map
                    (\t -> fact [ Element.el [ subdued ] (Element.text "created "), relativeTimeElement currentTime t ])

        countFact =
            metadata.objectCount
                |> Maybe.map
                    (\n ->
                        fact
                            [ Element.text (humanCount context.locale n)
                            , Element.el [ subdued ] (Element.text (" " ++ pluralizeCount n "object"))
                            ]
                    )

        sizeFact =
            metadata.bytesUsed
                |> Maybe.map (\n -> fact [ Element.text (usageBytesLabel context.locale n) ])

        accessFact =
            fact [ Element.el [ subdued ] (Element.text "access "), Element.text (accessWord metadata) ]

        facts =
            List.filterMap identity [ createdFact, countFact, sizeFact, Just accessFact ]

        urlHeader =
            case project.endpoints.swift of
                Just swiftUrl ->
                    [ containerUrlHeaderLabel context containerName (ObjectStorage.publicContainerUrl swiftUrl containerName) ]

                Nothing ->
                    []
    in
    VH.tile context
        (featherIcon [] Icons.info
            :: Element.text "Info"
            :: urlHeader
        )
        [ Element.wrappedRow [ Element.width Element.fill, Element.spaceEvenly ] facts ]


containerUrlHeaderLabel : View.Types.Context -> ObjectStorage.ContainerName -> String -> Element.Element Msg
containerUrlHeaderLabel context containerName fullUrl =
    Element.el
        [ Text.fontSize Text.Small
        , Font.color (SH.toElementColor context.palette.neutral.text.subdued)
        , Element.paddingXY spacer.px12 0
        , Text.fontFamily Text.Mono
        ]
        (Element.row [ Element.spacing spacer.px8 ]
            [ Element.text (truncatedContainerUrlDisplay containerName)
            , clipboardCopyButton context
                { icon = Icons.clipboard
                , accessibilityLabel = "Copy " ++ context.localization.objectStoreContainer ++ " URL"
                , textToCopy = fullUrl
                }
            ]
        )


truncatedContainerUrlDisplay : ObjectStorage.ContainerName -> String
truncatedContainerUrlDisplay containerName =
    let
        maxLen =
            40
    in
    "…/"
        ++ (if String.length containerName > maxLen then
                String.left (maxLen - 1) containerName ++ "…"

            else
                containerName
           )


accessWord : ObjectStorage.ContainerMetadata -> String
accessWord metadata =
    let
        readAcl =
            currentReadAcl metadata
    in
    if ObjectStorage.aclIsPublic readAcl then
        "Public"

    else if List.length (mergedGrantPrincipals readAcl (currentWriteAcl metadata)) > 0 then
        "Private, shared"

    else
        "Private"


aclSummary : View.Types.Context -> ObjectStorage.ContainerMetadata -> Element.Element Msg
aclSummary context metadata =
    let
        readAcl =
            currentReadAcl metadata

        writeAcl =
            currentWriteAcl metadata

        worldLines =
            List.filterMap identity
                [ if ObjectStorage.aclIsPublic readAcl then
                    Just "Anyone with the link can download"

                  else
                    Nothing
                , if ObjectStorage.aclHasListings readAcl then
                    Just "Anyone can list object names"

                  else
                    Nothing
                ]

        grantLines =
            mergedGrantPrincipals readAcl writeAcl
                |> List.map
                    (\( principal, canRead, canWrite ) ->
                        describePrincipal context.localization.unitOfTenancy principal ++ accessSuffix canRead canWrite
                    )
    in
    case worldLines ++ grantLines of
        [] ->
            subduedText context
                ("No one outside this " ++ context.localization.unitOfTenancy ++ " has access.")

        lines ->
            Element.column [ Element.spacing spacer.px4, Element.width Element.fill ]
                (List.map (\line -> Element.paragraph [] [ Element.text line ]) lines)


subduedText : View.Types.Context -> String -> Element.Element Msg
subduedText context label =
    Element.el
        [ Font.color (SH.toElementColor context.palette.neutral.text.subdued) ]
        (Element.text label)


{-| The "Manage access" section. Renders the container's HEAD-container ACL and usage via
`VH.renderRDPP` (loading/error/empty), then the stronger-confirm `.rlistings` option, a raw
advanced-ACL escape hatch, and copyable public share links when public. Public/private changes live
in the Actions dropdown.
-}
manageAccessSection : View.Types.Context -> Project -> Model -> Element.Element Msg
manageAccessSection context project model =
    Element.column
        [ Element.spacing spacer.px16, Element.width Element.fill ]
        [ Text.subheading context.palette
            []
            (featherIcon [] Icons.share2)
            "Manage access"
        , VH.renderRDPP
            context
            (GetterSetters.projectLookupObjectStorageContainerMetadata model.containerName project)
            "access settings"
            (manageAccessContent context project model)
        ]


manageAccessContent : View.Types.Context -> Project -> Model -> ObjectStorage.ContainerMetadata -> Element.Element Msg
manageAccessContent context project model metadata =
    -- add-people form, project-wide quick share, `.rlistings` toggle, raw ACL headers — behind the
    Element.column
        [ Element.spacing spacer.px24, Element.width Element.fill ]
        [ advancedAccessDisclosure context
            model
            [ whoHasAccessGroup context project model metadata
            , addPeopleForm context model metadata
            , shareWithProjectButton context project model metadata
            , listingsAccessControl context project model metadata
            , advancedAclControl context project model metadata
            ]
        ]


{-| A collapsed-by-default "Advanced sharing" disclosure wrapping everything beyond the public
toggle: the "Who has access" grants list + editor, the add-people form, the project-wide quick
share, the `.rlistings` toggle and the raw ACL editor — keeping the surface simple for a normal
user. Mirrors the page-local Bool + toggle idiom used by `Page.ServerCreate`'s advanced-options
section, rendered here with a chevron disclosure header (`Icons.chevronUp/Down`).
-}
advancedAccessDisclosure : View.Types.Context -> Model -> List (Element.Element Msg) -> Element.Element Msg
advancedAccessDisclosure context model contents =
    Element.column [ Element.spacing spacer.px16, Element.width Element.fill ]
        [ Input.button []
            { onPress = Just (GotShowAdvancedAccess (not model.showAdvancedAccess))
            , label =
                Element.row [ Element.spacing spacer.px8 ]
                    [ featherIcon []
                        (if model.showAdvancedAccess then
                            Icons.chevronUp

                         else
                            Icons.chevronDown
                        )
                    , Text.strong "Advanced sharing"
                    ]
            }
        , if model.showAdvancedAccess then
            Element.column
                [ Element.spacing spacer.px24
                , Element.width Element.fill
                , Font.color (SH.toElementColor context.palette.neutral.text.default)
                ]
                contents

          else
            Element.none
        ]


containerUsageLabel : Locale -> ObjectStorage.ContainerMetadata -> Maybe String
containerUsageLabel locale metadata =
    let
        bytesPart =
            metadata.bytesUsed
                |> Maybe.map (\n -> usageBytesLabel locale n ++ " used")

        countPart =
            metadata.objectCount
                |> Maybe.map (\n -> humanCount locale n ++ " " ++ pluralizeCount n "object")
    in
    case List.filterMap identity [ bytesPart, countPart ] of
        [] ->
            Nothing

        parts ->
            Just (String.join " · " parts)


usageBytesLabel : Locale -> Int -> String
usageBytesLabel locale n =
    if n == 0 then
        "0 B"

    else
        let
            ( num, unit ) =
                humanBytes locale n
        in
        num ++ " " ++ unit


{-| Build the SharedMsg that POSTs a no-clobber read/write ACL change for this container.
-}
setAclMsg : Project -> Model -> ObjectStorage.AclChange -> ObjectStorage.AclChange -> Msg
setAclMsg project model read write =
    SharedMsg <|
        SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
            SharedMsg.RequestSetContainerAcl model.containerName { read = read, write = write }


currentReadAcl : ObjectStorage.ContainerMetadata -> ObjectStorage.Acl
currentReadAcl metadata =
    ObjectStorage.parseAcl (Maybe.withDefault "" metadata.readAcl)


currentWriteAcl : ObjectStorage.ContainerMetadata -> ObjectStorage.Acl
currentWriteAcl metadata =
    ObjectStorage.parseAcl (Maybe.withDefault "" metadata.writeAcl)


{-| The "Who has access" group inside the Advanced-sharing disclosure: a plain-language,
single-source-of-truth list of the CURRENT grants (derived from the typed read + write ACLs) plus
the PROVISIONAL RGW note (shown once for the whole grants editor). The sibling add-people form /
quick share / `.rlistings` toggle / raw ACL fields follow it inside the same disclosure. Built on
`ObjectStorage.addGrant`/`removeGrant` (no-clobber).
-}
whoHasAccessGroup : View.Types.Context -> Project -> Model -> ObjectStorage.ContainerMetadata -> Element.Element Msg
whoHasAccessGroup context project model metadata =
    Element.column [ Element.spacing spacer.px12, Element.width Element.fill ]
        [ Text.strong "Who has access"
        , accessGrantList context project model metadata
        , Element.paragraph
            [ Font.color (SH.toElementColor context.palette.neutral.text.subdued) ]
            [ Element.text
                ("Some "
                    ++ pluralize context.localization.openstackWithOwnKeystone
                    ++ " don't support all of these rules yet."
                )
            ]
        ]


{-| The current-grants list. World-access fragments (`.r:*`, `.rlistings`) render as INFORMATIONAL
rows (no remove control — the public/listing toggles manage those); each `project:user` / `project:*`
/ `*:*` grant renders one row (read + write merged per principal) with a remove (X) action.
-}
accessGrantList : View.Types.Context -> Project -> Model -> ObjectStorage.ContainerMetadata -> Element.Element Msg
accessGrantList context project model metadata =
    let
        readAcl =
            currentReadAcl metadata

        writeAcl =
            currentWriteAcl metadata

        infoRows =
            List.filterMap identity
                [ if ObjectStorage.aclIsPublic readAcl then
                    Just (accessInfoRow context "Anyone with the link can download (managed by Make public in the Actions menu above)")

                  else
                    Nothing
                , if ObjectStorage.aclHasListings readAcl then
                    Just (accessInfoRow context "Anyone can list file names (managed by the List object names setting below)")

                  else
                    Nothing
                ]

        grantRows =
            List.map (grantRowView context project model metadata) (mergedGrantPrincipals readAcl writeAcl)

        allRows =
            infoRows ++ grantRows
    in
    if List.isEmpty allRows then
        Element.el
            [ Font.color (SH.toElementColor context.palette.neutral.text.subdued) ]
            (Element.text
                ("No one outside this " ++ context.localization.unitOfTenancy ++ " has been granted access yet.")
            )

    else
        Element.column [ Element.spacing spacer.px8, Element.width Element.fill ] allRows


accessInfoRow : View.Types.Context -> String -> Element.Element Msg
accessInfoRow context label =
    Element.paragraph
        [ Font.color (SH.toElementColor context.palette.neutral.text.subdued) ]
        [ Element.text label ]


mergedGrantPrincipals : ObjectStorage.Acl -> ObjectStorage.Acl -> List ( String, Bool, Bool )
mergedGrantPrincipals readAcl writeAcl =
    let
        reads =
            grantPrincipalsOf readAcl

        writes =
            grantPrincipalsOf writeAcl

        ordered =
            List.foldl
                (\p acc ->
                    if List.member p acc then
                        acc

                    else
                        acc ++ [ p ]
                )
                []
                (reads ++ writes)
    in
    List.map (\p -> ( p, List.member p reads, List.member p writes )) ordered


grantPrincipalsOf : ObjectStorage.Acl -> List String
grantPrincipalsOf acl =
    List.filterMap
        (\grantee ->
            case grantee of
                ObjectStorage.OtherGrantee token ->
                    if String.startsWith "." token then
                        Nothing

                    else
                        Just token

                _ ->
                    Nothing
        )
        acl


{-| One `project:user` / `project:*` / `*:*` grant row: a human sentence plus a remove (X) action that
POSTs the ACL(s) minus this principal (no-clobber; only the header that actually contains it is
touched).
-}
grantRowView : View.Types.Context -> Project -> Model -> ObjectStorage.ContainerMetadata -> ( String, Bool, Bool ) -> Element.Element Msg
grantRowView context project model metadata ( principal, canRead, canWrite ) =
    let
        removeChange acl =
            if List.member (ObjectStorage.OtherGrantee principal) acl then
                ObjectStorage.aclToChange (ObjectStorage.removeGrant principal acl)

            else
                ObjectStorage.LeaveAcl

        onRemove =
            setAclMsg project
                model
                (removeChange (currentReadAcl metadata))
                (removeChange (currentWriteAcl metadata))
    in
    Element.row [ Element.spacing spacer.px8, Element.width Element.fill ]
        [ Element.paragraph [ Element.width Element.fill ]
            [ Element.text (describePrincipal context.localization.unitOfTenancy principal ++ accessSuffix canRead canWrite) ]
        , rowActionIcon context
            { icon = Icons.x
            , accessibilityLabel = "Remove access"
            , onClick = Just onRemove
            , hoverColor = context.palette.danger.textOnNeutralBG |> SH.toElementColor
            }
        ]


describePrincipal : String -> String -> String
describePrincipal tenancyWord principal =
    case String.split ":" principal of
        [ "*", "*" ] ->
            "Any logged-in user"

        [ proj, "*" ] ->
            "Everyone in " ++ tenancyWord ++ " " ++ proj

        [ proj, user ] ->
            user ++ " (" ++ tenancyWord ++ " " ++ proj ++ ")"

        _ ->
            principal


accessSuffix : Bool -> Bool -> String
accessSuffix canRead canWrite =
    if canRead && canWrite then
        ": can read & write"

    else if canWrite then
        ": can write"

    else
        ": can read"


{-| The "Add people" form: free-text project + user IDs (no directory — Keystone forbids user
enumeration — so free text is correct), a read / read+write choice, and an Add button that merges the
grant into the container's ACL(s) via `addGrant` (no-clobber). Leaving the user blank grants the whole
project (`project:*`). The `ContainerAclUpdate` is computed here (current metadata in hand) and carried
on the Msg so the update handler can clear the form.
-}
addPeopleForm : View.Types.Context -> Model -> ObjectStorage.ContainerMetadata -> Element.Element Msg
addPeopleForm context model metadata =
    let
        trimmedProject =
            String.trim model.grantProjectInput

        -- the merged (no-clobber) read/write ACL update, computed lazily only when addable.
        addMsg =
            if String.isEmpty trimmedProject then
                Nothing

            else
                let
                    trimmedUser =
                        String.trim model.grantUserInput

                    principal =
                        if String.isEmpty trimmedUser then
                            trimmedProject ++ ":*"

                        else
                            trimmedProject ++ ":" ++ trimmedUser
                in
                Just
                    (GotAddGrant
                        { read = ObjectStorage.aclToChange (ObjectStorage.addGrant principal (currentReadAcl metadata))
                        , write =
                            if model.grantWrite then
                                ObjectStorage.aclToChange (ObjectStorage.addGrant principal (currentWriteAcl metadata))

                            else if List.member (ObjectStorage.OtherGrantee principal) (currentWriteAcl metadata) then
                                ObjectStorage.aclToChange (ObjectStorage.removeGrant principal (currentWriteAcl metadata))

                            else
                                ObjectStorage.LeaveAcl
                        }
                    )
    in
    Element.column [ Element.spacing spacer.px12, Element.width Element.fill ]
        [ Text.strong "Add people"
        , Text.p []
            [ Text.body
                ("Ask your collaborator for their "
                    ++ context.localization.unitOfTenancy
                    ++ {- @nonlocalized: "share" here is the verb, not the Manila share noun. -} " and user IDs. Leave user blank to share with a whole "
                    ++ context.localization.unitOfTenancy
                    ++ "."
                )
            ]
        , Input.text (VH.inputItemAttributes context.palette)
            { text = model.grantProjectInput
            , placeholder = Just (Input.placeholder [] (Element.text (context.localization.unitOfTenancy ++ " id")))
            , onChange = GotGrantProjectInput
            , label = Input.labelAbove [] (Text.body (toTitleCase context.localization.unitOfTenancy))
            }
        , Input.text (VH.inputItemAttributes context.palette)
            { text = model.grantUserInput
            , placeholder = Just (Input.placeholder [] (Element.text "user id (optional)"))
            , onChange = GotGrantUserInput
            , label = Input.labelAbove [] (Text.body "User")
            }
        , -- Radio idiom per ServerCreate: the label gets VH.radioLabelAttributes (bottom padding) so
          Input.radio [ Element.spacing spacer.px8 ]
            { onChange = GotGrantWrite
            , selected = Just model.grantWrite
            , label = Input.labelAbove VH.radioLabelAttributes (Text.body "Access level")
            , options =
                [ Input.option False (Element.text "Can read (download objects)")
                , Input.option True (Element.text "Can read & write (upload and delete objects)")
                ]
            }
        , Element.el []
            (Button.primary context.palette
                { text = "Add"
                , onPress = addMsg
                }
            )
        ]


{-| One-click "share with everyone in my project": adds a `<my project id>:*` READ grant (the project
UUID, not the name — reliable), leaving the write ACL untouched. No-clobber via `addGrant`.
-}
shareWithProjectButton : View.Types.Context -> Project -> Model -> ObjectStorage.ContainerMetadata -> Element.Element Msg
shareWithProjectButton context project model metadata =
    let
        principal =
            project.auth.project.uuid ++ ":*"

        onPress =
            setAclMsg project
                model
                (ObjectStorage.aclToChange (ObjectStorage.addGrant principal (currentReadAcl metadata)))
                ObjectStorage.LeaveAcl
    in
    Element.el []
        (Button.default context.palette
            { text =
                {- @nonlocalized: "Share" here is the verb, not the Manila share noun. -} "Share with everyone in my " ++ context.localization.unitOfTenancy
            , onPress = Just onPress
            }
        )


makePublicPopconfirm : View.Types.Context -> Project -> Model -> Msg -> Element.Element Msg
makePublicPopconfirm context project model onConfirm =
    let
        popconfirmId =
            Helpers.String.hyphenate
                [ "objectStorageMakePublicPopconfirm"
                , project.auth.project.uuid
                , model.containerName
                ]

        confirmation =
            Element.column [ Element.spacing spacer.px8, Element.width (Element.px 360) ]
                [ Element.paragraph []
                    [ Element.text "Make this "
                    , Element.text context.localization.objectStoreContainer
                    , Element.text " world-readable? Anyone with the link can download any object in it, no login required."
                    ]
                , Element.paragraph []
                    [ Element.text "Object NAMES stay unlistable unless you also enable listing below." ]
                ]
    in
    VH.dangerPopconfirm context
        (SharedMsg << SharedMsg.TogglePopover)
        popconfirmId
        { confirmation = confirmation
        , buttonText = "Make public"
        , buttonVariant = Button.Danger
        , onCancel = Just NoOp
        , onConfirm = Just onConfirm
        }
        ST.PositionBottomLeft
        (\togglePopoverMsg _ ->
            Button.button Button.Secondary
                context.palette
                { text = "Make public", onPress = Just togglePopoverMsg }
        )


{-| The make-private popconfirm: making a container private no longer flips immediately — existing
public links stop working, so it deserves the same confirm-gate as making it public. Plain,
user-level copy (the `X-Remove-Container-Read` revoke stays under the hood).
-}
makePrivatePopconfirm : View.Types.Context -> Project -> Model -> Msg -> Element.Element Msg
makePrivatePopconfirm context project model onConfirm =
    let
        popconfirmId =
            Helpers.String.hyphenate
                [ "objectStorageMakePrivatePopconfirm"
                , project.auth.project.uuid
                , model.containerName
                ]

        confirmation =
            Element.column [ Element.spacing spacer.px8, Element.width (Element.px 360) ]
                [ Element.paragraph []
                    [ Element.text "Make this "
                    , Element.text context.localization.objectStoreContainer
                    , Element.text " private? Existing public links will stop working."
                    ]
                ]
    in
    VH.dangerPopconfirm context
        (SharedMsg << SharedMsg.TogglePopover)
        popconfirmId
        { confirmation = confirmation
        , buttonText = "Make private"
        , buttonVariant = Button.Primary
        , onCancel = Just NoOp
        , onConfirm = Just onConfirm
        }
        ST.PositionBottomLeft
        (\togglePopoverMsg _ ->
            Button.button Button.Secondary
                context.palette
                { text = "Make private", onPress = Just togglePopoverMsg }
        )


{-| The SEPARATE, stronger-confirm "make object names listable" (`.rlistings`) option. Never bundled
with public read — enabling `.rlistings` adds ONLY that fragment and never implicitly grants `.r:*`.
-}
listingsAccessControl : View.Types.Context -> Project -> Model -> ObjectStorage.ContainerMetadata -> Element.Element Msg
listingsAccessControl context project model metadata =
    let
        hasListings =
            currentReadAcl metadata |> ObjectStorage.aclHasListings

        control =
            if hasListings then
                Button.default context.palette
                    { text = "Stop listing object names"
                    , onPress =
                        Just <|
                            setAclMsg project
                                model
                                (ObjectStorage.aclToChange (ObjectStorage.setAclListings False (currentReadAcl metadata)))
                                ObjectStorage.LeaveAcl
                    }

            else
                enableListingsPopconfirm context
                    project
                    model
                    (setAclMsg project
                        model
                        (ObjectStorage.aclToChange (ObjectStorage.setAclListings True (currentReadAcl metadata)))
                        ObjectStorage.LeaveAcl
                    )
    in
    Element.column [ Element.spacing spacer.px8, Element.width Element.fill ]
        [ Text.strong "List object names (.rlistings)"
        , Text.p []
            [ Text.body <|
                if hasListings then
                    "Object names in this "
                        ++ context.localization.objectStoreContainer
                        ++ " are publicly listable."

                else
                    "Object names are not publicly listable. This is a separate, stronger grant than public read."
            ]
        , control
        ]


{-| The enable-`.rlistings` popconfirm, with STRONGER warning copy than public read (it exposes every
object NAME, not just objects whose name is already known).
-}
enableListingsPopconfirm : View.Types.Context -> Project -> Model -> Msg -> Element.Element Msg
enableListingsPopconfirm context project model onConfirm =
    let
        popconfirmId =
            Helpers.String.hyphenate
                [ "objectStorageEnableListingsPopconfirm"
                , project.auth.project.uuid
                , model.containerName
                ]

        confirmation =
            Element.column [ Element.spacing spacer.px8, Element.width (Element.px 360) ]
                [ Element.paragraph []
                    [ Element.text "Allow anyone to LIST every object name in this "
                    , Element.text context.localization.objectStoreContainer
                    , Element.text "? This is stronger than public read: it reveals the full contents, not just objects whose names are already known."
                    ]
                , Element.text "Are you sure?"
                ]
    in
    VH.dangerPopconfirm context
        (SharedMsg << SharedMsg.TogglePopover)
        popconfirmId
        { confirmation = confirmation
        , buttonText = "Enable listing"
        , buttonVariant = Button.Danger
        , onCancel = Just NoOp
        , onConfirm = Just onConfirm
        }
        ST.PositionBottomLeft
        (\togglePopoverMsg _ ->
            Button.button Button.Secondary
                context.palette
                { text = "Make names listable", onPress = Just togglePopoverMsg }
        )


{-| The displayed value of a raw advanced-ACL text field. `Nothing` (untouched) falls back to the
container's current metadata ACL;
`Just s` is the user's edit and always wins — including `Just ""` (edited-to-empty), which shows blank
rather than the metadata fallback, because a cleared field is what drives the `X-Remove-Container-*`
revoke path.
-}
aclFieldValue : Maybe String -> Maybe String -> String
aclFieldValue userEdit metadataValue =
    Maybe.withDefault (Maybe.withDefault "" metadataValue) userEdit


{-| The raw advanced-ACL escape hatch: plain text inputs for `X-Container-Read` and
`X-Container-Write`. Each field defaults to the container's CURRENT ACL until edited. Applying posts
each verbatim (trimmed only); a cleared field sends `X-Remove-Container-*`.

PROVISIONAL: the exact ACL grammar Ceph RGW accepts is unverified (e.g. on a Jetstream2 cloud);
verified on devstack Swift 2.37. These strings are passed through with NO validation beyond
trimming — a power-user escape hatch.

-}
advancedAclControl : View.Types.Context -> Project -> Model -> ObjectStorage.ContainerMetadata -> Element.Element Msg
advancedAclControl context project model metadata =
    let
        readValue =
            aclFieldValue model.readAclInput metadata.readAcl

        writeValue =
            aclFieldValue model.writeAclInput metadata.writeAcl

        applyMsg =
            setAclMsg project
                model
                (model.readAclInput
                    |> Maybe.map ObjectStorage.rawAclChange
                    |> Maybe.withDefault ObjectStorage.LeaveAcl
                )
                (model.writeAclInput
                    |> Maybe.map ObjectStorage.rawAclChange
                    |> Maybe.withDefault ObjectStorage.LeaveAcl
                )
    in
    Element.column [ Element.spacing spacer.px12, Element.width Element.fill ]
        [ Text.strong "Advanced ACL"
        , Text.p []
            [ Text.body "Edit the raw X-Container-Read / X-Container-Write ACL strings directly. Clearing a field revokes that ACL. The exact grammar accepted is server-dependent (PROVISIONAL, unverified on Ceph RGW)." ]
        , Input.text (VH.inputItemAttributes context.palette)
            { text = readValue
            , placeholder = Just (Input.placeholder [] (Element.text ".r:*,project:user"))
            , onChange = GotReadAclInput
            , label = Input.labelAbove [] (Text.body "X-Container-Read")
            }
        , Input.text (VH.inputItemAttributes context.palette)
            { text = writeValue
            , placeholder = Just (Input.placeholder [] (Element.text "project:user"))
            , onChange = GotWriteAclInput
            , label = Input.labelAbove [] (Text.body "X-Container-Write")
            }
        , Element.el []
            (Button.primary context.palette
                { text = "Apply ACL", onPress = Just applyMsg }
            )
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
                case listing.nextMarker of
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
                [ copyPublicLinkAffordance context project model object
                , downloadObjectAffordance context project model object
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


{-| A compact "copy public link" icon button for a single object's action cluster, shown ONLY when the
container is world-readable (`.r:*` in its read ACL, per the typed `containerIsPublic`) AND a swift
endpoint is known. Replaces the old full-URL "Public link:" row + the duplicate open-in-new-tab
anchor: with many objects, ten full URLs is a bad pattern, and the anchor merely re-did the download
button.

The copy works via the standardized `clipboardCopyButton` (clipboard.js `data-clipboard-text`
literal-string mode; see that helper's doc). Reuses `ObjectStorage.publicObjectUrl` (the single place
the direct, un-proxied URL shape lives).

PROVISIONAL: the exact public-URL shape on Ceph RGW is unverified (e.g. on a Jetstream2 cloud);
verified on devstack Swift 2.37.

-}
copyPublicLinkAffordance : View.Types.Context -> Project -> Model -> ObjectStorage.SwiftObject -> Element.Element Msg
copyPublicLinkAffordance context project model object =
    case ( containerIsPublic project model.containerName, project.endpoints.swift ) of
        ( True, Just swiftUrl ) ->
            clipboardCopyButton context
                { icon = Icons.link2
                , accessibilityLabel = "Copy public link"
                , textToCopy = ObjectStorage.publicObjectUrl swiftUrl model.containerName object.name
                }

        _ ->
            Element.none


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


{-| Whether a container is world-readable, per its cached read ACL. Uses the TYPED ACL parse
(`ObjectStorage.readAclIsPublic`) — public iff the read ACL contains the `.r:*` grantee — replacing
the earlier PROVISIONAL `String.contains ".r:*"` substring check. Now populated by the
HEAD-container read; returns False while the metadata cache is empty/loading.
-}
containerIsPublic : Project -> ObjectStorage.ContainerName -> Bool
containerIsPublic project containerName =
    Dict.get containerName project.objectStorageContainerMetadata
        |> Maybe.andThen (\rdpp -> RDPP.withDefault Nothing (RDPP.map .readAcl rdpp))
        |> ObjectStorage.readAclIsPublic


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
