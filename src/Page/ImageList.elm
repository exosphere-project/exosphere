module Page.ImageList exposing (Model, Msg(..), init, update, view)

import Dict
import Element
import Element.Font as Font
import FeatherIcons as Icons
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (Unit(..), humanNumber)
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ResourceList exposing (listItemColumnAttribs)
import Helpers.String
import Html.Attributes as HtmlA
import OpenStack.Types as OSTypes exposing (Image)
import Route
import Set
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.DataList as DataList
import Style.Widgets.DeleteButton exposing (deleteIconButton, deletePopconfirm)
import Style.Widgets.Icon as Icon exposing (featherIcon, sizedFeatherIcon)
import Style.Widgets.Popover.Popover as Popover
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Tag as Tag
import Style.Widgets.Text as Text
import Types.Defaults
import Types.HelperTypes exposing (Localization)
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types exposing (Context)
import Widget


type alias Model =
    { deletionsAttempted : Set.Set OSTypes.ImageUuid
    , showDeleteButtons : Bool
    , showHeading : Bool
    , dataListModel : DataList.Model
    }


type Msg
    = GotDeleteConfirm OSTypes.ImageUuid
    | GotChangeVisibility OSTypes.ImageUuid OSTypes.ImageVisibility
    | DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Bool -> Bool -> Model
init showDeleteButtons showHeading =
    { deletionsAttempted = Set.empty
    , showDeleteButtons = showDeleteButtons
    , showHeading = showHeading
    , dataListModel = DataList.init <| DataList.getDefaultFilterOptions (filters Types.Defaults.localization)
    }


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotDeleteConfirm imageId ->
            ( { model
                | deletionsAttempted = Set.insert imageId model.deletionsAttempted
              }
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestDeleteImage imageId
            )

        GotChangeVisibility imageId imageVisibility ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestImageVisibilityChange imageId imageVisibility
            )

        DataListMsg dataListMsg ->
            ( { model
                | dataListModel =
                    DataList.update dataListMsg model.dataListModel
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        imagesInCustomOrder =
            let
                images =
                    project.images |> RDPP.withDefault []

                featuredImageNamePrefix =
                    VH.featuredImageNamePrefixLookup context project

                ( featuredImages, nonFeaturedImages_ ) =
                    List.partition (isImageFeaturedByDeployer featuredImageNamePrefix) images

                ( ownImages, otherImages ) =
                    List.partition (\i -> projectOwnsImage project i) nonFeaturedImages_
            in
            List.concat [ featuredImages, ownImages, otherImages ]

        loadedView : List OSTypes.Image -> Element.Element Msg
        loadedView _ =
            Element.column VH.contentContainer
                [ if model.showHeading then
                    Text.heading context.palette
                        []
                        (featherIcon [] Icons.package)
                        (context.localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        )

                  else
                    Element.none
                , DataList.view
                    context.localization.staticRepresentationOfBlockDeviceContents
                    model.dataListModel
                    DataListMsg
                    context
                    []
                    (imageView model context project)
                    (imageRecords context project imagesInCustomOrder)
                    []
                    (Just
                        { filters = filters context.localization
                        , dropdownMsgMapper =
                            \dropdownId ->
                                SharedMsg <| SharedMsg.TogglePopover dropdownId
                        }
                    )
                    (Just searchByNameFilter)
                ]
    in
    VH.renderRDPP context project.images (Helpers.String.pluralize context.localization.staticRepresentationOfBlockDeviceContents) loadedView


projectOwnsImage : Project -> OSTypes.Image -> Bool
projectOwnsImage project image =
    image.projectUuid == project.auth.project.uuid


isImageFeaturedByDeployer : Maybe String -> OSTypes.Image -> Bool
isImageFeaturedByDeployer maybeFeaturedImageNamePrefix image =
    case maybeFeaturedImageNamePrefix of
        Nothing ->
            False

        Just featuredImageNamePrefix ->
            String.startsWith featuredImageNamePrefix image.name
                && image.visibility
                == OSTypes.ImagePublic


type alias ImageRecord =
    DataList.DataRecord
        { image : Image
        , visibility : String
        , featured : Bool
        , owned : Bool
        }


imageRecords : Context -> Project -> List Image -> List ImageRecord
imageRecords context project images =
    let
        featuredImageNamePrefix =
            VH.featuredImageNamePrefixLookup context project
    in
    List.map
        (\image ->
            { id = image.uuid
            , selectable = False
            , image = image
            , visibility = OSTypes.imageVisibilityToString image.visibility
            , featured = isImageFeaturedByDeployer featuredImageNamePrefix image
            , owned = projectOwnsImage project image
            }
        )
        images


imageView : Model -> Context -> Project -> ImageRecord -> Element.Element Msg
imageView model context project imageRecord =
    let
        deleteImageBtn =
            let
                deleteBtn togglePopconfirmMsg _ =
                    let
                        ( deleteBtnText, deleteBtnOnPress ) =
                            if imageRecord.image.protected then
                                ( "Can't delete protected "
                                    ++ context.localization.staticRepresentationOfBlockDeviceContents
                                , Nothing
                                )

                            else
                                ( "Delete "
                                    ++ context.localization.staticRepresentationOfBlockDeviceContents
                                , Just togglePopconfirmMsg
                                )
                    in
                    deleteIconButton
                        context.palette
                        False
                        deleteBtnText
                        deleteBtnOnPress
            in
            if model.showDeleteButtons && projectOwnsImage project imageRecord.image then
                let
                    deletionAttempted =
                        Set.member imageRecord.id model.deletionsAttempted

                    deletionPending =
                        imageRecord.image.status == OSTypes.ImagePendingDelete
                in
                if deletionAttempted || deletionPending then
                    -- FIXME: Constraint progressIndicator svg's height to 36 px also
                    Element.el [ Element.height <| Element.px 36 ]
                        (Widget.circularProgressIndicator (SH.materialStyle context.palette).progressIndicator Nothing)

                else
                    let
                        deletePopconfirmId =
                            Helpers.String.hyphenate
                                [ "ImageListDeletePopconfirm"
                                , project.auth.project.uuid
                                , imageRecord.id
                                ]
                    in
                    deletePopconfirm context
                        (\deletePopconfirmId_ -> SharedMsg <| SharedMsg.TogglePopover deletePopconfirmId_)
                        deletePopconfirmId
                        { confirmation =
                            Element.text <|
                                "Are you sure you want to delete this "
                                    ++ context.localization.staticRepresentationOfBlockDeviceContents
                                    ++ "?"
                        , buttonText = Nothing
                        , onConfirm = Just <| GotDeleteConfirm imageRecord.id
                        , onCancel = Just NoOp
                        }
                        ST.PositionBottomRight
                        deleteBtn

            else
                Element.none

        imageSupportedLabel =
            imageRecord.image.operatingSystem
                |> Maybe.map
                    (\{ supported, distribution } ->
                        case supported of
                            Just True ->
                                Icon.featherIcon [ Font.color (SH.toElementColor context.palette.success.textOnNeutralBG) ] Icons.checkCircle

                            Just False ->
                                Element.row
                                    [ Element.spacing spacer.px4, Font.color (SH.toElementColor context.palette.danger.textOnNeutralBG) ]
                                    [ Icon.featherIcon [] Icons.alertOctagon
                                    , Element.text <|
                                        String.concat
                                            [ distribution
                                            , " is not supported"
                                            ]
                                    ]

                            _ ->
                                Element.none
                    )
                |> Maybe.withDefault Element.none

        createServerBtn =
            let
                textBtn onPress =
                    Button.default
                        context.palette
                        { text =
                            "Create "
                                ++ Helpers.String.toTitleCase
                                    context.localization.virtualComputer
                        , onPress = onPress
                        }
            in
            case imageRecord.image.status of
                OSTypes.ImageActive ->
                    let
                        serverCreationRoute =
                            Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                Route.ServerCreate
                                    imageRecord.image.uuid
                                    imageRecord.image.name
                                    Nothing
                                    (GetterSetters.getUserAppProxyFromContext project context
                                        |> Maybe.map (\_ -> True)
                                    )
                    in
                    Element.link []
                        { url = Route.toUrl context.urlPathPrefix serverCreationRoute
                        , label = textBtn (Just NoOp)
                        }

                _ ->
                    Element.el
                        [ Element.htmlAttribute <|
                            HtmlA.title
                                (Helpers.String.toTitleCase context.localization.staticRepresentationOfBlockDeviceContents
                                    ++ " is not active!"
                                )
                        ]
                        (textBtn Nothing)

        imageActions =
            Element.row [ Element.alignRight, Element.spacing spacer.px12 ]
                [ imageSupportedLabel
                , deleteImageBtn
                , createServerBtn
                , if imageRecord.owned then
                    imageVisibilityDropdown imageRecord context project

                  else
                    Element.none
                ]

        featuredIcon =
            if imageRecord.featured then
                featherIcon [ Element.htmlAttribute <| HtmlA.title "Featured" ]
                    (Icons.award |> Icons.withSize 20)

            else
                Element.none

        imageAttributesView =
            let
                ownerText =
                    if imageRecord.owned then
                        Just <|
                            Element.row []
                                [ Element.el [ Font.color (SH.toElementColor context.palette.neutral.text.default) ]
                                    (Element.text "belongs")
                                , Element.text <|
                                    " to this "
                                        ++ context.localization.unitOfTenancy
                                ]

                    else
                        Nothing

                imageTags =
                    if List.isEmpty imageRecord.image.tags then
                        Nothing

                    else
                        Just <|
                            Element.row
                                [ Element.spacing spacer.px8
                                , Element.paddingEach { left = spacer.px8, top = 0, right = 0, bottom = 0 }
                                ]
                                (List.map (Tag.tag context.palette) imageRecord.image.tags)

                imageType =
                    case imageRecord.image.imageType of
                        Just imageTypeName ->
                            imageTypeName

                        Nothing ->
                            context.localization.staticRepresentationOfBlockDeviceContents

                attributesAlwaysShown =
                    [ if imageRecord.image.status == OSTypes.ImageQueued then
                        Element.text "Building..."
                            |> Element.el
                                [ context.palette.neutral.text.default
                                    |> SH.toElementColor
                                    |> Font.color
                                ]

                      else
                        Element.text <|
                            case imageRecord.image.size of
                                Just s ->
                                    let
                                        { locale } =
                                            context

                                        ( count, units ) =
                                            humanNumber { locale | decimals = Exact 2 } Bytes s
                                    in
                                    count ++ " " ++ units

                                Nothing ->
                                    "unknown size"
                    , Element.row []
                        [ Element.el [ Font.color (SH.toElementColor context.palette.neutral.text.default) ]
                            (Element.text <| String.toLower <| OSTypes.imageVisibilityToString imageRecord.image.visibility)
                        , Element.text <| " " ++ imageType
                        ]
                    ]

                attributesMaybeShown =
                    [ ownerText
                    , imageTags
                    ]

                attributesShown =
                    attributesAlwaysShown ++ List.filterMap identity attributesMaybeShown

                separator =
                    Element.text "·"
            in
            Element.row [ Element.width Element.fill, Element.spacing spacer.px8 ] <|
                List.intersperse separator attributesShown
    in
    Element.column
        (listItemColumnAttribs context.palette)
        [ Element.row [ Element.width Element.fill, Element.spacing spacer.px12 ]
            [ Element.el
                (Text.typographyAttrs Text.Emphasized ++ [ Font.color (SH.toElementColor context.palette.neutral.text.default) ])
                (Element.text (VH.resourceName (Just imageRecord.image.name) imageRecord.image.uuid))
            , featuredIcon
            , imageActions
            ]
        , imageAttributesView
        ]


imageVisibilityDropdown : ImageRecord -> View.Types.Context -> Project -> Element.Element Msg
imageVisibilityDropdown imageRecord context project =
    let
        dropdownId =
            [ "imageVisibilityDropdown", project.auth.project.uuid, imageRecord.image.uuid ]
                |> List.intersperse "-"
                |> String.concat

        dropdownContent closeDropdown =
            (Element.column [ Element.spacing spacer.px8 ] <|
                setVisibilityButtons context imageRecord
            )
                |> renderActionButton closeDropdown

        dropdownTarget toggleDropdownMsg dropdownIsShown =
            Widget.iconButton
                (SH.materialStyle context.palette).button
                { text = "(Set visibility)"
                , icon =
                    Element.row
                        [ Element.spacing spacer.px4 ]
                        [ Element.text "Set visibility"
                        , sizedFeatherIcon 18 <|
                            if dropdownIsShown then
                                Icons.chevronUp

                            else
                                Icons.chevronDown
                        ]
                , onPress = Just toggleDropdownMsg
                }
    in
    Popover.popover context
        popoverMsgMapper
        { id = dropdownId
        , content = dropdownContent
        , contentStyleAttrs = []
        , position = ST.PositionBottomRight
        , distanceToTarget = Nothing
        , target = dropdownTarget
        , targetStyleAttrs = []
        }


allowedTransitions : OSTypes.ImageVisibility -> List OSTypes.ImageVisibility
allowedTransitions existingVisibility =
    [ OSTypes.ImagePrivate
    , OSTypes.ImageCommunity
    ]
        |> List.filter (\v -> v /= existingVisibility)


setVisibilityButtons : Context -> ImageRecord -> List (Element.Element Msg)
setVisibilityButtons context imageRecord =
    imageRecord.image.visibility
        |> allowedTransitions
        |> List.map (setVisibilityBtn context imageRecord.image.uuid)


renderActionButton : Element.Attribute Msg -> Element.Element Msg -> Element.Element Msg
renderActionButton closeActionsDropdown element =
    Element.el [ closeActionsDropdown ] element


setVisibilityBtn : View.Types.Context -> OSTypes.ImageUuid -> OSTypes.ImageVisibility -> Element.Element Msg
setVisibilityBtn context imageUuid visibility =
    let
        onPress =
            GotChangeVisibility imageUuid visibility
    in
    Widget.iconButton
        (Popover.dropdownItemStyle context.palette)
        { icon =
            Element.row
                [ Element.spacing spacer.px12 ]
                [ sizedFeatherIcon 16 <|
                    case visibility of
                        OSTypes.ImagePublic ->
                            Icons.unlock

                        OSTypes.ImagePrivate ->
                            Icons.lock

                        OSTypes.ImageCommunity ->
                            Icons.users

                        OSTypes.ImageShared ->
                            Icons.share
                , Element.text (OSTypes.imageVisibilityToString visibility)
                ]
        , text =
            OSTypes.imageVisibilityToString visibility
        , onPress =
            Just onPress
        }


filters :
    Localization
    ->
        List
            (DataList.Filter
                { record
                    | image : Image
                    , visibility : String
                    , owned : Bool
                }
            )
filters localization =
    [ { id = "visibility"
      , label = "Visibility"
      , chipPrefix = "Visibilty is "
      , filterOptions =
            \images ->
                List.map .visibility images
                    |> Set.fromList
                    |> Set.toList
                    |> List.map (\visibility -> ( visibility, visibility ))
                    |> Dict.fromList
      , filterTypeAndDefaultValue =
            DataList.MultiselectOption Set.empty
      , onFilter =
            \optionValue imageRecord ->
                imageRecord.visibility == optionValue
      }
    , { id = "isOwned"
      , label = "Belongs to"
      , chipPrefix = "Belongs to "
      , filterOptions =
            \_ ->
                Dict.fromList
                    [ ( "yes", "this " ++ localization.unitOfTenancy )
                    , ( "no", "other " ++ Helpers.String.pluralize localization.unitOfTenancy )
                    ]
      , filterTypeAndDefaultValue =
            DataList.UniselectOption DataList.UniselectNoChoice
      , onFilter =
            \optionValue imageRecord ->
                let
                    imageIsOwned =
                        if imageRecord.owned then
                            "yes"

                        else
                            "no"
                in
                imageIsOwned == optionValue
      }
    , { id = "tags"
      , label = Helpers.String.toTitleCase localization.staticRepresentationOfBlockDeviceContents ++ " tags"
      , chipPrefix = Helpers.String.toTitleCase localization.staticRepresentationOfBlockDeviceContents ++ " tag is "
      , filterOptions =
            \images ->
                List.concatMap (\i -> i.image.tags) images
                    |> Set.fromList
                    |> Set.toList
                    |> List.map (\tag -> ( tag, tag ))
                    |> Dict.fromList
      , filterTypeAndDefaultValue =
            DataList.MultiselectOption Set.empty
      , onFilter =
            \optionValue imageRecord ->
                List.member optionValue imageRecord.image.tags
      }
    , { id = "type"
      , label = Helpers.String.toTitleCase localization.staticRepresentationOfBlockDeviceContents ++ " type"
      , chipPrefix = Helpers.String.toTitleCase localization.staticRepresentationOfBlockDeviceContents ++ " type is "
      , filterOptions =
            \images ->
                List.map (\i -> Maybe.withDefault localization.staticRepresentationOfBlockDeviceContents i.image.imageType) images
                    |> List.map (\t -> ( t, t ))
                    |> Dict.fromList
      , filterTypeAndDefaultValue =
            DataList.MultiselectOption Set.empty
      , onFilter =
            \optionValue imageRecord ->
                let
                    imageType =
                        case imageRecord.image.imageType of
                            Just imageTypeName ->
                                imageTypeName

                            Nothing ->
                                localization.staticRepresentationOfBlockDeviceContents
                in
                imageType == optionValue
      }
    ]


searchByNameFilter : DataList.SearchFilter { record | image : OSTypes.Image }
searchByNameFilter =
    { label = "Search by name:"
    , placeholder = Just "try \"Ubuntu\""
    , textToSearch = \imageRecord -> imageRecord.image.name
    }


popoverMsgMapper : PopoverId -> Msg
popoverMsgMapper popoverId =
    SharedMsg <| SharedMsg.TogglePopover popoverId
