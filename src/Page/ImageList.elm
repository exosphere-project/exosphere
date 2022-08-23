module Page.ImageList exposing (Model, Msg(..), init, update, view)

import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
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
import Style.Widgets.Tag as Tag
import Style.Widgets.Text as Text
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
    | DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Bool -> Bool -> Model
init showDeleteButtons showHeading =
    { deletionsAttempted = Set.empty
    , showDeleteButtons = showDeleteButtons
    , showHeading = showHeading
    , dataListModel = DataList.init <| DataList.getDefaultFilterOptions filters
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
                        (FeatherIcons.package |> FeatherIcons.toHtml [] |> Element.html |> Element.el [])
                        (context.localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        )

                  else
                    Element.none
                , DataList.view
                    model.dataListModel
                    DataListMsg
                    context
                    []
                    (imageView model context project)
                    (imageRecords context project imagesInCustomOrder)
                    []
                    (Just
                        { filters = filters
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

                deletePopconfirmId =
                    Helpers.String.hyphenate
                        [ "ImageListDeletePopconfirm"
                        , project.auth.project.uuid
                        , imageRecord.id
                        ]

                deleteBtnWithPopconfirm =
                    deletePopconfirm context
                        (\deletePopconfirmId_ -> SharedMsg <| SharedMsg.TogglePopover deletePopconfirmId_)
                        deletePopconfirmId
                        { confirmationText =
                            "Are you sure you want to delete this "
                                ++ context.localization.staticRepresentationOfBlockDeviceContents
                                ++ "?"
                        , onConfirm = Just <| GotDeleteConfirm imageRecord.id
                        , onCancel = Just NoOp
                        }
                        ST.PositionBottomRight
                        deleteBtn

                deletionAttempted =
                    Set.member imageRecord.id model.deletionsAttempted

                deletionPending =
                    imageRecord.image.status == OSTypes.ImagePendingDelete
            in
            if model.showDeleteButtons && projectOwnsImage project imageRecord.image then
                if deletionAttempted || deletionPending then
                    -- FIXME: Constraint progressIndicator svg's height to 36 px also
                    Element.el [ Element.height <| Element.px 36 ]
                        (Widget.circularProgressIndicator (SH.materialStyle context.palette).progressIndicator Nothing)

                else
                    deleteBtnWithPopconfirm

            else
                Element.none

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

                serverCreationRoute =
                    Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                        Route.ServerCreate
                            imageRecord.image.uuid
                            imageRecord.image.name
                            Nothing
                            (VH.userAppProxyLookup context project
                                |> Maybe.map (\_ -> True)
                            )
            in
            case imageRecord.image.status of
                OSTypes.ImageActive ->
                    Element.link []
                        { url = Route.toUrl context.urlPathPrefix serverCreationRoute
                        , label = textBtn (Just NoOp)
                        }

                _ ->
                    Element.el [ Element.htmlAttribute <| HtmlA.title "Image is not active!" ] (textBtn Nothing)

        imageActions =
            Element.row [ Element.alignRight, Element.spacing 10 ]
                [ deleteImageBtn, createServerBtn ]

        size =
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

        featuredIcon =
            if imageRecord.featured then
                FeatherIcons.award
                    |> FeatherIcons.withSize 20
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el
                        [ Element.htmlAttribute <| HtmlA.title "Featured"
                        ]

            else
                Element.none

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
                        [ Element.spacing 6
                        , Element.paddingEach { left = 6, top = 0, right = 0, bottom = 0 }
                        ]
                        (List.map (Tag.tag context.palette) imageRecord.image.tags)

        imageAttributesView =
            let
                attributesAlwaysShown =
                    [ Element.text size
                    , Element.row []
                        [ Element.el [ Font.color (SH.toElementColor context.palette.neutral.text.default) ]
                            (Element.text <| String.toLower <| OSTypes.imageVisibilityToString imageRecord.image.visibility)
                        , Element.text <| " " ++ context.localization.staticRepresentationOfBlockDeviceContents
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
            Element.row [ Element.width Element.fill, Element.spacing 8 ] <|
                List.intersperse separator attributesShown
    in
    Element.column
        (listItemColumnAttribs context.palette)
        [ Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ Element.el
                [ Font.size 18
                , Font.color (SH.toElementColor context.palette.neutral.text.default)
                ]
                (Element.text imageRecord.image.name)
            , featuredIcon
            , imageActions
            ]
        , imageAttributesView
        ]


filters :
    List
        (DataList.Filter
            { record
                | image : Image
                , visibility : String
                , owned : Bool
            }
        )
filters =
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
                [ ( "yes", "this project" ), ( "no", "other projects" ) ]
                    |> Dict.fromList
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
      , label = "Image tags"
      , chipPrefix = "Image tag is "
      , filterOptions =
            \images ->
                List.map (\i -> i.image.tags) images
                    |> List.concat
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
    ]


searchByNameFilter : DataList.SearchFilter { record | image : OSTypes.Image }
searchByNameFilter =
    { label = "Search by name:"
    , placeholder = Just "try \"Ubuntu\""
    , textToSearch = \imageRecord -> imageRecord.image.name
    }
