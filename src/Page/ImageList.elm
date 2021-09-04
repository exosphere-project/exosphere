module Page.ImageList exposing (Model, Msg, init, update, view)

import Dict
import Element
import Element.Font as Font
import Element.Input as Input
import Filesize
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Html.Attributes as HtmlA
import List.Extra
import OpenStack.Types as OSTypes
import Route
import Set
import Set.Extra
import Style.Helpers as SH
import Style.Widgets.Card as ExoCard
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton exposing (chip)
import Time
import Types.HelperTypes as HelperTypes
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types exposing (ImageTag)
import Widget


type alias Model =
    { searchText : String
    , tags : Set.Set String
    , onlyOwnImages : Bool
    , expandImageDetails : Set.Set OSTypes.ImageUuid
    , visibilityFilter : ImageListVisibilityFilter
    , tab : Maybe Int
    }


type alias ImageListVisibilityFilter =
    { public : Bool
    , community : Bool
    , shared : Bool
    , private : Bool
    }


type Msg
    = SetTab Int
    | GotSearchText String
    | GotTagSelection String Bool
    | GotOnlyOwnImages Bool
    | GotExpandImage OSTypes.ImageUuid Bool
    | GotVisibilityFilter ImageListVisibilityFilter
    | GotClearFilters
    | NoOp


init : Model
init =
    Model "" Set.empty False Set.empty (ImageListVisibilityFilter True True True True) (Just 0)


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        SetTab tab ->
            ( { model | tab = Just tab }, Cmd.none, SharedMsg.NoOp )

        GotSearchText searchText ->
            ( { model | searchText = searchText }, Cmd.none, SharedMsg.NoOp )

        GotTagSelection tag selected ->
            let
                action =
                    if selected then
                        Set.insert

                    else
                        Set.remove
            in
            ( { model | tags = action tag model.tags }, Cmd.none, SharedMsg.NoOp )

        GotOnlyOwnImages onlyOwn ->
            ( { model | onlyOwnImages = onlyOwn }, Cmd.none, SharedMsg.NoOp )

        GotExpandImage imageUuid expanded ->
            ( { model
                | expandImageDetails =
                    let
                        func =
                            if expanded then
                                Set.insert

                            else
                                Set.remove
                    in
                    func imageUuid model.expandImageDetails
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotVisibilityFilter filter ->
            ( { model | visibilityFilter = filter }, Cmd.none, SharedMsg.NoOp )

        GotClearFilters ->
            ( init, Cmd.none, SharedMsg.NoOp )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    if List.isEmpty project.images then
        Element.row [ Element.spacing 15 ]
            [ Widget.circularProgressIndicator (SH.materialStyle context.palette).progressIndicator Nothing
            , Element.text <|
                String.join " "
                    [ context.localization.staticRepresentationOfBlockDeviceContents
                        |> Helpers.String.toTitleCase
                        |> Helpers.String.pluralize
                    , "loading..."
                    ]
            ]

    else
        -- TODO make a tabbed interface if we have choices
        let
            operatingSystemChoices =
                GetterSetters.cloudConfigLookup context.cloudSpecificConfigs project
                    |> Maybe.map .operatingSystemChoices
                    |> Maybe.withDefault []
        in
        if List.isEmpty operatingSystemChoices then
            Element.column
                (VH.exoColumnAttributes
                    ++ [ Element.width Element.fill ]
                )
                [ Element.el (VH.heading2 context.palette)
                    (Element.text <|
                        String.join " "
                            [ "Choose"
                            , Helpers.String.indefiniteArticle context.localization.staticRepresentationOfBlockDeviceContents
                            , context.localization.staticRepresentationOfBlockDeviceContents
                            ]
                    )
                , images context project model
                ]

        else
            Element.column
                (VH.exoColumnAttributes
                    ++ [ Element.width Element.fill ]
                )
                [ Element.el (VH.heading2 context.palette) <|
                    Element.text <|
                        Helpers.String.toTitleCase <|
                            String.join " "
                                [ "Choose"
                                , context.localization.virtualComputer
                                    |> Helpers.String.indefiniteArticle
                                , context.localization.virtualComputer
                                , "Source"
                                ]
                , Widget.tab (SH.materialStyle context.palette).tab
                    { tabs =
                        Widget.Select
                            model.tab
                            [ { text = "By Operating System", icon = Element.none }
                            , { text = "By Image", icon = Element.none }
                            ]
                            (\i -> Just <| SetTab i)
                    , content =
                        \maybeTabInt ->
                            case maybeTabInt of
                                Just 0 ->
                                    operatingSystems context project operatingSystemChoices model

                                Just 1 ->
                                    images context project model

                                _ ->
                                    Element.none
                    }
                ]



-- Helper functions for Images tab


projectOwnsImage : Project -> OSTypes.Image -> Bool
projectOwnsImage project image =
    image.projectUuid == project.auth.project.uuid


filterByOwner : Bool -> Project -> List OSTypes.Image -> List OSTypes.Image
filterByOwner onlyOwnImages project someImages =
    if not onlyOwnImages then
        someImages

    else
        List.filter (projectOwnsImage project) someImages


filterByTags : Set.Set String -> List OSTypes.Image -> List OSTypes.Image
filterByTags tagsToFilterBy someImages =
    if tagsToFilterBy == Set.empty then
        someImages

    else
        List.filter
            (\i ->
                let
                    imageTags =
                        Set.fromList i.tags
                in
                Set.Extra.subset tagsToFilterBy imageTags
            )
            someImages


filterBySearchText : String -> List OSTypes.Image -> List OSTypes.Image
filterBySearchText searchText someImages =
    if searchText == "" then
        someImages

    else
        List.filter (\i -> String.contains (String.toUpper searchText) (String.toUpper i.name)) someImages


filterByVisibility : ImageListVisibilityFilter -> List OSTypes.Image -> List OSTypes.Image
filterByVisibility filter someImages =
    let
        include i =
            List.any identity
                [ i.visibility == OSTypes.ImagePublic && filter.public
                , i.visibility == OSTypes.ImagePrivate && filter.private
                , i.visibility == OSTypes.ImageCommunity && filter.community
                , i.visibility == OSTypes.ImageShared && filter.shared
                ]
    in
    List.filter include someImages


isImageFeaturedByDeployer : Maybe String -> OSTypes.Image -> Bool
isImageFeaturedByDeployer maybeFeaturedImageNamePrefix image =
    case maybeFeaturedImageNamePrefix of
        Nothing ->
            False

        Just featuredImageNamePrefix ->
            String.startsWith featuredImageNamePrefix image.name && image.visibility == OSTypes.ImagePublic


filterImages : Model -> Project -> List OSTypes.Image -> List OSTypes.Image
filterImages model project someImages =
    someImages
        |> filterByOwner model.onlyOwnImages project
        |> filterByTags model.tags
        |> filterBySearchText model.searchText
        |> filterByVisibility model.visibilityFilter



-- Helper functions for Operating Systems tab


getImageforOpSysChoiceVersion : List OSTypes.Image -> HelperTypes.OperatingSystemImageFilters -> Maybe OSTypes.Image
getImageforOpSysChoiceVersion images_ filters =
    let
        applyUuidFilter : OSTypes.Image -> Bool
        applyUuidFilter image =
            case filters.uuidFilter of
                Just uuid ->
                    image.uuid == uuid

                Nothing ->
                    True

        applyVisibilityFilter : OSTypes.Image -> Bool
        applyVisibilityFilter image =
            case filters.visibilityFilter of
                Just visibility ->
                    image.visibility == visibility

                Nothing ->
                    True

        applyNameFilter : OSTypes.Image -> Bool
        applyNameFilter image =
            case filters.nameFilter of
                Just name ->
                    image.name == name

                Nothing ->
                    True

        applyOsDistroFilter : OSTypes.Image -> Bool
        applyOsDistroFilter image =
            case filters.osDistroFilter of
                Just filterOsDistro ->
                    case image.osDistro of
                        Just imageOsDistro ->
                            imageOsDistro == filterOsDistro

                        Nothing ->
                            False

                Nothing ->
                    True

        applyOsVersionFilter : OSTypes.Image -> Bool
        applyOsVersionFilter image =
            case filters.osVersionFilter of
                Just filterOsVersion ->
                    case image.osVersion of
                        Just imageOsVersion ->
                            imageOsVersion == filterOsVersion

                        Nothing ->
                            False

                Nothing ->
                    True

        applyMetadataFilter : OSTypes.Image -> Bool
        applyMetadataFilter image =
            case filters.metadataFilter of
                Just filterMetadata ->
                    case Dict.get filterMetadata.filterKey image.additionalProperties of
                        Just val ->
                            filterMetadata.filterValue == val

                        Nothing ->
                            False

                Nothing ->
                    True
    in
    images_
        |> List.filter applyUuidFilter
        |> List.filter applyVisibilityFilter
        |> List.filter applyNameFilter
        |> List.filter applyOsDistroFilter
        |> List.filter applyOsVersionFilter
        |> List.filter applyMetadataFilter
        |> List.sortBy (.createdAt >> Time.posixToMillis)
        |> List.reverse
        |> List.head



-- Tabs


operatingSystems : View.Types.Context -> Project -> List HelperTypes.OperatingSystemChoice -> Model -> Element.Element Msg
operatingSystems context project opSysChoices model =
    let
        renderOpSysChoiceVersion : HelperTypes.OperatingSystemChoiceVersion -> Element.Element Msg
        renderOpSysChoiceVersion opSysChoiceVersion =
            case getImageforOpSysChoiceVersion project.images opSysChoiceVersion.filters of
                Nothing ->
                    Element.none

                Just image ->
                    let
                        chooseRoute =
                            Route.ProjectRoute project.auth.project.uuid <|
                                Route.ServerCreate
                                    image.uuid
                                    image.name
                                    (VH.userAppProxyLookup context project
                                        |> Maybe.map (\_ -> True)
                                    )

                        buttonStyleProto =
                            if opSysChoiceVersion.isPrimary then
                                (SH.materialStyle context.palette).primaryButton

                            else
                                (SH.materialStyle context.palette).button

                        buttonStyle =
                            { buttonStyleProto
                                | container =
                                    buttonStyleProto.container
                                        ++ [ Element.width Element.fill
                                           , Element.centerX
                                           ]
                                , labelRow =
                                    buttonStyleProto.labelRow
                                        ++ [ Element.centerX ]
                            }
                    in
                    Element.link [ Element.centerX, Element.width Element.fill ]
                        { url = Route.toUrl context.urlPathPrefix chooseRoute
                        , label =
                            Widget.textButton
                                buttonStyle
                                { text =
                                    opSysChoiceVersion.friendlyName
                                , onPress =
                                    Just NoOp
                                }
                        }

        renderOpSysChoice : HelperTypes.OperatingSystemChoice -> Element.Element Msg
        renderOpSysChoice opSysChoice =
            Element.el
                [ Element.width <| Element.px 350 ]
            <|
                Widget.column
                    (SH.materialStyle context.palette).cardColumn
                    [ Element.column
                        [ Element.centerX
                        , Element.paddingXY 10 15
                        , Element.spacing 15
                        ]
                      <|
                        [ Element.image
                            [ Element.width (Element.px 80)
                            , Element.height (Element.px 80)
                            , Element.centerX
                            , Element.htmlAttribute <| HtmlA.style "color" "blue"
                            , Font.color <| SH.toElementColor context.palette.primary
                            ]
                            -- TODO get URL path prefix from view context after rebase, rather than hard-coding it?
                            { src = opSysChoice.logo
                            , description = opSysChoice.friendlyName ++ " logo"
                            }
                        , Element.el
                            [ Element.centerX
                            , Font.bold
                            ]
                          <|
                            Element.text opSysChoice.friendlyName
                        , Element.paragraph [ Element.width Element.fill ] <|
                            VH.renderMarkdown context opSysChoice.description
                        ]
                    , Element.column
                        [ Element.padding 10
                        , Element.spacing 10
                        , Element.centerX
                        ]
                        (opSysChoice.versions
                            |> List.map renderOpSysChoiceVersion
                        )
                    ]
    in
    Element.column VH.contentContainer
        [ Element.wrappedRow [ Element.width Element.fill, Element.spacing 40 ]
            (List.map renderOpSysChoice opSysChoices)
        ]


images : View.Types.Context -> Project -> Model -> Element.Element Msg
images context project model =
    let
        generateAllTags : List OSTypes.Image -> List ImageTag
        generateAllTags someImages =
            List.map (\i -> i.tags) someImages
                |> List.concat
                |> List.sort
                |> List.Extra.gatherEquals
                |> List.map (\t -> { label = Tuple.first t, frequency = 1 + List.length (Tuple.second t) })
                |> List.sortBy .frequency
                |> List.reverse

        filteredImages =
            project.images |> filterImages model project

        tagsAfterFilteringImages =
            generateAllTags filteredImages

        noMatchWarning =
            (model.tags /= Set.empty) && (List.length filteredImages == 0)

        featuredImageNamePrefix =
            VH.featuredImageNamePrefixLookup context project

        ( featuredImages, nonFeaturedImages_ ) =
            List.partition (isImageFeaturedByDeployer featuredImageNamePrefix) filteredImages

        ( ownImages, otherImages ) =
            List.partition (\i -> projectOwnsImage project i) nonFeaturedImages_

        combinedImages =
            List.concat [ featuredImages, ownImages, otherImages ]

        tagView : ImageTag -> Element.Element Msg
        tagView tag =
            let
                iconFunction checked =
                    if checked then
                        Element.none

                    else
                        Icon.plusCircle (SH.toElementColor context.palette.on.background) 12

                tagChecked =
                    Set.member tag.label model.tags

                checkboxLabel =
                    tag.label ++ " (" ++ String.fromInt tag.frequency ++ ")"
            in
            if tagChecked then
                Element.none

            else
                Input.checkbox [ Element.paddingXY 10 5 ]
                    { checked = tagChecked
                    , onChange = \_ -> GotTagSelection tag.label True
                    , icon = iconFunction
                    , label = Input.labelRight [] (Element.text checkboxLabel)
                    }

        tagChipView : ImageTag -> Element.Element Msg
        tagChipView tag =
            let
                tagChecked =
                    Set.member tag.label model.tags

                chipLabel =
                    Element.text tag.label

                unselectTag =
                    GotTagSelection tag.label False
            in
            if tagChecked then
                chip context.palette (Just unselectTag) chipLabel

            else
                Element.none

        tagsView =
            Element.column [ Element.spacing 10 ]
                [ Element.text "Filtering on these tags:"
                , Element.wrappedRow
                    [ Element.height Element.shrink
                    , Element.width Element.shrink
                    ]
                    (List.map tagChipView tagsAfterFilteringImages)
                , Element.text <|
                    String.join " "
                        [ "Select tags to filter"
                        , context.localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.pluralize
                        , "on:"
                        ]
                , Element.wrappedRow []
                    (List.map tagView tagsAfterFilteringImages)
                ]

        imagesColumnView =
            Widget.column
                ((SH.materialStyle context.palette).column
                    |> (\x ->
                            { x
                                | containerColumn =
                                    (SH.materialStyle context.palette).column.containerColumn
                                        ++ [ Element.width Element.fill
                                           , Element.padding 0
                                           ]
                                , element =
                                    (SH.materialStyle context.palette).column.element
                                        ++ [ Element.width Element.fill
                                           ]
                            }
                       )
                )

        visibilityFilters =
            Element.row
                [ Element.spacing 10 ]
                [ Element.text <|
                    String.join " "
                        [ "Filter on"
                        , context.localization.staticRepresentationOfBlockDeviceContents
                        , "visibility:"
                        ]

                -- TODO duplication of logic in these checkboxes, factor out what is common
                , Input.checkbox []
                    { checked = model.visibilityFilter.public
                    , onChange =
                        \new ->
                            let
                                oldVisibilityFilter =
                                    model.visibilityFilter

                                newVisibilityFilter =
                                    { oldVisibilityFilter | public = new }
                            in
                            GotVisibilityFilter newVisibilityFilter
                    , icon = Input.defaultCheckbox
                    , label =
                        Input.labelRight [] <|
                            Element.text "Public"
                    }
                , Input.checkbox []
                    { checked = model.visibilityFilter.community
                    , onChange =
                        \new ->
                            let
                                oldVisibilityFilter =
                                    model.visibilityFilter

                                newVisibilityFilter =
                                    { oldVisibilityFilter | community = new }
                            in
                            GotVisibilityFilter newVisibilityFilter
                    , icon = Input.defaultCheckbox
                    , label =
                        Input.labelRight [] <|
                            Element.text "Community"
                    }
                , Input.checkbox []
                    { checked = model.visibilityFilter.shared
                    , onChange =
                        \new ->
                            let
                                oldVisibilityFilter =
                                    model.visibilityFilter

                                newVisibilityFilter =
                                    { oldVisibilityFilter | shared = new }
                            in
                            GotVisibilityFilter newVisibilityFilter
                    , icon = Input.defaultCheckbox
                    , label =
                        Input.labelRight [] <|
                            Element.text "Shared"
                    }
                , Input.checkbox []
                    { checked = model.visibilityFilter.private
                    , onChange =
                        \new ->
                            let
                                oldVisibilityFilter =
                                    model.visibilityFilter

                                newVisibilityFilter =
                                    { oldVisibilityFilter | private = new }
                            in
                            GotVisibilityFilter newVisibilityFilter
                    , icon = Input.defaultCheckbox
                    , label =
                        Input.labelRight [] <|
                            Element.text "Private"
                    }
                ]
    in
    Element.column VH.contentContainer
        [ Input.text (VH.inputItemAttributes context.palette.background)
            { text = model.searchText
            , placeholder = Just (Input.placeholder [] (Element.text "try \"Ubuntu\""))
            , onChange = GotSearchText
            , label =
                Input.labelAbove []
                    (Element.text <|
                        String.join " "
                            [ "Filter on"
                            , context.localization.staticRepresentationOfBlockDeviceContents
                            , "name:"
                            ]
                    )
            }
        , visibilityFilters
        , tagsView
        , Input.checkbox []
            { checked = model.onlyOwnImages
            , onChange = GotOnlyOwnImages
            , icon = Input.defaultCheckbox
            , label =
                Input.labelRight [] <|
                    Element.text <|
                        String.join
                            " "
                            [ "Show only"
                            , context.localization.staticRepresentationOfBlockDeviceContents
                                |> Helpers.String.pluralize
                            , "owned by this"
                            , context.localization.unitOfTenancy
                            ]
            }
        , Widget.textButton
            (SH.materialStyle context.palette).button
            { text = "Clear filters (show all)"
            , onPress = Just GotClearFilters
            }
        , if noMatchWarning then
            Element.text "No matches found. Broaden your search terms, or clear the search filter."

          else
            Element.none
        , List.map (renderImage context project model) combinedImages
            |> imagesColumnView
        ]


renderImage : View.Types.Context -> Project -> Model -> OSTypes.Image -> Element.Element Msg
renderImage context project model image =
    let
        imageDetailsExpanded =
            Set.member image.uuid model.expandImageDetails

        size =
            case image.size of
                Just s ->
                    Filesize.format s

                Nothing ->
                    "size unknown"

        chooseRoute =
            Route.ProjectRoute project.auth.project.uuid <|
                Route.ServerCreate
                    image.uuid
                    image.name
                    (VH.userAppProxyLookup context project
                        |> Maybe.map (\_ -> True)
                    )

        tagChip tag =
            Element.el [ Element.paddingXY 5 0 ]
                (Widget.button (SH.materialStyle context.palette).chipButton
                    { text = tag
                    , icon = Element.none
                    , onPress =
                        Nothing
                    }
                )

        chooseButton =
            Element.link []
                { url = Route.toUrl context.urlPathPrefix chooseRoute
                , label =
                    Widget.textButton
                        (SH.materialStyle context.palette).primaryButton
                        { text = "Choose"
                        , onPress =
                            case image.status of
                                OSTypes.ImageActive ->
                                    Just NoOp

                                _ ->
                                    Nothing
                        }
                }

        featuredImageNamePrefix =
            VH.featuredImageNamePrefixLookup context project

        featuredBadge =
            if isImageFeaturedByDeployer featuredImageNamePrefix image then
                ExoCard.badge "featured"

            else
                Element.none

        ownerBadge =
            if projectOwnsImage project image then
                ExoCard.badge <|
                    String.join " "
                        [ "belongs to this"
                        , context.localization.unitOfTenancy
                        ]

            else
                Element.none

        title =
            Element.row
                [ Element.width Element.fill
                ]
                [ Element.el
                    [ Font.bold
                    , Element.padding 5
                    ]
                    (Element.text image.name)
                , featuredBadge
                , ownerBadge
                ]

        subtitle =
            Element.row
                []
                [ Element.el
                    [ Font.color <| SH.toElementColor <| context.palette.muted
                    , Element.padding 5
                    ]
                    (Element.text size)
                ]

        imageDetailsView =
            Element.column
                (VH.exoColumnAttributes
                    ++ [ Element.width Element.fill ]
                )
                [ Element.wrappedRow
                    [ Element.width Element.fill
                    ]
                    [ Element.el
                        [ Font.color <| SH.toElementColor <| context.palette.muted
                        , Element.padding 5
                        ]
                        (Element.text <| "Visibility: " ++ OSTypes.imageVisibilityToString image.visibility)
                    ]
                , Element.wrappedRow
                    [ Element.width Element.fill
                    ]
                    (Element.el
                        [ Font.color <| SH.toElementColor <| context.palette.muted
                        , Element.padding 5
                        ]
                        (Element.text "Tags:")
                        :: List.map
                            tagChip
                            image.tags
                    )
                , Element.el
                    [ Element.alignRight ]
                    chooseButton
                ]
    in
    ExoCard.expandoCard
        context.palette
        imageDetailsExpanded
        (\expanded -> GotExpandImage image.uuid expanded)
        title
        subtitle
        imageDetailsView
