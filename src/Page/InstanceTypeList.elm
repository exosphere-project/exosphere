module Page.InstanceTypeList exposing (Msg(..), view)

import Color
import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Html.Attributes as HtmlA
import OpenStack.Types as OSTypes
import Route
import Style.Helpers as SH
import Time
import Types.HelperTypes as HelperTypes
import Types.Project exposing (Project)
import View.Helpers as VH
import View.Types
import Widget


type Msg
    = NoOp


type HaveUsableFlavors
    = NoUsableFlavors
    | YesUsableFlavors FlavorRestriction


type FlavorRestriction
    = NoFlavorRestriction
    | FlavorRestrictionValidFlavors


view : View.Types.Context -> Project -> List HelperTypes.InstanceType -> Element.Element Msg
view context project instanceTypes =
    let
        renderVersion : HelperTypes.InstanceTypeVersion -> Element.Element Msg
        renderVersion instanceTypeVersion =
            let
                maybeImage =
                    getImageforInstanceTypeVersion (RDPP.withDefault [] project.images) instanceTypeVersion.imageFilters

                haveUsableFlavors =
                    case instanceTypeVersion.restrictFlavorIds of
                        Nothing ->
                            YesUsableFlavors NoFlavorRestriction

                        Just validFlavorIds ->
                            let
                                validFlavors =
                                    validFlavorIds
                                        |> List.filterMap (GetterSetters.flavorLookup project)
                            in
                            if List.isEmpty validFlavors then
                                NoUsableFlavors

                            else
                                YesUsableFlavors FlavorRestrictionValidFlavors
            in
            case ( maybeImage, haveUsableFlavors ) of
                ( Nothing, _ ) ->
                    Element.none

                ( _, NoUsableFlavors ) ->
                    Element.none

                ( Just image, _ ) ->
                    let
                        chooseRoute =
                            Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                Route.ServerCreate
                                    image.uuid
                                    image.name
                                    instanceTypeVersion.restrictFlavorIds
                                    (GetterSetters.getUserAppProxyFromContext project context
                                        |> Maybe.map (\_ -> True)
                                    )

                        buttonStyleProto =
                            if instanceTypeVersion.isPrimary then
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
                                    instanceTypeVersion.friendlyName
                                , onPress =
                                    Just NoOp
                                }
                        }

        renderInstanceType : HelperTypes.InstanceType -> Element.Element Msg
        renderInstanceType instanceType =
            Element.el
                [ Element.width <| Element.px 350, Element.alignTop ]
            <|
                Widget.column
                    (SH.materialStyle context.palette).cardColumn
                    [ Element.column
                        [ Element.centerX
                        , Element.paddingXY 10 15
                        , Element.spacing 15
                        ]
                      <|
                        [ Element.el
                            -- Yes, a hard-coded color when we've otherwise removed them from the app. These logos need a light background to look right.
                            [ Background.color <| SH.toElementColor <| Color.rgb255 255 255 255
                            , Element.centerX
                            , Element.paddingXY 15 0
                            , Border.rounded 10
                            , Element.height <| Element.px 100
                            ]
                            (Element.image
                                [ Element.width (Element.px 80)
                                , Element.height (Element.px 80)
                                , Element.centerX
                                , Element.centerY
                                , Element.htmlAttribute <| HtmlA.style "color" "blue"
                                , Font.color <| SH.toElementColor context.palette.primary
                                ]
                                { src = instanceType.logo
                                , description = instanceType.friendlyName ++ " logo"
                                }
                            )
                        , Element.el
                            [ Element.centerX
                            , Font.semiBold
                            ]
                          <|
                            Element.text instanceType.friendlyName
                        , Element.paragraph [ Element.width Element.fill ] <|
                            VH.renderMarkdown context instanceType.description
                        ]
                    , Element.column
                        [ Element.padding 10
                        , Element.spacing 10
                        , Element.centerX
                        ]
                        (instanceType.versions
                            |> List.map renderVersion
                        )
                    ]
    in
    Element.wrappedRow
        [ Element.width Element.fill, Element.spacing 24, Element.alignTop ]
        (List.map renderInstanceType instanceTypes)


getImageforInstanceTypeVersion : List OSTypes.Image -> HelperTypes.InstanceTypeImageFilters -> Maybe OSTypes.Image
getImageforInstanceTypeVersion images_ imageFilters =
    let
        applyNameFilter : OSTypes.Image -> Bool
        applyNameFilter image =
            case imageFilters.nameFilter of
                Just name ->
                    image.name == name

                Nothing ->
                    True

        applyUuidFilter : OSTypes.Image -> Bool
        applyUuidFilter image =
            case imageFilters.uuidFilter of
                Just uuid ->
                    let
                        lowerCaseNoHyphens : String -> String
                        lowerCaseNoHyphens str =
                            str
                                |> String.replace "-" ""
                                |> String.toLower
                    in
                    lowerCaseNoHyphens image.uuid == lowerCaseNoHyphens uuid

                Nothing ->
                    True

        applyVisibilityFilter : OSTypes.Image -> Bool
        applyVisibilityFilter image =
            case imageFilters.visibilityFilter of
                Just visibility ->
                    image.visibility == visibility

                Nothing ->
                    True

        applyOsDistroFilter : OSTypes.Image -> Bool
        applyOsDistroFilter image =
            case imageFilters.osDistroFilter of
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
            case imageFilters.osVersionFilter of
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
            case imageFilters.metadataFilter of
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
        |> List.filter applyNameFilter
        |> List.filter applyUuidFilter
        |> List.filter applyVisibilityFilter
        |> List.filter applyOsDistroFilter
        |> List.filter applyOsVersionFilter
        |> List.filter applyMetadataFilter
        |> List.sortBy (.createdAt >> Time.posixToMillis)
        |> List.reverse
        |> List.head
