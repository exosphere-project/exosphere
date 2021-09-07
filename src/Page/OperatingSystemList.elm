module Page.OperatingSystemList exposing (Msg(..), view)

import Dict
import Element
import Element.Font as Font
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


view : View.Types.Context -> Project -> List HelperTypes.OperatingSystemChoice -> Element.Element Msg
view context project opSysChoices =
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


getImageforOpSysChoiceVersion : List OSTypes.Image -> HelperTypes.OperatingSystemImageFilters -> Maybe OSTypes.Image
getImageforOpSysChoiceVersion images_ filters =
    let
        applyNameFilter : OSTypes.Image -> Bool
        applyNameFilter image =
            case filters.nameFilter of
                Just name ->
                    image.name == name

                Nothing ->
                    True

        applyUuidFilter : OSTypes.Image -> Bool
        applyUuidFilter image =
            case filters.uuidFilter of
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
            case filters.visibilityFilter of
                Just visibility ->
                    image.visibility == visibility

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
        |> List.filter applyNameFilter
        |> List.filter applyUuidFilter
        |> List.filter applyVisibilityFilter
        |> List.filter applyOsDistroFilter
        |> List.filter applyOsVersionFilter
        |> List.filter applyMetadataFilter
        |> List.sortBy (.createdAt >> Time.posixToMillis)
        |> List.reverse
        |> List.head
