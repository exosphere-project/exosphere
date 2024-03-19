module Page.OnrampCLI exposing (Format, Model, headerView, init, pageTitle, toQueryStringParameters, toUrl, view)

import Element
import FeatherIcons
import Helpers.Credentials as Credentials
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Helpers.Url exposing (textDataUrl)
import Style.Widgets.CopyableText exposing (copyableScript)
import Style.Widgets.Icon as Icon
import Style.Widgets.Link as Link
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.HelperTypes as HelperTypes
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Url.Builder as UB
import View.Helpers as VH
import View.Types exposing (Context)


type Format
    = OpenRcSh
    | OpenRcPs1
    | CloudsYaml


formatToString : Format -> String
formatToString format =
    case format of
        OpenRcPs1 ->
            "openrc.ps1"

        OpenRcSh ->
            "openrc.sh"

        CloudsYaml ->
            "clouds.yaml"


formatFromString : String -> Maybe Format
formatFromString s =
    case s of
        "openrc.sh" ->
            Just OpenRcSh

        "openrc.ps1" ->
            Just OpenRcPs1

        "clouds.yaml" ->
            Just CloudsYaml

        _ ->
            Nothing


type alias Model =
    { maybeProjectIdentifier : Maybe HelperTypes.ProjectIdentifier
    , format : Format
    }


init : Maybe HelperTypes.ProjectIdentifier -> Maybe String -> Model
init maybeProjectIdentifier maybeFormatString =
    { maybeProjectIdentifier = maybeProjectIdentifier
    , format =
        maybeFormatString
            |> Maybe.andThen formatFromString
            |> Maybe.withDefault CloudsYaml
    }


toQueryStringParameters : Maybe HelperTypes.ProjectIdentifier -> Maybe Format -> List UB.QueryParameter
toQueryStringParameters maybeProjectIdentifier maybeFormat =
    List.filterMap identity
        [ maybeProjectIdentifier |> Maybe.map .projectUuid |> Maybe.map (UB.string {- @nonlocalized -} "project")
        , maybeProjectIdentifier |> Maybe.andThen .regionId |> Maybe.map (UB.string {- @nonlocalized -} "region")
        , maybeFormat |> Maybe.map (\format -> UB.string "format" (formatToString format))
        ]


toUrl : Model -> String
toUrl model =
    let
        queryString =
            toQueryStringParameters model.maybeProjectIdentifier (Just model.format)
                |> UB.toQuery
    in
    "/onramp" ++ queryString


view : Context -> SharedModel -> Model -> Element.Element msg
view context sharedModel model =
    let
        maybeProject : Maybe Project
        maybeProject =
            model.maybeProjectIdentifier
                |> Maybe.andThen (GetterSetters.projectLookup sharedModel)

        projectLinks =
            Element.column
                [ Element.alignTop
                , Element.spacing spacer.px8
                , Element.width <| Element.fillPortion 25
                ]
                (Text.subheading context.palette
                    []
                    Element.none
                    (context.localization.unitOfTenancy |> Helpers.String.pluralize |> Helpers.String.toTitleCase)
                    :: List.map
                        (\project ->
                            Element.el [ Element.paddingXY 0 4 ] <|
                                if maybeProject == Just project then
                                    Element.text (projectFormattedName project)

                                else
                                    Link.link context.palette
                                        (toUrl { model | maybeProjectIdentifier = Just (GetterSetters.projectIdentifier project) })
                                        (projectFormattedName project)
                        )
                        sharedModel.projects
                )
    in
    Element.row
        [ Element.spacing spacer.px12

        -- , Element.explain Debug.todo
        ]
        [ projectLinks
        , Element.column
            [ Element.alignTop
            , Element.spacing spacer.px12
            , Element.width (Element.fillPortion 75)
            ]
            (List.concat
                [ [ Element.row [ Element.spacing spacer.px24 ] <|
                        List.map
                            (\format ->
                                Element.el [ Element.padding spacer.px4 ] <|
                                    Link.link context.palette
                                        (toUrl { model | format = format })
                                        (formatToString format)
                            )
                            [ CloudsYaml, OpenRcSh, OpenRcPs1 ]
                  ]
                , let
                    maybeScriptAndFilename =
                        case ( model.format, maybeProject ) of
                            ( CloudsYaml, _ ) ->
                                Just <| ( Credentials.getCloudsYaml sharedModel.projects, "clouds.yaml" )

                            ( OpenRcSh, Just project ) ->
                                Just <| ( Credentials.getOpenRcSh project, "openrc_" ++ Credentials.projectCloudName project ++ ".sh" )

                            ( OpenRcPs1, Just project ) ->
                                Just <| ( Credentials.getOpenRcPs1 project, "openrc_" ++ Credentials.projectCloudName project ++ ".ps1" )

                            _ ->
                                Nothing
                  in
                  case maybeScriptAndFilename of
                    Just ( script, filename ) ->
                        [ Element.column []
                            [ Element.downloadAs [ Element.alignRight ]
                                { url = textDataUrl script
                                , label =
                                    Element.row [ Element.spacing spacer.px8 ]
                                        [ Icon.featherIcon [] FeatherIcons.download
                                        , Element.text ("Download " ++ filename)
                                        ]
                                , filename = filename
                                }
                            , copyableScript context.palette script
                            ]
                        ]

                    Nothing ->
                        []
                ]
            )
        ]


projectFormattedName : Project -> String
projectFormattedName project =
    case project.region of
        Just region ->
            project.auth.project.name ++ " (" ++ region.id ++ ")"

        Nothing ->
            project.auth.project.name


pageTitle : SharedModel -> Model -> String
pageTitle sharedModel model =
    case
        model.maybeProjectIdentifier
            |> Maybe.andThen (GetterSetters.projectLookup sharedModel)
    of
        Just project ->
            "Onramp to " ++ projectFormattedName project

        Nothing ->
            "Onramp"


headerView : Context -> SharedModel -> Model -> Element.Element msg
headerView context sharedModel model =
    Text.heading context.palette
        VH.headerHeadingAttributes
        Element.none
        (pageTitle sharedModel model)
