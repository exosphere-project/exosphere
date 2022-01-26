module Page.SelectProjectRegions exposing (Model, Msg(..), init, update, view)

import Element
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Types as OSTypes
import Set
import Style.Helpers as SH
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { providerKeystoneUrl : OSTypes.KeystoneUrl
    , projectUuid : OSTypes.ProjectUuid
    , selectedRegions : Set.Set OSTypes.RegionId
    }


type Msg
    = GotBoxChecked OSTypes.RegionId Bool
    | GotSubmit


init : OSTypes.KeystoneUrl -> OSTypes.ProjectUuid -> Model
init keystoneUrl projectUuid =
    Model keystoneUrl projectUuid Set.empty


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        GotBoxChecked regionId checked ->
            let
                newSelectedRegions =
                    model.selectedRegions
                        |> (if checked then
                                Set.insert regionId

                            else
                                Set.remove regionId
                           )
            in
            ( { model | selectedRegions = newSelectedRegions }, Cmd.none, SharedMsg.NoOp )

        GotSubmit ->
            ( model
            , Cmd.none
            , SharedMsg.CreateProjectsFromRegionSelections model.providerKeystoneUrl
                model.projectUuid
                (Set.toList model.selectedRegions)
            )


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel model =
    let
        maybeScopedAuthToken =
            sharedModel.scopedAuthTokensWaitingRegionSelection
                |> List.filter (\t -> t.project.uuid == model.projectUuid)
                |> List.head
    in
    case ( GetterSetters.unscopedProviderLookup sharedModel model.providerKeystoneUrl, maybeScopedAuthToken ) of
        ( Just provider, Just scopedAuthToken ) ->
            let
                renderSuccessCase : List OSTypes.Region -> Element.Element Msg
                renderSuccessCase regions =
                    Element.column VH.formContainer <|
                        List.append
                            (List.map
                                (renderRegion model.selectedRegions)
                                regions
                            )
                            [ Widget.textButton
                                (SH.materialStyle context.palette).primaryButton
                                { text = "Choose"
                                , onPress =
                                    Just GotSubmit
                                }
                            ]
            in
            Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
                [ Element.el (VH.heading2 context.palette)
                    (Element.text <|
                        String.join " "
                            [ "Choose"
                            , context.localization.openstackSharingKeystoneWithAnother
                                |> Helpers.String.pluralize
                                |> Helpers.String.toTitleCase
                            , "for"
                            , context.localization.unitOfTenancy
                                |> Helpers.String.toTitleCase
                            , scopedAuthToken.project.name
                            ]
                    )
                , VH.renderWebData
                    context
                    provider.regionsAvailable
                    (context.localization.openstackSharingKeystoneWithAnother
                        |> Helpers.String.pluralize
                    )
                    renderSuccessCase
                ]

        _ ->
            Element.text "Provider or scoped auth token not found"


renderRegion : Set.Set OSTypes.RegionId -> OSTypes.Region -> Element.Element Msg
renderRegion selectedRegions region =
    let
        selected =
            Set.member region.id selectedRegions

        renderRegionLabel : Element.Element Msg
        renderRegionLabel =
            let
                labelStrNoDescription =
                    String.join " "
                        [ region.id
                        ]

                labelStrWithDescription =
                    String.join " "
                        [ region.id
                        , String.fromChar '—'
                        , region.description
                        ]

                labelStr =
                    if String.isEmpty region.description then
                        labelStrNoDescription

                    else
                        labelStrWithDescription
            in
            Element.paragraph [ Element.width Element.fill ] [ Element.text labelStr ]
    in
    Input.checkbox []
        { checked = selected
        , onChange = GotBoxChecked region.id
        , icon = Input.defaultCheckbox
        , label = Input.labelRight [ Element.width Element.fill ] renderRegionLabel
        }
