module Page.SelectProjectRegions exposing (Model, Msg(..), canSubmit, init, update, views)

import Element
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Types as OSTypes
import Set
import Style.Widgets.Button as Button
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { providerKeystoneUrl : OSTypes.KeystoneUrl
    , projectUuid : OSTypes.ProjectUuid
    , selectedRegions : Set.Set OSTypes.RegionId
    }


type Msg
    = GotBoxChecked OSTypes.RegionId Bool
    | GotSubmit
    | GotCancel


init : OSTypes.KeystoneUrl -> OSTypes.ProjectUuid -> Model
init keystoneUrl projectUuid =
    Model keystoneUrl projectUuid Set.empty


{-| Whether there is anything to submit. Submitting no regions builds no project and drops the
login that is waiting on this answer, which is a thing to ask for on purpose with Cancel rather
than a thing to fall into by pressing Choose.
-}
canSubmit : Model -> Bool
canSubmit model =
    not (Set.isEmpty model.selectedRegions)


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

        GotCancel ->
            ( model
            , Cmd.none
            , SharedMsg.AbandonProjectRegionSelection model.providerKeystoneUrl model.projectUuid
            )


views : View.Types.Context -> SharedModel -> Model -> ( Maybe (Element.Element msg), Element.Element Msg )
views context sharedModel model =
    let
        maybeScopedAuthTokenWaitingSelection =
            sharedModel.heldLoginToken
                |> Maybe.andThen
                    (\token ->
                        if token.keystoneUrl == model.providerKeystoneUrl && token.authToken.project.uuid == model.projectUuid then
                            Just token

                        else
                            Nothing
                    )
    in
    case maybeScopedAuthTokenWaitingSelection of
        Just scopedAuthTokenWaitingSelection ->
            let
                scopedAuthToken =
                    scopedAuthTokenWaitingSelection.authToken

                renderHeader : Maybe (Element.Element msg)
                renderHeader =
                    Just <|
                        Text.heading context.palette
                            VH.headerHeadingAttributes
                            Element.none
                            (String.join " "
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

                renderSuccessCase : List OSTypes.Region -> Element.Element Msg
                renderSuccessCase regions =
                    Element.column VH.formContainer <|
                        List.append
                            (List.map
                                (renderRegion model.selectedRegions)
                                regions
                            )
                            [ Element.row
                                [ Element.width Element.fill
                                , Element.paddingXY 0 spacer.px16
                                , Element.spacing spacer.px12
                                ]
                                [ Button.default
                                    context.palette
                                    { text = "Cancel"
                                    , onPress = Just GotCancel
                                    }
                                , Element.el [ Element.alignRight ]
                                    (Button.primary
                                        context.palette
                                        { text = "Choose"
                                        , onPress =
                                            if canSubmit model then
                                                Just GotSubmit

                                            else
                                                Nothing
                                        }
                                    )
                                ]
                            ]

                maybeProvider =
                    GetterSetters.unscopedProviderLookup sharedModel model.providerKeystoneUrl
            in
            case maybeProvider of
                Just provider ->
                    ( renderHeader
                    , VH.renderRDPP
                        context
                        provider.regionsAvailable
                        (context.localization.openstackSharingKeystoneWithAnother
                            |> Helpers.String.pluralize
                        )
                        renderSuccessCase
                    )

                Nothing ->
                    ( renderHeader
                    , GetterSetters.getCatalogRegionIds scopedAuthToken.catalog
                        |> List.map (\regionId -> OSTypes.Region regionId "")
                        |> renderSuccessCase
                    )

        Nothing ->
            ( Nothing, Element.text "Scoped auth token not found" )


renderRegion : Set.Set OSTypes.RegionId -> OSTypes.Region -> Element.Element Msg
renderRegion selectedRegions region =
    let
        selected =
            Set.member region.id selectedRegions

        renderRegionLabel : Element.Element Msg
        renderRegionLabel =
            let
                labelStr =
                    if String.isEmpty region.description then
                        String.join " "
                            [ region.id
                            ]

                    else
                        String.join " "
                            [ region.id
                            , String.fromChar '—'
                            , region.description
                            ]
            in
            Element.paragraph [ Element.width Element.fill ] [ Element.text labelStr ]
    in
    Input.checkbox []
        { checked = selected
        , onChange = GotBoxChecked region.id
        , icon = Input.defaultCheckbox
        , label = Input.labelRight [ Element.width Element.fill ] renderRegionLabel
        }
