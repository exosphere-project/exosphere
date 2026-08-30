module Page.ObjectStorageContainerCreate exposing (Model, Msg(..), init, nameError, update, view)

import Element
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String exposing (toTitleCase)
import OpenStack.ObjectStorage as ObjectStorage
import Style.Widgets.Button as Button
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.Validation as Validation
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { name : String }


type Msg
    = GotName String
    | GotSubmit ObjectStorage.ContainerName


init : Model
init =
    { name = "" }


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotName name ->
            ( { model | name = name }, Cmd.none, SharedMsg.NoOp )

        GotSubmit name ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestCreateContainer name
            )


{-| Why the typed name cannot be used, or `Nothing` when it can. An empty field is not an error
yet, it is just an unfinished form, so it reads as `Nothing` and the Create button stays disabled.

Shape rules come from `ObjectStorage.containerNameError`, which is where the Swift naming limits
live. The duplicate-name check needs the cached container list, so it lives here.

-}
nameError : View.Types.Context -> List ObjectStorage.Container -> String -> Maybe String
nameError context containers name =
    if String.isEmpty name then
        Nothing

    else
        case ObjectStorage.containerNameError name of
            Just err ->
                Just err

            Nothing ->
                if List.any (\c -> c.name == name) containers then
                    Just <| "A " ++ context.localization.objectStoreContainer ++ " with that name already exists."

                else
                    Nothing


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    Element.column
        VH.formContainer
        [ Text.heading context.palette
            []
            Element.none
            (String.join " " [ "Create", toTitleCase context.localization.objectStoreContainer ])
        , case project.endpoints.swift of
            Nothing ->
                Text.p []
                    [ Text.body <|
                        String.join " "
                            [ {- @nonlocalized -} "Object storage is not available for this"
                            , context.localization.unitOfTenancy ++ "."
                            ]
                    ]

            Just _ ->
                createForm context project model
        ]


createForm : View.Types.Context -> Project -> Model -> Element.Element Msg
createForm context project model =
    let
        maybeError =
            nameError context (RDPP.withDefault [] project.objectStorageContainers) model.name

        canCreate =
            not (String.isEmpty model.name) && maybeError == Nothing
    in
    Element.column [ Element.spacing spacer.px32, Element.width Element.fill ]
        [ Element.column [ Element.spacing spacer.px12, Element.width Element.fill ]
            [ Input.text
                (VH.inputItemAttributes context.palette)
                { text = model.name
                , placeholder = Just (Input.placeholder [] (Element.text "my-container"))
                , onChange = GotName
                , label =
                    Input.labelAbove []
                        (VH.requiredLabel context.palette (Element.text "Name"))
                }
            , case maybeError of
                Just err ->
                    Validation.invalidMessage context.palette err

                Nothing ->
                    Element.none
            , Text.p []
                [ Text.body "Names can't be changed after creation." ]
            ]
        , Element.row [ Element.width Element.fill ]
            [ Element.el [ Element.alignRight ] <|
                Button.primary context.palette
                    { text = "Create"
                    , onPress =
                        if canCreate then
                            Just (GotSubmit model.name)

                        else
                            Nothing
                    }
            ]
        ]
