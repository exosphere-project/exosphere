module Page.VolumeCreate exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Quotas as OSQuotas
import RemoteData
import Style.Helpers as SH
import Style.Widgets.Button as Button
import Style.Widgets.NumericTextInput.NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Style.Widgets.Text as Text
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import View.Helpers as VH
import View.Types


type alias Model =
    { name : String
    , sizeInput : NumericTextInput
    }


type Msg
    = GotName String
    | GotSize NumericTextInput
    | GotSubmit Int


init : Model
init =
    Model "" (ValidNumericTextInput 10)


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg )
update msg project model =
    case msg of
        GotName name ->
            ( { model | name = name }, Cmd.none, NoOp )

        GotSize sizeInput ->
            ( { model | sizeInput = sizeInput }, Cmd.none, NoOp )

        GotSubmit validSizeGb ->
            ( model, Cmd.none, ProjectMsg (GetterSetters.projectIdentifier project) (RequestCreateVolume model.name validSizeGb) )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        renderInvalidReasonsFunction reason condition =
            reason |> VH.invalidInputHelperText context.palette |> VH.renderIf condition

        ( renderInvalidReason, isNameValid ) =
            if String.isEmpty model.name then
                ( renderInvalidReasonsFunction "Name is required" True, False )

            else if String.left 1 model.name == " " then
                ( renderInvalidReasonsFunction "Name cannot start with a space" True, False )

            else if String.right 1 model.name == " " then
                ( renderInvalidReasonsFunction "Name cannot end with a space" True, False )

            else
                ( Element.none, True )

        maybeVolumeQuotaAvail =
            project.volumeQuota
                |> RemoteData.toMaybe
                |> Maybe.map OSQuotas.volumeQuotaAvail

        ( canAttemptCreateVol, volGbAvail ) =
            case maybeVolumeQuotaAvail of
                Just ( numVolsAvail, volGbAvail_ ) ->
                    ( numVolsAvail |> Maybe.map (\v -> v >= 1) |> Maybe.withDefault True
                    , volGbAvail_
                    )

                Nothing ->
                    ( True, Nothing )
    in
    Element.column
        VH.formContainer
        [ Text.heading context.palette
            []
            Element.none
            (String.join
                " "
                [ "Create"
                , context.localization.blockDevice |> Helpers.String.toTitleCase
                ]
            )
        , Element.column [ Element.spacing 16 ]
            [ Input.text
                (VH.inputItemAttributes context.palette)
                { text = model.name
                , placeholder = Just (Input.placeholder [] (Element.text "My Important Data"))
                , onChange = GotName
                , label = Input.labelAbove [] (VH.requiredLabel context.palette (Element.text "Name"))
                }
            , renderInvalidReason
            , Element.text <|
                String.join " "
                    [ "(Suggestion: choose a good name that describes what the"
                    , context.localization.blockDevice
                    , "will store.)"
                    ]
            , numericTextInput
                context.palette
                (VH.inputItemAttributes context.palette)
                model.sizeInput
                { labelText = "Size in GB"
                , minVal = Just 1
                , maxVal = volGbAvail
                , defaultVal = Just 2
                }
                GotSize
            , let
                ( onPress, quotaWarnText ) =
                    if canAttemptCreateVol then
                        case ( model.sizeInput, isNameValid ) of
                            ( ValidNumericTextInput volSizeGb, True ) ->
                                ( Just <| GotSubmit volSizeGb
                                , Nothing
                                )

                            ( _, _ ) ->
                                ( Nothing, Nothing )

                    else
                        ( Nothing
                        , Just <|
                            String.concat
                                [ "Your "
                                , context.localization.maxResourcesPerProject
                                , " does not allow for creation of another "
                                , context.localization.blockDevice
                                , "."
                                ]
                        )
              in
              Element.row (List.append VH.exoRowAttributes [ Element.width Element.fill ])
                [ case quotaWarnText of
                    Just text ->
                        Element.el [ Font.color <| SH.toElementColor context.palette.danger.textOnNeutralBG ] <| Element.text text

                    Nothing ->
                        Element.none
                , Element.el [ Element.alignRight ] <|
                    Button.primary
                        context.palette
                        { text = "Create"
                        , onPress = onPress
                        }
                ]
            ]
        ]
