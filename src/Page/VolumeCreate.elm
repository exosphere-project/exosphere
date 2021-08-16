module Page.VolumeCreate exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.String
import OpenStack.Quotas as OSQuotas
import RemoteData
import Style.Helpers as SH
import Style.Widgets.NumericTextInput.NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import View.Helpers as VH
import View.Types
import Widget


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
            ( model, Cmd.none, ProjectMsg project.auth.project.uuid (RequestCreateVolume model.name validSizeGb) )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
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
    Element.column (List.append VH.exoColumnAttributes [ Element.spacing 20, Element.width Element.fill ])
        [ Element.el (VH.heading2 context.palette)
            (Element.text <|
                String.join " "
                    [ "Create"
                    , context.localization.blockDevice |> Helpers.String.toTitleCase
                    ]
            )
        , Element.column VH.formContainer
            [ Input.text
                (VH.inputItemAttributes context.palette.background)
                { text = model.name
                , placeholder = Just (Input.placeholder [] (Element.text "My Important Data"))
                , onChange = GotName
                , label = Input.labelAbove [] (Element.text "Name")
                }
            , Element.text <|
                String.join " "
                    [ "(Suggestion: choose a good name that describes what the"
                    , context.localization.blockDevice
                    , "will store.)"
                    ]
            , numericTextInput
                context.palette
                (VH.inputItemAttributes context.palette.background)
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
                        case model.sizeInput of
                            ValidNumericTextInput volSizeGb ->
                                ( Just <| GotSubmit volSizeGb
                                , Nothing
                                )

                            InvalidNumericTextInput _ ->
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
                        Element.el [ Font.color <| SH.toElementColor context.palette.error ] <| Element.text text

                    Nothing ->
                        Element.none
                , Element.el [ Element.alignRight ] <|
                    Widget.textButton
                        (SH.materialStyle context.palette).primaryButton
                        { text = "Create"
                        , onPress = onPress
                        }
                ]
            ]
        ]
