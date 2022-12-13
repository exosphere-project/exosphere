module Page.VolumeCreate exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Quotas as OSQuotas
import RemoteData
import String exposing (trim)
import Style.Helpers as SH exposing (spacer)
import Style.Widgets.Button as Button
import Style.Widgets.NumericTextInput.NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Style.Widgets.Text as Text
import Time
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
            ( model, Cmd.none, ProjectMsg (GetterSetters.projectIdentifier project) (RequestCreateVolume (trim model.name) validSizeGb) )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime model =
    let
        nameExists =
            VH.volumeNameExists project model.name

        renderNameExists =
            if nameExists then
                [ VH.warnMessageHelperText context.palette (VH.volumeNameExistsMessage context) ]

            else
                []

        nameSuggestionButtons =
            let
                suggestedNames =
                    VH.resourceNameSuggestions currentTime project model.name
                        |> List.filter (\n -> not (VH.volumeNameExists project n))

                suggestionButtons =
                    suggestedNames
                        |> List.map
                            (\name ->
                                Button.default
                                    context.palette
                                    { text = name
                                    , onPress = Just (GotName name)
                                    }
                            )
            in
            if nameExists then
                [ Element.row
                    [ Element.spacing spacer.px8 ]
                    suggestionButtons
                ]

            else
                [ Element.none ]

        renderInvalidReason reason =
            case reason of
                Just r ->
                    r |> VH.invalidInputHelperText context.palette

                Nothing ->
                    Element.none

        invalidReason =
            if String.isEmpty model.name then
                Just "Name is required"

            else if String.left 1 model.name == " " then
                Just "Name cannot start with a space"

            else
                Nothing

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
        , Element.column [ Element.spacing spacer.px32 ]
            [ Element.column [ Element.spacing spacer.px12 ]
                ([ Input.text
                    (VH.inputItemAttributes context.palette)
                    { text = model.name
                    , placeholder = Just (Input.placeholder [] (Element.text "My Important Data"))
                    , onChange = GotName
                    , label = Input.labelAbove [] (VH.requiredLabel context.palette (Element.text "Name"))
                    }
                 , renderInvalidReason invalidReason
                 ]
                    ++ renderNameExists
                    ++ nameSuggestionButtons
                    ++ [ Element.text <|
                            String.join " "
                                [ "(Suggestion: choose a good name that describes what the"
                                , context.localization.blockDevice
                                , "will store.)"
                                ]
                       ]
                )
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
                        case ( model.sizeInput, invalidReason ) of
                            ( ValidNumericTextInput volSizeGb, Nothing ) ->
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
              Element.row [ Element.width Element.fill ]
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
