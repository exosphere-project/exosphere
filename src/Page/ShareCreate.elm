module Page.ShareCreate exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Quotas as OSQuotas
import OpenStack.Types as OSTypes exposing (ShareProtocol(..), defaultShareTypeNameForProtocol)
import String exposing (trim)
import Style.Helpers as SH
import Style.Widgets.Button as Button
import Style.Widgets.NumericTextInput.NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.Validation as Validation
import Time
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import View.Forms as Forms exposing (Resource(..))
import View.Helpers as VH
import View.Types


type alias Model =
    { name : String
    , description : String
    , sizeInput : NumericTextInput
    }


type Msg
    = GotName String
    | GotDescription String
    | GotSize NumericTextInput
    | GotSubmit Int


init : Model
init =
    Model "" "" (ValidNumericTextInput 10)


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg )
update msg project model =
    case msg of
        GotName name ->
            ( { model | name = name }, Cmd.none, NoOp )

        GotDescription description ->
            ( { model | description = description }, Cmd.none, NoOp )

        GotSize sizeInput ->
            ( { model | sizeInput = sizeInput }, Cmd.none, NoOp )

        GotSubmit validSizeGb ->
            ( model, Cmd.none, ProjectMsg (GetterSetters.projectIdentifier project) (RequestCreateShare (trim model.name) (trim model.description) validSizeGb CephFS (defaultShareTypeNameForProtocol CephFS)) )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime model =
    let
        renderInvalidReason reason =
            case reason of
                Just r ->
                    r |> Validation.invalidMessage context.palette

                Nothing ->
                    Element.none

        invalidReason =
            if String.isEmpty model.name then
                Just "Name is required"

            else if String.left 1 model.name == " " then
                Just "Name cannot start with a space"

            else
                Nothing

        maybeQuotaAvail =
            project.shareQuota
                |> RDPP.toMaybe
                |> Maybe.map OSQuotas.shareQuotaAvail

        ( canAttemptCreate, gbAvail, perShareGigabytes ) =
            case maybeQuotaAvail of
                Just ( numAvail, gbAvail_, perShareGigabytes_ ) ->
                    ( case numAvail of
                        OSTypes.Limit l ->
                            l >= 1

                        OSTypes.Unlimited ->
                            True
                    , gbAvail_
                    , perShareGigabytes_
                    )

                Nothing ->
                    ( True, OSTypes.Unlimited, OSTypes.Unlimited )
    in
    Element.column
        VH.formContainer
        [ Text.heading context.palette
            []
            Element.none
            (String.join
                " "
                [ "Create"
                , context.localization.share |> Helpers.String.toTitleCase
                ]
            )
        , Element.column [ Element.spacing spacer.px32 ]
            [ Element.column [ Element.spacing spacer.px12 ]
                ([ Input.text
                    (VH.inputItemAttributes context.palette)
                    { text = model.name
                    , placeholder = Just (Input.placeholder [] (Element.text "My Shared Data"))
                    , onChange = GotName
                    , label = Input.labelAbove [] (VH.requiredLabel context.palette (Element.text "Name"))
                    }
                 , renderInvalidReason invalidReason
                 ]
                    ++ Forms.resourceNameAlreadyExists context project currentTime { resource = Share model.name, onSuggestionPressed = \suggestion -> GotName suggestion }
                    ++ [ Element.text <|
                            String.join " "
                                [ "(Note:"
                                , Helpers.String.toTitleCase <| Helpers.String.pluralize context.localization.share
                                , "require a globally unique name.)"
                                ]
                       ]
                )
            , numericTextInput
                context.palette
                (VH.inputItemAttributes context.palette)
                model.sizeInput
                { labelText = "Size in GB"
                , minVal = Just 1
                , maxVal =
                    case ( gbAvail, perShareGigabytes ) of
                        ( OSTypes.Limit totalLimit, OSTypes.Limit perShareLimit ) ->
                            Just <| min totalLimit perShareLimit

                        ( OSTypes.Limit totalLimit, OSTypes.Unlimited ) ->
                            Just totalLimit

                        ( OSTypes.Unlimited, OSTypes.Limit perShareLimit ) ->
                            Just perShareLimit

                        ( OSTypes.Unlimited, OSTypes.Unlimited ) ->
                            Nothing
                , defaultVal = Just 2
                , required = True
                }
                GotSize
            , Input.multiline
                (VH.inputItemAttributes context.palette
                    ++ [ Element.height <| Element.px 200
                       , Element.width Element.fill
                       ]
                )
                { onChange = GotDescription
                , text = model.description
                , placeholder = Just <| Input.placeholder [] (Text.body <| "An optional description for the " ++ context.localization.share ++ ".")
                , label = Input.labelAbove [] (Element.text "Description (optional)")
                , spellcheck = True
                }
            , let
                ( onPress, quotaWarnText ) =
                    if canAttemptCreate then
                        case ( model.sizeInput, invalidReason ) of
                            ( ValidNumericTextInput sizeGb, Nothing ) ->
                                ( Just <| GotSubmit sizeGb
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
                                , context.localization.share
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
