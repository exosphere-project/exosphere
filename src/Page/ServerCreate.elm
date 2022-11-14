module Page.ServerCreate exposing (Model, Msg(..), init, update, view)

import DateFormat
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (Unit(..), humanCount)
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.Random as RandomHelper
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Maybe
import OpenStack.Quotas as OSQuotas
import OpenStack.ServerNameValidator exposing (serverNameValidator)
import OpenStack.Types as OSTypes
import RemoteData
import Route
import ServerDeploy exposing (cloudInitUserDataTemplate)
import Style.Helpers as SH exposing (spacer)
import Style.Types as ST
import Style.Widgets.Alert as Alert
import Style.Widgets.Button as Button
import Style.Widgets.NumericTextInput.NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Style.Widgets.Select
import Style.Widgets.Tag as Tag
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Time
import Types.HelperTypes as HelperTypes
    exposing
        ( FloatingIpAssignmentStatus(..)
        , FloatingIpOption(..)
        , FloatingIpReuseOption(..)
        )
import Types.Project exposing (Project)
import Types.Server exposing (NewServerNetworkOptions(..))
import Types.SharedMsg as SharedMsg
import View.Helpers as VH exposing (edges)
import View.Types
import Widget


type alias Model =
    HelperTypes.CreateServerPageModel


type Msg
    = GotServerName String
    | GotChooseRandomServerName
    | GotRandomServerName String
    | GotCount Int
    | GotCreateServerButtonPressed OSTypes.NetworkUuid OSTypes.FlavorId
    | GotFlavorId OSTypes.FlavorId
    | GotFlavorList
    | GotVolSizeTextInput (Maybe NumericTextInput)
    | GotUserDataTemplate String
    | GotNetworks
    | GotNetworkUuid (Maybe OSTypes.NetworkUuid)
    | GotAutoAllocatedNetwork OSTypes.NetworkUuid
    | GotShowAdvancedOptions Bool
    | GotKeypairName (Maybe String)
    | GotDeployGuacamole (Maybe Bool)
    | GotDeployDesktopEnvironment Bool
    | GotInstallOperatingSystemUpdates Bool
    | GotFloatingIpCreationOption FloatingIpOption
    | GotIncludeWorkflow Bool
    | GotWorkflowRepository String
    | GotWorkflowReference String
    | GotWorkflowPath String
    | GotWorkflowInputLoseFocus
    | GotCreateCluster Bool
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : OSTypes.ImageUuid -> String -> Maybe (List OSTypes.FlavorId) -> Maybe Bool -> Model
init imageUuid imageName restrictFlavorIds deployGuacamole =
    { serverName = ""
    , imageUuid = imageUuid
    , imageName = imageName
    , restrictFlavorIds = restrictFlavorIds
    , count = 1
    , flavorId = Nothing
    , volSizeTextInput = Nothing
    , userDataTemplate = cloudInitUserDataTemplate
    , networkUuid = Nothing
    , showAdvancedOptions = False
    , keypairName = Nothing
    , deployGuacamole = deployGuacamole
    , deployDesktopEnvironment = False
    , installOperatingSystemUpdates = True
    , floatingIpCreationOption = HelperTypes.Automatic
    , includeWorkflow = False
    , workflowInputRepository = ""
    , workflowInputReference = ""
    , workflowInputPath = ""
    , workflowInputIsValid = Nothing
    , createCluster = False
    , showFormInvalidToggleTip = False
    , createServerAttempted = False
    , randomServerName = ""
    }


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotRandomServerName name ->
            ( { model | randomServerName = name, serverName = name }, Cmd.none, SharedMsg.NoOp )

        GotChooseRandomServerName ->
            let
                generateCmd =
                    RandomHelper.generateServerName
                        (\serverName ->
                            GotRandomServerName serverName
                        )
            in
            ( model, generateCmd, SharedMsg.NoOp )

        GotServerName name ->
            ( { model | serverName = name }, Cmd.none, SharedMsg.NoOp )

        GotCount count ->
            ( enforceQuotaCompliance project { model | count = count }, Cmd.none, SharedMsg.NoOp )

        GotCreateServerButtonPressed netUuid flavorId ->
            ( { model | createServerAttempted = True }
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) (SharedMsg.RequestCreateServer model netUuid flavorId)
            )

        GotFlavorId flavorId ->
            ( enforceQuotaCompliance project { model | flavorId = Just flavorId }, Cmd.none, SharedMsg.NoOp )

        GotFlavorList ->
            let
                allowedFlavors =
                    case model.restrictFlavorIds of
                        Nothing ->
                            project.flavors

                        Just restrictedFlavorIds ->
                            restrictedFlavorIds
                                |> List.filterMap (GetterSetters.flavorLookup project)

                maybeSmallestFlavor =
                    GetterSetters.sortedFlavors allowedFlavors |> List.head
            in
            case maybeSmallestFlavor of
                Just smallestFlavor ->
                    ( enforceQuotaCompliance project
                        { model
                            | flavorId =
                                Just <| Maybe.withDefault smallestFlavor.id model.flavorId
                        }
                    , Cmd.none
                    , SharedMsg.NoOp
                    )

                Nothing ->
                    ( model, Cmd.none, SharedMsg.NoOp )

        GotVolSizeTextInput maybeVolSizeInput ->
            ( enforceQuotaCompliance project { model | volSizeTextInput = maybeVolSizeInput }, Cmd.none, SharedMsg.NoOp )

        GotUserDataTemplate userData ->
            ( { model | userDataTemplate = userData }, Cmd.none, SharedMsg.NoOp )

        GotNetworks ->
            -- SharedModel just updated with new networks, choose a default if haven't done so already
            if model.networkUuid == Nothing then
                case Helpers.newServerNetworkOptions project of
                    AutoSelectedNetwork netUuid ->
                        ( { model | networkUuid = Just netUuid }, Cmd.none, SharedMsg.NoOp )

                    _ ->
                        ( model, Cmd.none, SharedMsg.NoOp )

            else
                ( model, Cmd.none, SharedMsg.NoOp )

        GotNetworkUuid maybeNetworkUuid ->
            ( { model | networkUuid = maybeNetworkUuid }, Cmd.none, SharedMsg.NoOp )

        GotAutoAllocatedNetwork autoAllocatedNetworkUuid ->
            ( { model
                | networkUuid =
                    if model.networkUuid == Nothing then
                        Just autoAllocatedNetworkUuid

                    else
                        model.networkUuid
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotShowAdvancedOptions showAdvancedOptions ->
            ( { model | showAdvancedOptions = showAdvancedOptions }, Cmd.none, SharedMsg.NoOp )

        GotKeypairName maybeKeypairName ->
            ( { model | keypairName = maybeKeypairName }, Cmd.none, SharedMsg.NoOp )

        GotDeployGuacamole maybeDeployGuacamole ->
            ( { model | deployGuacamole = maybeDeployGuacamole }, Cmd.none, SharedMsg.NoOp )

        GotDeployDesktopEnvironment deployDesktopEnvironment ->
            ( { model | deployDesktopEnvironment = deployDesktopEnvironment }, Cmd.none, SharedMsg.NoOp )

        GotInstallOperatingSystemUpdates installUpdates ->
            ( { model | installOperatingSystemUpdates = installUpdates }, Cmd.none, SharedMsg.NoOp )

        GotFloatingIpCreationOption floatingIpOption ->
            ( { model | floatingIpCreationOption = floatingIpOption }, Cmd.none, SharedMsg.NoOp )

        GotIncludeWorkflow includeWorkflow ->
            ( { model
                | includeWorkflow = includeWorkflow
                , workflowInputIsValid =
                    if includeWorkflow then
                        Just False

                    else
                        Nothing
                , workflowInputRepository = ""
                , workflowInputReference = ""
                , workflowInputPath = ""
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotWorkflowRepository repository ->
            let
                newWorkflowInputIsValid =
                    if model.workflowInputIsValid /= Nothing then
                        Just
                            (workflowInputIsValid
                                ( model.workflowInputRepository
                                , model.workflowInputReference
                                , model.workflowInputPath
                                )
                            )

                    else
                        Nothing
            in
            ( { model
                | workflowInputRepository = repository
                , workflowInputIsValid = newWorkflowInputIsValid
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotWorkflowInputLoseFocus ->
            ( { model
                | workflowInputIsValid =
                    Just
                        (workflowInputIsValid
                            ( model.workflowInputRepository
                            , model.workflowInputReference
                            , model.workflowInputPath
                            )
                        )
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotWorkflowReference reference ->
            ( { model
                | workflowInputReference = reference
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotWorkflowPath path ->
            ( { model
                | workflowInputPath = path
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotCreateCluster createCluster ->
            ( { model
                | createCluster = createCluster
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


enforceQuotaCompliance : Project -> Model -> Model
enforceQuotaCompliance project model =
    -- If user is trying to choose a combination of flavor, volume-backed disk size, and count
    -- that would exceed quota, reduce count to comply with quota.
    case
        ( model.flavorId |> Maybe.andThen (GetterSetters.flavorLookup project)
        , project.computeQuota
        , project.volumeQuota
        )
    of
        ( Just flavor, RemoteData.Success computeQuota, RemoteData.Success volumeQuota ) ->
            let
                availServers =
                    OSQuotas.overallQuotaAvailServers
                        (model.volSizeTextInput
                            |> Maybe.andThen Style.Widgets.NumericTextInput.NumericTextInput.toMaybe
                        )
                        flavor
                        computeQuota
                        volumeQuota
            in
            { model
                | count =
                    case availServers of
                        Just availServers_ ->
                            if model.count > availServers_ then
                                availServers_

                            else
                                model.count

                        Nothing ->
                            model.count
            }

        ( _, _, _ ) ->
            model


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime model =
    let
        serverNameExists =
            VH.serverNameExists project model.serverName

        renderServerNameExists =
            if serverNameExists then
                [ VH.warnMessageHelperText context.palette (VH.serverNameExistsMessage context) ]

            else
                []

        nameSuggestionButtons =
            let
                currentDate =
                    DateFormat.format
                        [ DateFormat.yearNumber
                        , DateFormat.text "-"
                        , DateFormat.monthFixed
                        , DateFormat.text "-"
                        , DateFormat.dayOfMonthFixed
                        ]
                        Time.utc
                        currentTime

                suggestedNameWithUsername =
                    if not (String.contains project.auth.user.name model.serverName) then
                        [ model.serverName
                            ++ " "
                            ++ project.auth.user.name
                        ]

                    else
                        []

                suggestedNameWithDate =
                    if not (String.contains currentDate model.serverName) then
                        [ model.serverName
                            ++ " "
                            ++ currentDate
                        ]

                    else
                        []

                suggestedNameWithUsernameAndDate =
                    if not (String.contains project.auth.user.name model.serverName) && not (String.contains currentDate model.serverName) then
                        [ model.serverName
                            ++ " "
                            ++ project.auth.user.name
                            ++ " "
                            ++ currentDate
                        ]

                    else
                        []

                namesSuggestionsNotDuplicated serverName =
                    not (VH.serverNameExists project serverName)

                filteredSuggestedNames =
                    (model.randomServerName :: suggestedNameWithUsername ++ suggestedNameWithDate ++ suggestedNameWithUsernameAndDate)
                        |> List.filter namesSuggestionsNotDuplicated

                suggestionButtons =
                    filteredSuggestedNames
                        |> List.map
                            (\name ->
                                Button.default
                                    context.palette
                                    { text = name
                                    , onPress = Just (GotServerName name)
                                    }
                            )
            in
            if serverNameExists then
                [ Element.row
                    [ -- 75 is used to align it vertically with name input text box
                      Element.paddingEach { edges | left = 75 }
                    , Element.spacing spacer.px8
                    ]
                    suggestionButtons
                ]

            else
                [ Element.none ]

        invalidNameReasons =
            serverNameValidator (Just context.localization.virtualComputer) model.serverName

        serverNameValidationStatusAttributes =
            case ( invalidNameReasons, serverNameExists ) of
                ( Nothing, False ) ->
                    VH.validInputAttributes context.palette

                ( Nothing, True ) ->
                    VH.warningInputAttributes context.palette

                ( Just _, _ ) ->
                    VH.invalidInputAttributes context.palette

        renderInvalidNameReasons =
            case invalidNameReasons of
                Just reasons ->
                    List.map (VH.invalidInputHelperText context.palette) reasons

                Nothing ->
                    []

        maybeNetworkGuidance =
            case Helpers.newServerNetworkOptions project of
                NetworksLoading ->
                    Just "Loading networks, please wait a moment."

                AutoSelectedNetwork _ ->
                    Nothing

                ManualNetworkSelection ->
                    case model.networkUuid of
                        Just _ ->
                            Nothing

                        Nothing ->
                            Just <|
                                String.join " "
                                    [ "Exosphere could not determine a suitable network to create a server."
                                    , "Please select a network in the advanced options."
                                    ]

                NoneAvailable ->
                    Just <|
                        String.join " "
                            [ "No networks to create a server available."
                            , "Please contact your cloud administrator."
                            ]

        renderNetworkGuidance =
            case maybeNetworkGuidance of
                Nothing ->
                    Element.none

                Just guidanceText ->
                    Element.paragraph
                        [ Font.color (SH.toElementColor context.palette.danger.textOnNeutralBG)
                        , Element.alignRight
                        ]
                        [ Element.text guidanceText
                        ]

        contents flavor computeQuota volumeQuota =
            let
                hasAvailableResources =
                    let
                        allowedFlavors =
                            case model.restrictFlavorIds of
                                Nothing ->
                                    project.flavors

                                Just restrictedFlavorIds ->
                                    restrictedFlavorIds
                                        |> List.filterMap (GetterSetters.flavorLookup project)

                        canBeLaunched quota fl =
                            case OSQuotas.computeQuotaFlavorAvailServers quota fl of
                                Nothing ->
                                    False

                                Just availServers ->
                                    availServers >= 1

                        flavorAvailability : List Bool
                        flavorAvailability =
                            allowedFlavors
                                |> List.map (canBeLaunched computeQuota)
                    in
                    List.any identity flavorAvailability

                ( createOnPress, maybeInvalidFormFields ) =
                    let
                        invalidVolSizeTextInput =
                            case model.volSizeTextInput of
                                Just input ->
                                    case input of
                                        ValidNumericTextInput _ ->
                                            False

                                        InvalidNumericTextInput _ ->
                                            True

                                Nothing ->
                                    False

                        invalidWorkflowTextInput =
                            model.workflowInputRepository == "" && model.workflowInputIsValid == Just False

                        invalidInputs =
                            invalidVolSizeTextInput || invalidWorkflowTextInput || not hasAvailableResources
                    in
                    case ( invalidNameReasons, invalidInputs ) of
                        ( Nothing, False ) ->
                            case ( model.networkUuid, model.flavorId ) of
                                ( Just netUuid, Just flavorId ) ->
                                    ( Just <| GotCreateServerButtonPressed netUuid flavorId
                                    , Nothing
                                    )

                                ( _, _ ) ->
                                    let
                                        invalidNetworkField =
                                            if model.networkUuid == Nothing then
                                                [ "network" ]

                                            else
                                                []

                                        invalidFlavorField =
                                            if model.flavorId == Nothing then
                                                [ "flavor" ]

                                            else
                                                []

                                        invalidFormFields =
                                            invalidNetworkField
                                                ++ invalidFlavorField
                                    in
                                    ( Nothing, Just invalidFormFields )

                        ( _, _ ) ->
                            let
                                invalidNameFormField =
                                    if invalidNameReasons == Nothing then
                                        []

                                    else
                                        [ context.localization.virtualComputer ++ " name" ]

                                invalidVolSizeField =
                                    if invalidVolSizeTextInput then
                                        [ "custom root disk size" ]

                                    else
                                        []

                                invalidWorkflowField =
                                    if invalidWorkflowTextInput then
                                        [ "workflow repository" ]

                                    else
                                        []

                                invalidFormFields =
                                    invalidNameFormField
                                        ++ invalidVolSizeField
                                        ++ invalidWorkflowField
                            in
                            ( Nothing, Just invalidFormFields )

                createButton =
                    if model.createServerAttempted then
                        loading
                            ("Creating "
                                ++ context.localization.virtualComputer
                                |> Helpers.String.toTitleCase
                            )

                    else
                        Button.primary
                            context.palette
                            { text = "Create"
                            , onPress = createOnPress
                            }

                invalidFormHintView =
                    if hasAvailableResources == False then
                        -- this error isn't form field specific, show it over other errors
                        let
                            invalidFormHint =
                                (context.localization.maxResourcesPerProject
                                    |> Helpers.String.pluralize
                                    |> Helpers.String.toTitleCase
                                )
                                    ++ " have been exhausted. Contact your cloud administrator, or delete some stuff"
                        in
                        VH.invalidInputHelperText context.palette invalidFormHint

                    else
                        case maybeInvalidFormFields of
                            Nothing ->
                                Element.none

                            Just _ ->
                                let
                                    genericInvalidFormHint =
                                        "Please correct problems with the form"

                                    invalidFormHint =
                                        case maybeInvalidFormFields of
                                            Just invalidFormFields ->
                                                let
                                                    invalidFormFieldsString =
                                                        Helpers.String.itemsListToString <|
                                                            List.map (\s -> "'" ++ s ++ "'")
                                                                invalidFormFields
                                                in
                                                if List.isEmpty invalidFormFields then
                                                    genericInvalidFormHint

                                                else if List.length invalidFormFields == 1 then
                                                    "Please correct problem with "
                                                        ++ invalidFormFieldsString
                                                        ++ " field"

                                                else
                                                    "Please correct problems with "
                                                        ++ invalidFormFieldsString
                                                        ++ " fields"

                                            Nothing ->
                                                genericInvalidFormHint
                                in
                                VH.invalidInputHelperText context.palette invalidFormHint
            in
            [ Element.column
                [ Element.spacing spacer.px8
                , Element.width Element.fill
                ]
                (Element.row
                    [ Element.spacing spacer.px8
                    , Element.width Element.fill
                    ]
                    [ Input.text
                        (VH.inputItemAttributes context.palette
                            ++ serverNameValidationStatusAttributes
                        )
                        { text = model.serverName
                        , placeholder =
                            Just
                                (Input.placeholder
                                    []
                                    (Element.text <|
                                        String.join " "
                                            [ "Example, My"
                                            , context.localization.virtualComputer
                                                |> Helpers.String.toTitleCase
                                            ]
                                    )
                                )
                        , onChange = GotServerName
                        , label =
                            Input.labelLeft []
                                (VH.requiredLabel context.palette (Element.text "Name"))
                        }
                    , Button.default
                        context.palette
                        { text = "Random Name"
                        , onPress = Just GotChooseRandomServerName
                        }
                    ]
                    :: renderInvalidNameReasons
                    ++ nameSuggestionButtons
                    ++ renderServerNameExists
                )
            , Element.row [ Element.spacing spacer.px8 ]
                [ Element.el [ Font.semiBold ] <|
                    Element.text <|
                        String.concat
                            [ context.localization.staticRepresentationOfBlockDeviceContents
                                |> Helpers.String.toTitleCase
                            , ": "
                            ]
                , Element.text model.imageName
                ]
            , VH.flavorPicker context
                project
                model.restrictFlavorIds
                computeQuota
                (\flavorGroupTipId -> SharedMsg <| SharedMsg.TogglePopover flavorGroupTipId)
                (Helpers.String.hyphenate [ "serverCreateFlavorGroupTip", project.auth.project.uuid ])
                Nothing
                model.flavorId
                GotFlavorId
            , volBackedPrompt context model volumeQuota flavor
            , countPicker context model computeQuota volumeQuota flavor
            , desktopEnvironmentPicker context project model
            , customWorkflowInput context project model
            , Element.column
                [ Element.spacing spacer.px32 ]
              <|
                Input.radioRow [ Element.spacing spacer.px32 ]
                    { label =
                        Input.labelAbove (VH.radioLabelAttributes True)
                            (Element.text "Advanced Options")
                    , onChange = GotShowAdvancedOptions
                    , options =
                        [ Input.option False (Element.text "Hide")
                        , Input.option True (Element.text "Show")

                        {- -}
                        ]
                    , selected = Just model.showAdvancedOptions
                    }
                    :: (if not model.showAdvancedOptions then
                            [ Element.none ]

                        else
                            [ skipOperatingSystemUpdatesPicker context model
                            , guacamolePicker context model
                            , networkPicker context project model
                            , floatingIpPicker context project model
                            , keypairPicker context project model
                            , clusterInput context model
                            , userDataInput context model
                            ]
                       )
            , renderNetworkGuidance
            , Element.row
                [ -- add extra padding to make it look separate from all form fields with uniform 32px spacing
                  -- inspired from PF demos: https://www.patternfly.org/v4/components/form/#basic
                  Element.paddingEach { edges | top = spacer.px32 }
                , Element.spacing spacer.px8
                , Element.width Element.fill
                ]
                [ Element.el [ Element.width Element.fill ] invalidFormHintView
                , Element.el [ Element.alignRight ] createButton
                ]
            ]

        loading message =
            Element.row [ Element.spacing spacer.px16 ]
                [ Widget.circularProgressIndicator
                    (SH.materialStyle context.palette).progressIndicator
                    Nothing
                , Element.text message
                ]
    in
    Element.column VH.formContainer <|
        [ Text.heading context.palette
            []
            Element.none
            (String.join " "
                [ "Create"
                , context.localization.virtualComputer
                    |> Helpers.String.toTitleCase
                ]
            )
        , Element.column
            [ -- Keeps form fields from displaying too wide
              Element.width (Element.maximum 600 Element.fill)

            -- PatternFly guidelines: There should be atleast 24 pixels between field inside a form (spacing)
            , Element.spacing spacer.px32
            ]
          <|
            case
                ( model.flavorId |> Maybe.andThen (GetterSetters.flavorLookup project)
                , project.computeQuota
                , project.volumeQuota
                )
            of
                ( Just flavor, RemoteData.Success computeQuota, RemoteData.Success volumeQuota ) ->
                    contents flavor computeQuota volumeQuota

                ( _, _, RemoteData.Loading ) ->
                    [ loading "Loading..." ]

                ( _, RemoteData.Loading, _ ) ->
                    [ loading "Loading..." ]

                ( _, _, _ ) ->
                    [ loading "Loading..." ]
        ]


volBackedPrompt : View.Types.Context -> Model -> OSTypes.VolumeQuota -> OSTypes.Flavor -> Element.Element Msg
volBackedPrompt context model volumeQuota flavor =
    let
        { locale } =
            context

        ( volumeCountAvail, volumeSizeGbAvail ) =
            OSQuotas.volumeQuotaAvail volumeQuota

        canLaunchVolBacked =
            let
                tooSmall quotaItem minVal =
                    case quotaItem of
                        Just val ->
                            minVal > val

                        Nothing ->
                            False
            in
            not (tooSmall volumeCountAvail 1 || tooSmall volumeSizeGbAvail 2)

        flavorRootDiskSize =
            flavor.disk_root

        nonVolBackedOptionText =
            if flavorRootDiskSize == 0 then
                String.join " "
                    [ "Default for selected"
                    , context.localization.staticRepresentationOfBlockDeviceContents
                    , "(warning, could be too small for your work)"
                    ]

            else
                String.concat
                    [ FormatNumber.format { locale | decimals = Exact 0 } (toFloat flavorRootDiskSize)
                    , " GB (default for selected "
                    , context.localization.virtualComputerHardwareConfig
                    , ")"
                    ]

        defaultVolSizeGB =
            10

        defaultVolNumericInputParams =
            { labelText = "Root disk size (GB)"
            , minVal = Just 2
            , maxVal = volumeSizeGbAvail
            , defaultVal = Just defaultVolSizeGB
            }

        radioInput =
            Input.radio [ Element.spacing spacer.px4 ]
                { label = Input.labelHidden "Root disk size"
                , onChange =
                    \new ->
                        let
                            newVolSizeTextInput =
                                if new == True then
                                    Just <| ValidNumericTextInput defaultVolSizeGB

                                else
                                    Nothing
                        in
                        GotVolSizeTextInput newVolSizeTextInput
                , options =
                    [ Input.option False (Element.text nonVolBackedOptionText)
                    , Input.option True
                        (Element.text <|
                            String.concat
                                [ "Custom disk size ("
                                , context.localization.blockDevice
                                , "-backed)"
                                ]
                        )
                    ]
                , selected =
                    case model.volSizeTextInput of
                        Just _ ->
                            Just True

                        Nothing ->
                            Just False
                }
    in
    Element.column [ Element.spacing spacer.px12 ]
        [ Element.el [ Font.semiBold ] <| Element.text "Choose a root disk size"
        , if canLaunchVolBacked then
            radioInput

          else
            Element.text <|
                String.concat
                    [ "(N/A: "
                    , context.localization.blockDevice
                    , " "
                    , context.localization.maxResourcesPerProject
                    , " exhausted, cannot launch a "
                    , context.localization.blockDevice
                    , "-backed instance)"
                    ]
        , case model.volSizeTextInput of
            Nothing ->
                Element.none

            Just volSizeTextInput ->
                Element.row [ Element.spacing spacer.px8 ]
                    [ numericTextInput
                        context.palette
                        (VH.inputItemAttributes context.palette)
                        volSizeTextInput
                        defaultVolNumericInputParams
                        (\newInput -> GotVolSizeTextInput <| Just newInput)
                    , case ( volumeSizeGbAvail, volSizeTextInput ) of
                        ( Just volumeSizeAvail_, ValidNumericTextInput i ) ->
                            if i == volumeSizeAvail_ then
                                Element.text ("(" ++ context.localization.maxResourcesPerProject ++ " max)")

                            else
                                Element.none

                        ( _, _ ) ->
                            Element.none
                    ]
        ]


countPicker :
    View.Types.Context
    -> Model
    -> OSTypes.ComputeQuota
    -> OSTypes.VolumeQuota
    -> OSTypes.Flavor
    -> Element.Element Msg
countPicker context model computeQuota volumeQuota flavor =
    let
        { locale } =
            context

        countAvailPerQuota =
            OSQuotas.overallQuotaAvailServers
                (model.volSizeTextInput
                    |> Maybe.andThen Style.Widgets.NumericTextInput.NumericTextInput.toMaybe
                )
                flavor
                computeQuota
                volumeQuota

        -- Exosphere becomes slow and unresponsive in the browser if the user creates too many instances at a time, this prevents that.
        countAvailPerApp =
            25
    in
    Element.column [ Element.spacing spacer.px12 ]
        [ Element.el [ Font.semiBold ] <|
            Element.row [ Element.spacing spacer.px12 ]
                [ Element.text <|
                    String.concat
                        [ "How many "
                        , context.localization.virtualComputer
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        , "?"
                        ]
                , Style.Widgets.ToggleTip.toggleTip
                    context
                    (\multipleInstancesNamingTipId -> SharedMsg <| SharedMsg.TogglePopover multipleInstancesNamingTipId)
                    "multipleInstancesNamingToggleTip"
                    (Element.paragraph
                        [ Element.width (Element.fill |> Element.minimum 300)
                        , Element.spacing spacer.px8
                        , Font.regular
                        ]
                        [ Element.text <|
                            String.concat
                                [ "If more than one "
                                    ++ context.localization.virtualComputer
                                    |> Helpers.String.pluralize
                                , " is chosen, each will be named, for example, \""
                                , model.serverName
                                , " 1 of "
                                , if model.count == 1 then
                                    "n"

                                  else
                                    String.fromInt model.count
                                , "\""
                                ]
                        ]
                    )
                    ST.PositionRight
                ]
        , case countAvailPerQuota of
            Just countAvailPerQuota_ ->
                let
                    text =
                        Element.text <|
                            String.join " " <|
                                List.concat
                                    [ [ "Your"
                                      , context.localization.maxResourcesPerProject
                                      , "supports up to"
                                      , humanCount locale countAvailPerQuota_
                                      , "of these."
                                      ]
                                    , if countAvailPerQuota_ > countAvailPerApp then
                                        [ "Exosphere can create up to"
                                        , String.fromInt countAvailPerApp
                                        , "at a time."
                                        ]

                                      else
                                        []
                                    ]
                in
                Element.paragraph [] [ text ]

            Nothing ->
                Element.none
        , Element.row [ Element.spacing spacer.px12 ]
            [ Input.slider
                [ Element.height (Element.px 30)
                , Element.width (Element.px 100 |> Element.minimum 200)

                -- Here is where we're creating/styling the "track"
                , Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Background.color (SH.toElementColor context.palette.neutral.icon)
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = \c -> GotCount <| round c
                , label = Input.labelHidden "How many?"
                , min = 1
                , max =
                    countAvailPerQuota
                        |> Maybe.map (\countAvailPerQuota_ -> min countAvailPerQuota_ countAvailPerApp)
                        |> Maybe.withDefault countAvailPerApp
                        |> toFloat
                , step = Just 1
                , value = toFloat model.count
                , thumb =
                    Input.defaultThumb
                }
            , Element.el
                [ Element.width Element.shrink ]
                (Element.text (humanCount locale model.count))
            , case countAvailPerQuota of
                Just countAvailPerQuota_ ->
                    if model.count == countAvailPerQuota_ then
                        Element.text ("(" ++ context.localization.maxResourcesPerProject ++ " max)")

                    else if model.count == countAvailPerApp then
                        Element.text "(max createable at a time)"

                    else
                        Element.none

                Nothing ->
                    Element.none
            ]
        ]


customWorkflowInput : View.Types.Context -> Project -> Model -> Element.Element Msg
customWorkflowInput context project model =
    if context.experimentalFeaturesEnabled then
        customWorkflowInputExperimental context project model

    else
        Element.none


workflowInputIsEmpty : ( String, String, String ) -> Bool
workflowInputIsEmpty ( repository, reference, path ) =
    [ repository
    , reference
    , path
    ]
        |> List.map String.trim
        |> List.all String.isEmpty


workflowInputIsValid : ( String, String, String ) -> Bool
workflowInputIsValid ( repository, reference, path ) =
    (repository /= "") && not (workflowInputIsEmpty ( repository, reference, path ))


customWorkflowInputExperimental : View.Types.Context -> Project -> Model -> Element.Element Msg
customWorkflowInputExperimental context project model =
    let
        workflowInput =
            let
                displayRepoInputError =
                    model.workflowInputRepository == "" && model.workflowInputIsValid == Just False

                repoInputLabel =
                    VH.requiredLabel context.palette (Element.text "DOI or Git repository URL")

                repoInputHelperText =
                    if displayRepoInputError then
                        VH.invalidInputHelperText context.palette "Required"

                    else
                        Element.none

                inputValidationStatusAttributes =
                    if displayRepoInputError then
                        VH.invalidInputAttributes context.palette

                    else
                        []

                repoInput =
                    Element.column [ Element.width Element.fill, Element.spacing spacer.px12 ]
                        [ Input.text
                            (Events.onLoseFocus GotWorkflowInputLoseFocus
                                :: (VH.inputItemAttributes context.palette
                                        ++ inputValidationStatusAttributes
                                   )
                            )
                            { text = model.workflowInputRepository
                            , placeholder =
                                Just
                                    (Input.placeholder
                                        []
                                        (Element.text "Example, https://github.com/binder-examples/minimal-dockerfile")
                                    )
                            , onChange =
                                GotWorkflowRepository
                            , label = Input.labelAbove [] repoInputLabel
                            }
                        , repoInputHelperText
                        ]

                referenceInput =
                    Input.text
                        (VH.inputItemAttributes context.palette
                            ++ [ Events.onLoseFocus GotWorkflowInputLoseFocus ]
                        )
                        { text = model.workflowInputReference
                        , placeholder =
                            Just
                                (Input.placeholder
                                    []
                                    (Element.text "Example, HEAD")
                                )
                        , onChange =
                            GotWorkflowReference
                        , label = Input.labelAbove [] (Element.text "Git reference (branch, tag, or commit)")
                        }

                sourcePathInput =
                    let
                        pathInputLabel =
                            "Path to open"
                    in
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing spacer.px12
                        ]
                        [ Element.text pathInputLabel
                        , Input.text
                            (VH.inputItemAttributes context.palette
                                ++ [ Events.onLoseFocus GotWorkflowInputLoseFocus ]
                            )
                            { text = model.workflowInputPath
                            , placeholder =
                                Just
                                    (Input.placeholder
                                        []
                                        (Element.text "Example, /rstudio")
                                    )
                            , onChange =
                                GotWorkflowPath
                            , label = Input.labelHidden pathInputLabel
                            }
                        ]
            in
            Element.column
                [ Element.width Element.fill
                , Element.spacing spacer.px24
                ]
                [ repoInput
                , referenceInput
                , sourcePathInput
                ]

        workflowExplanationToggleTip =
            Style.Widgets.ToggleTip.toggleTip
                context
                (\workflowExplainationTipId -> SharedMsg <| SharedMsg.TogglePopover workflowExplainationTipId)
                (Helpers.String.hyphenate [ "workflowExplainationTip", project.auth.project.uuid ])
                (Element.column
                    [ Element.width
                        (Element.fill
                            |> Element.minimum 100
                        )
                    , Element.spacing spacer.px8
                    ]
                    [ Element.text "Any Binder™-compatible repository can be launched."
                    , Element.paragraph []
                        [ Element.text "See mybinder.org for more information"
                        ]
                    ]
                )
                ST.PositionTop

        experimentalTag =
            Tag.tag context.palette "Experimental"
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing spacer.px24
        ]
    <|
        (Input.radioRow [ Element.spacing spacer.px32 ]
            { label =
                Input.labelAbove (VH.radioLabelAttributes False)
                    (Element.row [ Element.spacing spacer.px8 ]
                        [ Text.text Text.H4
                            []
                            ("Launch a workflow in the " ++ context.localization.virtualComputer)
                        , experimentalTag
                        , workflowExplanationToggleTip
                        ]
                    )
            , onChange = GotIncludeWorkflow
            , options =
                [ Input.option False (Element.text "No")
                , Input.option True (Element.text "Yes")

                {- -}
                ]
            , selected = Just model.includeWorkflow
            }
            :: (if not model.includeWorkflow then
                    [ Element.none ]

                else
                    [ workflowInput
                    ]
               )
        )


clusterInput : View.Types.Context -> Model -> Element.Element Msg
clusterInput context model =
    if context.experimentalFeaturesEnabled then
        clusterInputExperimental context model

    else
        Element.none


clusterInputExperimental : View.Types.Context -> Model -> Element.Element Msg
clusterInputExperimental context model =
    let
        warnings =
            [ Element.text "Warning: This will only work on Jetstream Cloud, and can take 30 minutes or longer to set up a cluster."
            , Element.text <|
                String.concat
                    [ "This feature currently only supports "
                    , context.localization.staticRepresentationOfBlockDeviceContents
                        |> Helpers.String.pluralize
                    , " based on Rocky Linux 8. If you choose "
                    , context.localization.staticRepresentationOfBlockDeviceContents
                        |> Helpers.String.indefiniteArticle
                    , " "
                    , context.localization.staticRepresentationOfBlockDeviceContents
                    , " based on a different operating system it is unlikely to work."
                    ]
            ]

        experimentalTag =
            Tag.tag context.palette "Experimental"
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing spacer.px12
        ]
        [ Input.radioRow [ Element.spacing spacer.px32 ]
            { label =
                Input.labelAbove (VH.radioLabelAttributes False)
                    (Element.wrappedRow [ Element.spacing spacer.px8 ]
                        [ Text.text Text.H4 [] ("Create your own SLURM cluster with this " ++ context.localization.virtualComputer ++ " as the head node")
                        , experimentalTag
                        ]
                    )
            , onChange = GotCreateCluster
            , options =
                [ Input.option False (Element.text "No")
                , Input.option True (Element.text "Yes")

                {- -}
                ]
            , selected = Just model.createCluster
            }
        , if model.createCluster then
            Alert.alert []
                context.palette
                { state = Alert.Warning
                , showIcon = False
                , showContainer = True
                , content =
                    Element.column
                        [ Element.spacing spacer.px12, Element.width Element.fill ]
                        (List.map (\warning -> Element.paragraph [] [ warning ]) warnings)
                }

          else
            Element.none
        ]


desktopEnvironmentPicker : View.Types.Context -> Project -> Model -> Element.Element Msg
desktopEnvironmentPicker context project model =
    let
        genericMessage : String
        genericMessage =
            String.join " "
                [ context.localization.graphicalDesktopEnvironment
                    |> Helpers.String.capitalizeWord
                , "works for"
                , context.localization.staticRepresentationOfBlockDeviceContents
                    |> Helpers.String.pluralize
                , "based on Ubuntu (20.04 or newer), Rocky Linux, or AlmaLinux. If you selected a different operating system, it may not work. Also, if selected"
                , context.localization.staticRepresentationOfBlockDeviceContents
                , "does not have a desktop environment pre-installed,"
                , context.localization.virtualComputer
                , "may take a long time to deploy."
                ]

        cloudSpecificMessage : Maybe String
        cloudSpecificMessage =
            GetterSetters.cloudSpecificConfigLookup context.cloudSpecificConfigs project
                |> Maybe.andThen .desktopMessage

        imageSpecificMessage : Maybe String
        imageSpecificMessage =
            GetterSetters.imageLookup project model.imageUuid
                |> Maybe.andThen GetterSetters.imageGetDesktopMessage

        messages : List (Element.Element Msg)
        messages =
            [ -- Prefer image-specific message, failing that show a cloud-specific message, failing that show a generic message
              (case imageSpecificMessage of
                Just "" ->
                    -- Empty string, cloud operator wants to hide message entirely
                    Nothing

                Just message ->
                    Just message

                Nothing ->
                    case cloudSpecificMessage of
                        Just "" ->
                            -- Empty string, cloud operator wants to hide message entirely
                            Nothing

                        Just message ->
                            Just message

                        Nothing ->
                            Just genericMessage
              )
                |> Maybe.map Element.text
            , let
                warningMaxGB =
                    12

                rootDiskWarnText =
                    String.join " "
                        [ "Warning: root disk may be too small for a graphical desktop environment. Please select a"
                        , context.localization.virtualComputerHardwareConfig
                        , "with a"
                        , String.fromInt warningMaxGB
                        , "GB or larger root disk, or select a volume-backed root disk at least"
                        , String.fromInt warningMaxGB
                        , "GB in size."
                        ]
              in
              case model.volSizeTextInput of
                Nothing ->
                    case model.flavorId |> Maybe.andThen (GetterSetters.flavorLookup project) of
                        Just flavor ->
                            if flavor.disk_root < warningMaxGB then
                                Just <| Element.text rootDiskWarnText

                            else
                                Nothing

                        Nothing ->
                            Nothing

                Just numericTextInput ->
                    case numericTextInput of
                        ValidNumericTextInput rootVolSize ->
                            if rootVolSize < warningMaxGB then
                                Just <| Element.text rootDiskWarnText

                            else
                                Nothing

                        _ ->
                            Nothing
            ]
                |> List.filterMap identity
    in
    Element.column [ Element.spacing spacer.px12 ]
        [ Input.radioRow [ Element.spacing spacer.px32 ]
            { label =
                Input.labelAbove
                    (VH.radioLabelAttributes True)
                    (Element.text <|
                        String.concat
                            [ "Enable "
                            , context.localization.graphicalDesktopEnvironment
                            , "?"
                            ]
                    )
            , onChange = GotDeployDesktopEnvironment
            , options =
                [ Input.option False (Element.text "No")
                , Input.option True (Element.text "Yes")
                ]
            , selected = Just model.deployDesktopEnvironment
            }
        , if model.deployDesktopEnvironment && not (List.isEmpty messages) then
            Alert.alert []
                context.palette
                { state = Alert.Info
                , showIcon = False
                , showContainer = True
                , content =
                    Element.column
                        [ Element.spacing spacer.px12, Element.width Element.fill ]
                        (List.map (\message -> Element.paragraph [] [ message ]) messages)
                }

          else
            Element.none
        ]


guacamolePicker : View.Types.Context -> Model -> Element.Element Msg
guacamolePicker context model =
    case model.deployGuacamole of
        Nothing ->
            Element.text <|
                String.concat
                    [ "Guacamole deployment is not supported for this "
                    , context.localization.openstackWithOwnKeystone
                    , "."
                    ]

        Just deployGuacamole ->
            Input.radioRow [ Element.spacing spacer.px32 ]
                { label =
                    Input.labelAbove
                        (VH.radioLabelAttributes True)
                        (Element.text "Deploy Guacamole for easy remote access?")
                , onChange = \new -> GotDeployGuacamole <| Just new
                , options =
                    [ Input.option True (Element.text "Yes")
                    , Input.option False (Element.text "No")

                    {- -}
                    ]
                , selected = Just deployGuacamole
                }


skipOperatingSystemUpdatesPicker : View.Types.Context -> Model -> Element.Element Msg
skipOperatingSystemUpdatesPicker context model =
    Element.column [ Element.spacing spacer.px12 ]
        [ Input.radioRow [ Element.spacing spacer.px32 ]
            { label =
                Input.labelAbove
                    (VH.radioLabelAttributes True)
                    (Element.text "Install operating system updates?")
            , onChange = GotInstallOperatingSystemUpdates
            , options =
                [ Input.option True (Element.text "Yes")
                , Input.option False (Element.text "No")

                {- -}
                ]
            , selected = Just model.installOperatingSystemUpdates
            }
        , if not model.installOperatingSystemUpdates then
            Alert.alert []
                context.palette
                { state = Alert.Warning
                , showIcon = False
                , showContainer = True
                , content =
                    Element.paragraph []
                        [ Element.text <|
                            String.concat
                                [ "Warning: Skipping operating system updates is a security risk, especially when launching "
                                , Helpers.String.indefiniteArticle context.localization.virtualComputer
                                , " "
                                , context.localization.virtualComputer
                                , " from an older "
                                , context.localization.staticRepresentationOfBlockDeviceContents
                                , ". Do not use this "
                                , context.localization.virtualComputer
                                , " for any sensitive information or workloads."
                                ]
                        ]
                }

          else
            Element.none
        ]


networkPicker : View.Types.Context -> Project -> Model -> Element.Element Msg
networkPicker context project model =
    let
        networkOptions =
            Helpers.newServerNetworkOptions project

        guidance =
            let
                maybeStr =
                    if networkOptions == ManualNetworkSelection && model.networkUuid == Nothing then
                        Just "Please choose a network."

                    else
                        Just "Please only change this if you know what you are doing."
            in
            case maybeStr of
                Just str ->
                    Element.paragraph
                        [ Font.color (context.palette.danger.textOnNeutralBG |> SH.toElementColor) ]
                        [ Element.text str ]

                Nothing ->
                    Element.none

        picker =
            let
                networkAsInputOption network =
                    ( network.uuid, network.name )
            in
            Style.Widgets.Select.select
                []
                context.palette
                { label = "Choose a Network"
                , onChange = \networkUuid -> GotNetworkUuid networkUuid
                , options =
                    case project.networks.data of
                        RDPP.DoHave networks _ ->
                            List.map networkAsInputOption networks

                        RDPP.DontHave ->
                            []
                , selected = model.networkUuid
                }
    in
    Element.column
        [ Element.spacing spacer.px12 ]
        [ VH.requiredLabel context.palette
            (Element.el [ Font.semiBold ] <| Element.text "Network")
        , guidance
        , picker
        ]


floatingIpPicker : View.Types.Context -> Project -> Model -> Element.Element Msg
floatingIpPicker context project model =
    let
        optionPicker =
            let
                options =
                    [ Input.option Automatic (Element.text "Automatic")
                    , Input.option (UseFloatingIp CreateNewFloatingIp Unknown)
                        (Element.text <|
                            String.join " "
                                [ "Assign a"
                                , context.localization.floatingIpAddress
                                , "to this"
                                , context.localization.virtualComputer
                                ]
                        )
                    , Input.option DoNotUseFloatingIp
                        (Element.text <|
                            String.join " "
                                [ "Do not create or assign a"
                                , context.localization.floatingIpAddress
                                ]
                        )
                    ]
            in
            Input.radio [ Element.spacing spacer.px4 ]
                { label =
                    Input.labelHidden <|
                        String.join " "
                            [ "Choose a"
                            , context.localization.floatingIpAddress
                            , "option"
                            ]
                , onChange = GotFloatingIpCreationOption
                , options =
                    options
                , selected = Just model.floatingIpCreationOption
                }

        reuseOptionPicker =
            case model.floatingIpCreationOption of
                UseFloatingIp reuseOption _ ->
                    let
                        unassignedFloatingIpOptions =
                            project.floatingIps
                                |> RDPP.withDefault []
                                |> List.filter (\ip -> ip.portUuid == Nothing)
                                |> List.map
                                    (\ip ->
                                        Input.option
                                            (UseExistingFloatingIp ip.uuid)
                                            (Element.text <| String.join " " [ "Use existing", ip.address ])
                                    )

                        options =
                            List.concat
                                [ [ Input.option
                                        CreateNewFloatingIp
                                        (Element.text <|
                                            String.join " " [ "Create a new", context.localization.floatingIpAddress ]
                                        )
                                  ]
                                , unassignedFloatingIpOptions
                                ]
                    in
                    Element.column
                        [ Element.spacing spacer.px12 ]
                        [ Element.el [ Font.semiBold ] <|
                            Element.text <|
                                String.join " "
                                    [ Helpers.String.toTitleCase context.localization.floatingIpAddress
                                    , "Reuse Option"
                                    ]
                        , Input.radio [ Element.spacing spacer.px4 ]
                            { label =
                                Input.labelHidden <|
                                    String.join " "
                                        [ "Choose whether to create a new"
                                        , context.localization.floatingIpAddress
                                        , "or re-use an existing one"
                                        ]
                            , onChange = \option -> GotFloatingIpCreationOption <| UseFloatingIp option Unknown
                            , options = options
                            , selected =
                                Just reuseOption
                            }
                        ]

                _ ->
                    Element.none
    in
    Element.column [ Element.spacing spacer.px24 ]
        [ Element.column
            [ Element.spacing spacer.px12 ]
            [ Element.el [ Font.semiBold ] <|
                Element.text <|
                    Helpers.String.toTitleCase context.localization.floatingIpAddress
            , optionPicker
            ]
        , reuseOptionPicker
        ]


keypairPicker : View.Types.Context -> Project -> Model -> Element.Element Msg
keypairPicker context project model =
    let
        keypairAsOption keypair =
            Input.option (Just keypair.name) (Element.text keypair.name)

        noneOption =
            Input.option Nothing (Element.text "None")

        renderKeypairs keypairs =
            if List.isEmpty keypairs then
                Text.p []
                    [ Element.text <|
                        String.concat
                            [ "(This "
                            , context.localization.unitOfTenancy
                            , " has no "
                            , context.localization.pkiPublicKeyForSsh
                                |> Helpers.String.pluralize
                            , " to choose from, but you can still create "
                            , Helpers.String.indefiniteArticle context.localization.virtualComputer
                            , " "
                            , context.localization.virtualComputer
                            , "!)"
                            ]
                    ]

            else
                Input.radio []
                    { label =
                        Input.labelAbove
                            [ Element.paddingXY 0 spacer.px12 ]
                            (Element.text <|
                                String.join " "
                                    [ "Choose"
                                    , Helpers.String.indefiniteArticle context.localization.pkiPublicKeyForSsh
                                    , context.localization.pkiPublicKeyForSsh
                                    , "(this is optional, skip if unsure)"
                                    ]
                            )
                    , onChange = \keypairName -> GotKeypairName <| keypairName
                    , options = noneOption :: List.map keypairAsOption keypairs
                    , selected = Just model.keypairName
                    }
    in
    Element.column
        [ Element.spacing spacer.px12 ]
        [ Element.el
            [ Font.semiBold ]
            (Element.text
                (Helpers.String.toTitleCase context.localization.pkiPublicKeyForSsh)
            )
        , VH.renderWebData
            context
            project.keypairs
            (Helpers.String.pluralize context.localization.pkiPublicKeyForSsh)
            renderKeypairs
        , let
            text =
                String.concat [ "Upload a new ", context.localization.pkiPublicKeyForSsh ]
          in
          Element.link []
            { url = Route.toUrl context.urlPathPrefix (Route.ProjectRoute (GetterSetters.projectIdentifier project) <| Route.KeypairCreate)
            , label =
                Widget.iconButton
                    (SH.materialStyle context.palette).button
                    { text = text
                    , icon =
                        Element.row
                            [ Element.spacing spacer.px4 ]
                            [ Element.text text
                            , Element.el []
                                (FeatherIcons.chevronRight
                                    |> FeatherIcons.toHtml []
                                    |> Element.html
                                )
                            ]
                    , onPress =
                        Just <| NoOp
                    }
            }
        ]


userDataInput : View.Types.Context -> Model -> Element.Element Msg
userDataInput context model =
    Element.column
        [ Element.spacing spacer.px12 ]
        [ Element.el
            [ Font.semiBold ]
            (Element.text
                (Helpers.String.toTitleCase context.localization.cloudInitData)
            )
        , Input.multiline
            (VH.inputItemAttributes context.palette
                ++ [ Element.width Element.fill
                   , Element.height (Element.px 500)
                   , Element.spacing spacer.px4
                   , Font.family [ Font.monospace ]
                   ]
            )
            { onChange = GotUserDataTemplate
            , text = model.userDataTemplate
            , placeholder =
                Just
                    (Input.placeholder []
                        (Element.text <|
                            String.join
                                " "
                                [ "#!/bin/bash\n\n# Your"
                                , context.localization.cloudInitData
                                , "here"
                                ]
                        )
                    )
            , label =
                Input.labelHidden <| Helpers.String.toTitleCase context.localization.cloudInitData
            , spellcheck = False
            }
        ]
