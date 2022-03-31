module Page.ServerCreate exposing (Model, Msg(..), init, update, view)

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
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Maybe
import OpenStack.Quotas as OSQuotas
import OpenStack.ServerNameValidator exposing (serverNameValidator)
import OpenStack.Types as OSTypes
import RemoteData
import Route
import ServerDeploy exposing (cloudInitUserDataTemplate)
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.Card exposing (badge)
import Style.Widgets.NumericTextInput.NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Style.Widgets.Select
import Style.Widgets.ToggleTip
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
    | GotCount Int
    | GotSelectedFlavorGroupToggleTip (Maybe HelperTypes.FlavorGroupTitle)
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
    | GotShowWorkFlowExplanationToggleTip
    | GotCreateCluster Bool
    | GotDisabledCreateButtonPressed
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : OSTypes.ImageUuid -> String -> Maybe (List OSTypes.FlavorId) -> Maybe Bool -> Model
init imageUuid imageName restrictFlavorIds deployGuacamole =
    { serverName = imageName
    , imageUuid = imageUuid
    , imageName = imageName
    , restrictFlavorIds = restrictFlavorIds
    , count = 1
    , selectedFlavorGroupToggleTip = Nothing
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
    , showWorkflowExplanationToggleTip = False
    , createCluster = False
    , showClusterExplanationToggleTip = False
    , showFormInvalidToggleTip = False
    }


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotServerName name ->
            ( { model | serverName = name }, Cmd.none, SharedMsg.NoOp )

        GotCount count ->
            ( enforceQuotaCompliance project { model | count = count }, Cmd.none, SharedMsg.NoOp )

        GotSelectedFlavorGroupToggleTip maybeFlavorGroupTitle ->
            let
                newModel =
                    { model
                        | selectedFlavorGroupToggleTip = maybeFlavorGroupTitle
                    }
            in
            ( newModel
            , Cmd.none
            , SharedMsg.NoOp
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

        GotShowWorkFlowExplanationToggleTip ->
            ( { model
                | showWorkflowExplanationToggleTip = not model.showWorkflowExplanationToggleTip
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

        GotDisabledCreateButtonPressed ->
            ( { model
                | showFormInvalidToggleTip = not model.showFormInvalidToggleTip
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


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        invalidNameReasons =
            serverNameValidator (Just context.localization.virtualComputer) model.serverName

        serverNameValidationStatusAttributes =
            case invalidNameReasons of
                Nothing ->
                    VH.validInputAttributes context.palette

                Just _ ->
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
                        [ Font.color (SH.toElementColor context.palette.error)
                        , Element.alignRight
                        ]
                        [ Element.text guidanceText
                        ]

        ( createOnPress, maybeInvalidFormReasons ) =
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
                    invalidVolSizeTextInput || invalidWorkflowTextInput
            in
            case ( invalidNameReasons, invalidInputs ) of
                ( Nothing, False ) ->
                    case ( model.networkUuid, model.flavorId ) of
                        ( Just netUuid, Just flavorId ) ->
                            ( Just <| SharedMsg (SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) (SharedMsg.RequestCreateServer model netUuid flavorId))
                            , Nothing
                            )

                        ( _, _ ) ->
                            let
                                invalidNetworkReason =
                                    if model.networkUuid == Nothing then
                                        [ "Choose a network" ]

                                    else
                                        []

                                invalidFlavorReason =
                                    if model.flavorId == Nothing then
                                        [ "Choose a flavor" ]

                                    else
                                        []

                                invalidFormReasons =
                                    invalidNetworkReason
                                        ++ invalidFlavorReason
                            in
                            ( Just GotDisabledCreateButtonPressed, Just invalidFormReasons )

                ( _, _ ) ->
                    let
                        invalidNameFormReason =
                            if invalidNameReasons == Nothing then
                                []

                            else
                                [ "Enter a valid " ++ context.localization.virtualComputer ++ " name" ]

                        invalidVolSizeReason =
                            if invalidVolSizeTextInput then
                                [ "Enter valid custom root disk size" ]

                            else
                                []

                        invalidWorkflowReason =
                            if invalidWorkflowTextInput then
                                [ "Enter a valid workflow repository" ]

                            else
                                []

                        invalidFormReasons =
                            invalidNameFormReason
                                ++ invalidVolSizeReason
                                ++ invalidWorkflowReason
                    in
                    ( Just GotDisabledCreateButtonPressed, Just invalidFormReasons )

        createButton =
            case maybeInvalidFormReasons of
                Nothing ->
                    Button.primary
                        context.palette
                        { text = "Create"
                        , onPress = createOnPress
                        }

                Just _ ->
                    let
                        formInvalidHintView =
                            Element.column
                                (SH.popoverStyleDefaults context.palette
                                    ++ [ Element.width
                                            (Element.fill
                                                |> Element.minimum 100
                                            )
                                       ]
                                )
                                [ Element.column
                                    [ Element.spacing 10
                                    ]
                                    (maybeInvalidFormReasons
                                        |> Maybe.withDefault [ "Please correct problems with the form" ]
                                        |> List.map (VH.invalidInputHelperText context.palette)
                                    )
                                ]
                    in
                    Element.el
                        (if model.showFormInvalidToggleTip then
                            SH.popoverAttribs formInvalidHintView ST.PositionTopRight (Just 8)

                         else
                            []
                        )
                        (Widget.button
                            (SH.materialStyle context.palette).warningButton
                            { text = "Create"
                            , icon =
                                FeatherIcons.alertTriangle
                                    |> FeatherIcons.withSize 20
                                    |> FeatherIcons.toHtml []
                                    |> Element.html
                                    |> Element.el [ Element.paddingEach { edges | right = 5 } ]
                            , onPress = createOnPress
                            }
                        )

        contents flavor computeQuota volumeQuota =
            [ Element.column
                [ Element.spacing 10
                , Element.width Element.fill
                ]
                (Input.text
                    (VH.inputItemAttributes context.palette.background
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
                    :: renderInvalidNameReasons
                )
            , Element.row []
                [ Element.text <|
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
                model.selectedFlavorGroupToggleTip
                GotSelectedFlavorGroupToggleTip
                Nothing
                model.flavorId
                GotFlavorId
            , volBackedPrompt context model volumeQuota flavor
            , countPicker context model computeQuota volumeQuota flavor
            , desktopEnvironmentPicker context project model
            , customWorkflowInput context model
            , Element.column
                [ Element.spacing 24 ]
              <|
                [ Input.radioRow [ Element.spacing 10 ]
                    { label = Input.labelAbove [ Element.paddingXY 0 12, Font.bold ] (Element.text "Advanced Options")
                    , onChange = GotShowAdvancedOptions
                    , options =
                        [ Input.option False (Element.text "Hide")
                        , Input.option True (Element.text "Show")

                        {- -}
                        ]
                    , selected = Just model.showAdvancedOptions
                    }
                ]
                    ++ (if not model.showAdvancedOptions then
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
            , Element.el [ Element.alignRight ] <|
                createButton
            ]

        loading =
            [ Element.row [ Element.spacing 15 ]
                [ Widget.circularProgressIndicator
                    (SH.materialStyle context.palette).progressIndicator
                    Nothing
                , Element.text "Loading..."
                ]
            ]
    in
    Element.column
        [ Element.spacing 24
        , Element.padding 10
        , Element.width Element.fill
        ]
    <|
        [ Element.el
            (VH.heading2 context.palette)
            (Element.text <|
                String.join " "
                    [ "Create"
                    , context.localization.virtualComputer
                        |> Helpers.String.toTitleCase
                    ]
            )
        , Element.column
            [ -- Keeps form fields from displaying too wide
              Element.width (Element.maximum 600 Element.fill)

            -- PatternFly guidelines: There should be 24 pixels between field inside a form (spacing)
            , Element.spacing 24

            -- PatternFly guidelines: There should be 24 pixels between a form and it surroundings (padding)
            -- It's set to 4 here, because the form is probably already inside two nested elements each with padding of 10
            , Element.paddingEach { edges | left = 4 }
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
                    loading

                ( _, RemoteData.Loading, _ ) ->
                    loading

                ( _, _, _ ) ->
                    loading
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
            Input.radio []
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
    Element.column [ Element.spacing 10 ]
        [ Element.text "Choose a root disk size"
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
                Element.row [ Element.spacing 10 ]
                    [ numericTextInput
                        context.palette
                        (VH.inputItemAttributes context.palette.background)
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

        countAvail =
            OSQuotas.overallQuotaAvailServers
                (model.volSizeTextInput
                    |> Maybe.andThen Style.Widgets.NumericTextInput.NumericTextInput.toMaybe
                )
                flavor
                computeQuota
                volumeQuota
    in
    Element.column [ Element.spacing 10 ]
        [ Element.text <|
            String.concat
                [ "How many "
                , context.localization.virtualComputer
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                , "?"
                ]
        , case countAvail of
            Just countAvail_ ->
                Element.text <|
                    String.join " "
                        [ "Your"
                        , context.localization.maxResourcesPerProject
                        , "supports up to"
                        , humanCount locale countAvail_
                        , "of these."
                        ]

            Nothing ->
                Element.none
        , Element.row [ Element.spacing 10 ]
            [ Input.slider
                [ Element.height (Element.px 30)
                , Element.width (Element.px 100 |> Element.minimum 200)

                -- Here is where we're creating/styling the "track"
                , Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Background.color (SH.toElementColor context.palette.on.background)
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = \c -> GotCount <| round c
                , label = Input.labelHidden "How many?"
                , min = 1
                , max = countAvail |> Maybe.withDefault 20 |> toFloat
                , step = Just 1
                , value = toFloat model.count
                , thumb =
                    Input.defaultThumb
                }
            , Element.el
                [ Element.width Element.shrink ]
                (Element.text (humanCount locale model.count))
            , case countAvail of
                Just countAvail_ ->
                    if model.count == countAvail_ then
                        Element.text ("(" ++ context.localization.maxResourcesPerProject ++ " max)")

                    else
                        Element.none

                Nothing ->
                    Element.none
            ]
        ]


customWorkflowInput : View.Types.Context -> Model -> Element.Element Msg
customWorkflowInput context model =
    if context.experimentalFeaturesEnabled then
        customWorkflowInputExperimental context model

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


customWorkflowInputExperimental : View.Types.Context -> Model -> Element.Element Msg
customWorkflowInputExperimental context model =
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
                    Element.column [ Element.width Element.fill, Element.spacing 10 ]
                        [ Input.text
                            (Events.onLoseFocus GotWorkflowInputLoseFocus
                                :: (VH.inputItemAttributes context.palette.background
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
                        (VH.inputItemAttributes context.palette.background
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
                        , Element.spacing 10
                        ]
                        [ Element.text pathInputLabel
                        , Input.text
                            (VH.inputItemAttributes context.palette.background
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
                , Element.spacing 24
                ]
                [ repoInput
                , referenceInput
                , sourcePathInput
                ]

        workflowExplanationToggleTip =
            Style.Widgets.ToggleTip.toggleTip
                context.palette
                (Element.column
                    [ Element.width
                        (Element.fill
                            |> Element.minimum 100
                        )
                    , Element.spacing 7
                    ]
                    [ Element.text "Any Binder™-compatible repository can be launched."
                    , Element.paragraph []
                        [ Element.text "See mybinder.org for more information"
                        ]
                    ]
                )
                ST.PositionTop
                model.showWorkflowExplanationToggleTip
                GotShowWorkFlowExplanationToggleTip

        experimentalBadge =
            badge "Experimental"
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 24
        ]
    <|
        (Input.radioRow [ Element.spacing 10 ]
            { label =
                Input.labelAbove [ Element.paddingXY 0 12 ]
                    (Element.row [ Element.spacingXY 10 0 ]
                        [ Element.el
                            (VH.heading4 ++ [ Font.size 17 ])
                            (Element.text ("Launch a workflow in the " ++ context.localization.virtualComputer))
                        , experimentalBadge
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

        experimentalBadge =
            badge "Experimental"
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 24
        ]
        [ Input.radioRow [ Element.spacing 10 ]
            { label =
                Input.labelAbove [ Element.paddingXY 0 12 ]
                    (Element.row [ Element.spacingXY 10 0 ]
                        [ Element.el
                            (VH.heading4 ++ [ Font.size 17 ])
                            (Element.text ("Create your own SLURM cluster with this " ++ context.localization.virtualComputer ++ " as the head node"))
                        , experimentalBadge
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
            Element.column
                ([ Background.color (SH.toElementColor context.palette.warn), Font.color (SH.toElementColor context.palette.on.warn) ]
                    ++ VH.exoElementAttributes
                )
                (List.map (\warning -> Element.paragraph [] [ warning ]) warnings)

          else
            Element.none
        ]


desktopEnvironmentPicker : View.Types.Context -> Project -> Model -> Element.Element Msg
desktopEnvironmentPicker context project model =
    let
        warnings : List (Element.Element Msg)
        warnings =
            [ Just <|
                Element.text <|
                    String.concat
                        [ "Warning: this feature currently only supports "
                        , context.localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.pluralize
                        , " based on Ubuntu 20.04, Rocky Linux, or AlmaLinux. Support for other operating systems is coming soon, but if you choose "
                        , context.localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.indefiniteArticle
                        , " "
                        , context.localization.staticRepresentationOfBlockDeviceContents
                        , " based on a different operating system now, it is unlikely to work."
                        ]
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
            , if model.deployDesktopEnvironment then
                Just <|
                    Element.text <|
                        String.join " "
                            [ "Warning: If selected"
                            , context.localization.staticRepresentationOfBlockDeviceContents
                            , "does not already include a graphical desktop environment,"
                            , context.localization.virtualComputer
                            , "can take 30 minutes or longer to deploy."
                            ]

              else
                Nothing
            ]
                |> List.filterMap identity
    in
    Element.column [ Element.spacing 10 ]
        [ Input.radioRow VH.exoElementAttributes
            { label =
                Input.labelAbove [ Element.paddingXY 0 12, Font.bold ]
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
        , if model.deployDesktopEnvironment then
            Element.column
                ([ Background.color (SH.toElementColor context.palette.warn), Font.color (SH.toElementColor context.palette.on.warn) ]
                    ++ VH.exoElementAttributes
                )
                (List.map (\warning -> Element.paragraph [] [ warning ]) warnings)

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
            Input.radioRow [ Element.spacing 10 ]
                { label = Input.labelAbove [ Element.paddingXY 0 12, Font.bold ] (Element.text "Deploy Guacamole for easy remote access?")
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
    Element.column [ Element.spacing 10 ]
        [ Input.radioRow [ Element.spacing 10 ]
            { label = Input.labelAbove [ Element.paddingXY 0 12, Font.bold ] (Element.text "Install operating system updates?")
            , onChange = GotInstallOperatingSystemUpdates
            , options =
                [ Input.option True (Element.text "Yes")
                , Input.option False (Element.text "No")

                {- -}
                ]
            , selected = Just model.installOperatingSystemUpdates
            }
        , if not model.installOperatingSystemUpdates then
            Element.paragraph
                ([ Background.color (SH.toElementColor context.palette.warn), Font.color (SH.toElementColor context.palette.on.warn) ]
                    ++ VH.exoElementAttributes
                )
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
                        [ Font.color (context.palette.error |> SH.toElementColor) ]
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
        [ Element.spacing 10 ]
        [ VH.requiredLabel context.palette
            (Element.el [ Font.bold ] <| Element.text "Network")
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
            Input.radio []
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
                        [ Element.paddingXY 0 10, Element.spacingXY 0 10 ]
                        [ Element.el [ Font.bold ] <|
                            Element.text <|
                                String.join " "
                                    [ Helpers.String.toTitleCase context.localization.floatingIpAddress
                                    , "Reuse Option"
                                    ]
                        , Input.radio []
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
    Element.column
        [ Element.spacing 10 ]
        [ Element.el [ Font.bold ] <|
            Element.text <|
                Helpers.String.toTitleCase context.localization.floatingIpAddress
        , optionPicker
        , reuseOptionPicker
        ]


keypairPicker : View.Types.Context -> Project -> Model -> Element.Element Msg
keypairPicker context project model =
    let
        keypairAsOption keypair =
            Input.option keypair.name (Element.text keypair.name)

        renderKeypairs keypairs =
            if List.isEmpty keypairs then
                Element.text <|
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

            else
                Input.radio []
                    { label =
                        Input.labelAbove
                            [ Element.paddingXY 0 12 ]
                            (Element.text <|
                                String.join " "
                                    [ "Choose"
                                    , Helpers.String.indefiniteArticle context.localization.pkiPublicKeyForSsh
                                    , context.localization.pkiPublicKeyForSsh
                                    , "(this is optional, skip if unsure)"
                                    ]
                            )
                    , onChange = \keypairName -> GotKeypairName <| Just keypairName
                    , options = List.map keypairAsOption keypairs
                    , selected = Just (Maybe.withDefault "" model.keypairName)
                    }
    in
    Element.column
        [ Element.spacing 10 ]
        [ Element.el
            [ Font.bold ]
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
                            [ Element.spacing 5 ]
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
        [ Element.spacing 10 ]
        [ Element.el
            [ Font.bold ]
            (Element.text
                (Helpers.String.toTitleCase context.localization.cloudInitData)
            )
        , Input.multiline
            (VH.inputItemAttributes context.palette.background
                ++ [ Element.width Element.fill
                   , Element.height (Element.px 500)
                   , Element.spacing 3
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
