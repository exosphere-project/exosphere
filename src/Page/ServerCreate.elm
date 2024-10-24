module Page.ServerCreate exposing (Model, Msg(..), init, update, view)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icons
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (humanCount)
import Helpers.GetterSetters as GetterSetters exposing (isDefaultSecurityGroup)
import Helpers.Helpers as Helpers
import Helpers.Random as RandomHelper
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Helpers.Units
import Helpers.Validation as Validation
import Helpers.ValidationResult
import Html.Attributes
import Maybe
import OpenStack.Quotas as OSQuotas
import OpenStack.ServerNameValidator exposing (serverNameValidator)
import OpenStack.Types as OSTypes exposing (securityGroupExoTags, securityGroupTaggedAs)
import Page.SecurityGroupRulesTable as SecurityGroupRulesTable
import Rest.Naming
import Route
import ServerDeploy exposing (cloudInitUserDataTemplate)
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Alert as Alert
import Style.Widgets.Button as Button
import Style.Widgets.CopyableText exposing (copyableTextAccessory)
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Link as Link
import Style.Widgets.NumericTextInput.NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Style.Widgets.Select
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Tag as Tag
import Style.Widgets.Text as Text exposing (TextVariant(..))
import Style.Widgets.ToggleTip
import Style.Widgets.Validation exposing (invalidMessage)
import Time
import Types.Error exposing (HttpErrorWithBody)
import Types.HelperTypes as HelperTypes
    exposing
        ( FloatingIpAssignmentStatus(..)
        , FloatingIpOption(..)
        , FloatingIpReuseOption(..)
        , SelectedFlavor(..)
        )
import Types.Project exposing (Project)
import Types.Server exposing (NewServerNetworkOptions(..), Server)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Forms as Forms exposing (Resource(..))
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
    | GotVolSizeTextInput (Maybe NumericTextInput)
    | GotUserDataTemplate String
    | GotNetworks
    | GotNetworkUuid (Maybe OSTypes.NetworkUuid)
    | GotAutoAllocatedNetwork OSTypes.NetworkUuid
    | GotShowAdvancedOptions Bool
    | GotKeypairName (Maybe String)
    | GotSecurityGroups (List OSTypes.SecurityGroup)
    | GotSecurityGroupUuid (Maybe OSTypes.SecurityGroupUuid)
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


init : View.Types.Context -> Project -> OSTypes.ImageUuid -> String -> Maybe (List OSTypes.FlavorId) -> Maybe Bool -> Model
init context project imageUuid imageName restrictFlavorIds deployGuacamole =
    { serverName = ""
    , imageUuid = imageUuid
    , imageName = imageName
    , restrictFlavorIds = restrictFlavorIds
    , count = 1
    , selectedFlavor = DefaultFlavorSelection
    , volSizeTextInput = Nothing
    , userDataTemplate = cloudInitUserDataTemplate
    , networkUuid = Nothing
    , showAdvancedOptions = False
    , keypairName = initialKeypairName project
    , securityGroupUuid =
        case project.securityGroups.data of
            RDPP.DoHave securityGroups _ ->
                securityGroups
                    |> List.filter (isDefaultSecurityGroup context project)
                    |> List.head
                    |> Maybe.map .uuid

            _ ->
                Nothing
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


initialKeypairName : Project -> Maybe OSTypes.KeypairName
initialKeypairName project =
    let
        projectKeypairNames : List OSTypes.KeypairName
        projectKeypairNames =
            project.keypairs |> RDPP.withDefault [] |> List.map .name

        keypairNameOfNewestServerCreatedByUser =
            let
                newestServerCreatedByUser : Maybe Server
                newestServerCreatedByUser =
                    let
                        serverSorter : Server -> Int
                        serverSorter s =
                            s.osProps.details.created |> Time.posixToMillis
                    in
                    project.servers
                        |> RDPP.withDefault []
                        |> List.filter
                            (\s ->
                                GetterSetters.serverCreatedByCurrentUser project s.osProps.uuid
                                    |> Maybe.withDefault False
                            )
                        |> List.sortBy serverSorter
                        |> List.head

                maybeKn : Maybe OSTypes.KeypairName
                maybeKn =
                    newestServerCreatedByUser
                        |> Maybe.andThen (\s -> s.osProps.details.keypairName)
            in
            maybeKn
                -- Ensure there is actually a keypair with this name
                -- (i.e. that the user didn't delete it since creating the server)
                |> Maybe.andThen
                    (\kn ->
                        if List.member kn projectKeypairNames then
                            Just kn

                        else
                            Nothing
                    )

        anyKeypairNameBelongingToUser =
            projectKeypairNames
                |> List.head
    in
    -- Use the first of these which resolves to `Just` a keypair name.
    [ keypairNameOfNewestServerCreatedByUser, anyKeypairNameBelongingToUser ]
        |> List.filterMap identity
        |> List.head


getAllowedFlavors : Model -> List OSTypes.Flavor -> List OSTypes.Flavor
getAllowedFlavors model projectFlavors =
    let
        allowedFlavors : List OSTypes.Flavor
        allowedFlavors =
            case model.restrictFlavorIds of
                Nothing ->
                    projectFlavors

                Just restrictedFlavorIds ->
                    List.filterMap
                        (\flavor ->
                            if List.member flavor.id restrictedFlavorIds then
                                Nothing

                            else
                                Just flavor
                        )
                        projectFlavors
    in
    GetterSetters.sortedFlavors allowedFlavors


update : Msg -> SharedModel -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg { viewContext } project model =
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
            case GetterSetters.flavorLookup project flavorId of
                Nothing ->
                    -- Should we indicate an error here? How?
                    ( model, Cmd.none, SharedMsg.NoOp )

                Just flavor ->
                    ( enforceQuotaCompliance project { model | selectedFlavor = WithSelectedFlavor flavor }, Cmd.none, SharedMsg.NoOp )

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

        GotSecurityGroupUuid maybeSecurityGroupUuid ->
            ( { model | securityGroupUuid = maybeSecurityGroupUuid }, Cmd.none, SharedMsg.NoOp )

        GotSecurityGroups securityGroups ->
            -- SharedModel just updated with new security groups. If we don't have anything selected yet, choose the default.
            if model.securityGroupUuid == Nothing then
                let
                    defaultSecurityGroupUuid =
                        securityGroups
                            |> List.filter (isDefaultSecurityGroup viewContext project)
                            |> List.head
                            |> Maybe.map .uuid
                in
                ( { model | securityGroupUuid = defaultSecurityGroupUuid }, Cmd.none, SharedMsg.NoOp )

            else
                ( model, Cmd.none, SharedMsg.NoOp )

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


getSelectedFlavorMaybe : Model -> RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.Flavor) -> Maybe OSTypes.Flavor
getSelectedFlavorMaybe model projectFlavors =
    case projectFlavors.data of
        RDPP.DontHave ->
            Nothing

        RDPP.DoHave flavors _ ->
            case getAllowedFlavors model flavors of
                [] ->
                    Nothing

                smallestFlavor :: _ ->
                    Just <| getSelectedFlavor model.selectedFlavor smallestFlavor


getSelectedFlavor : SelectedFlavor -> OSTypes.Flavor -> OSTypes.Flavor
getSelectedFlavor flavorSelection smallestFlavor =
    case flavorSelection of
        WithSelectedFlavor flavor ->
            flavor

        DefaultFlavorSelection ->
            smallestFlavor


enforceQuotaCompliance : Project -> Model -> Model
enforceQuotaCompliance project model =
    -- If user is trying to choose a combination of flavor, volume-backed disk size, and count
    -- that would exceed quota, reduce count to comply with quota.
    case
        ( getSelectedFlavorMaybe model project.flavors
        , project.computeQuota.data
        , project.volumeQuota.data
        )
    of
        ( Just flavor, RDPP.DoHave computeQuota _, RDPP.DoHave volumeQuota _ ) ->
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
            Validation.serverNameExists project model.serverName

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
                    List.map (invalidMessage context.palette) reasons

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
                            , "Please contact your " ++ context.localization.openstackWithOwnKeystone ++ " administrator."
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

        contents : OSTypes.Flavor -> OSTypes.ComputeQuota -> OSTypes.VolumeQuota -> List (Element.Element Msg)
        contents flavor computeQuota volumeQuota =
            let
                canBeLaunched quota fl =
                    OSQuotas.computeQuotaFlavorAvailServers quota fl
                        |> Maybe.map (\count -> count >= 1)
                        |> Maybe.withDefault False

                flavorAvailability : List Bool
                flavorAvailability =
                    model.restrictFlavorIds
                        |> Maybe.map (List.filterMap (GetterSetters.flavorLookup project))
                        |> Maybe.withDefault (RDPP.withDefault [] project.flavors)
                        |> List.map (canBeLaunched computeQuota)

                hasAvailableResources =
                    List.any identity flavorAvailability

                invalidVolSizeTextInput =
                    case model.volSizeTextInput of
                        Just (InvalidNumericTextInput _) ->
                            True

                        _ ->
                            False

                invalidWorkflowTextInput =
                    model.workflowInputRepository == "" && model.workflowInputIsValid == Just False

                invalidInputs =
                    invalidVolSizeTextInput || invalidWorkflowTextInput || not hasAvailableResources || (compareDiskSize project model |> Helpers.ValidationResult.isInvalid)

                ( createOnPress, maybeInvalidFormFields ) =
                    case ( invalidNameReasons, invalidInputs ) of
                        ( Nothing, False ) ->
                            case model.networkUuid of
                                Just netUuid ->
                                    ( Just <| GotCreateServerButtonPressed netUuid flavor.id
                                    , Nothing
                                    )

                                _ ->
                                    let
                                        invalidNetworkField =
                                            if model.networkUuid == Nothing then
                                                [ "network" ]

                                            else
                                                []

                                        invalidFormFields =
                                            invalidNetworkField
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
                                String.join " "
                                    [ context.localization.maxResourcesPerProject
                                        |> Helpers.String.pluralize
                                        |> Helpers.String.toTitleCase
                                    , "have been exhausted. Contact your"
                                    , context.localization.openstackWithOwnKeystone
                                    , "administrator, or delete some stuff"
                                    ]
                        in
                        invalidMessage context.palette invalidFormHint

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
                                                if List.isEmpty invalidFormFields then
                                                    genericInvalidFormHint

                                                else
                                                    let
                                                        invalidFormFieldsString =
                                                            Helpers.String.itemsListToString <|
                                                                List.map (\s -> "'" ++ s ++ "'")
                                                                    invalidFormFields
                                                    in
                                                    if List.length invalidFormFields == 1 then
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
                                invalidMessage context.palette invalidFormHint

                hasAnyKeypairs : Bool
                hasAnyKeypairs =
                    project.keypairs |> RDPP.withDefault [] |> List.isEmpty |> not
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
                    ++ Forms.resourceNameAlreadyExists context project currentTime { resource = Compute model.serverName, onSuggestionPressed = \suggestion -> GotServerName suggestion }
                )
            , Element.row [ Element.spacing spacer.px8 ]
                [ Text.strong <|
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
                Nothing
                computeQuota
                (\flavorGroupTipId -> SharedMsg <| SharedMsg.TogglePopover flavorGroupTipId)
                (Helpers.String.hyphenate [ "serverCreateFlavorGroupTip", project.auth.project.uuid ])
                Nothing
                (Just flavor.id)
                GotFlavorId
            , volBackedPrompt project context model volumeQuota flavor
            , countPicker context model computeQuota volumeQuota flavor
            , desktopEnvironmentPicker context project model
            , customWorkflowInput context project model
            , if hasAnyKeypairs then
                keypairPicker context project model

              else
                -- No keypairs, so show this further down in advanced options
                Element.none
            , if context.experimentalFeaturesEnabled then
                securityGroupPicker context project model

              else
                Element.none
            , Element.column
                [ Element.spacing spacer.px32 ]
              <|
                Input.radioRow [ Element.spacing spacer.px32 ]
                    { label =
                        Input.labelAbove VH.radioLabelAttributes
                            (Text.strong "Advanced Options")
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
                            , if hasAnyKeypairs then
                                -- Show this further up, outside of the advanced options
                                Element.none

                              else
                                keypairPicker context project model
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
            case ( project.flavors.data, project.flavors.refreshStatus ) of
                ( RDPP.DontHave, RDPP.NotLoading Nothing ) ->
                    --
                    [ Element.text "Uninitialized" ]

                ( RDPP.DontHave, RDPP.Loading ) ->
                    [ loading "Loading..." ]

                ( RDPP.DontHave, RDPP.NotLoading (Just _) ) ->
                    -- Error will be shown to the user via another method
                    []

                ( RDPP.DoHave flavors _, _ ) ->
                    let
                        allowedFlavors =
                            getAllowedFlavors model flavors
                    in
                    case allowedFlavors of
                        [] ->
                            case flavors of
                                [] ->
                                    [ Element.text <|
                                        String.concat
                                            [ Helpers.String.toTitleCase context.localization.unitOfTenancy
                                            , " has no flavors assigned. You cannot create "
                                            , Helpers.String.pluralize context.localization.virtualComputer
                                            , " until flavors are made available. Contact your OpenStack administrator."
                                            ]
                                    ]

                                _ ->
                                    [ Element.text <|
                                        String.concat
                                            [ "You are not allowed to use any of the flavors in your "
                                            , context.localization.unitOfTenancy
                                            , ". You cannot create "
                                            , Helpers.String.pluralize context.localization.virtualComputer
                                            , " until flavors are made available. Contact your OpenStack administrator."
                                            ]
                                    ]

                        smallestFlavor :: _ ->
                            case
                                ( RDPP.toMaybe project.computeQuota
                                , RDPP.toMaybe project.volumeQuota
                                )
                            of
                                ( Just computeQuota, Just volumeQuota ) ->
                                    let
                                        selectedFlavor =
                                            getSelectedFlavor model.selectedFlavor smallestFlavor
                                    in
                                    contents selectedFlavor computeQuota volumeQuota

                                _ ->
                                    [ loading "Loading..." ]
        ]


volBackedPrompt : Project -> View.Types.Context -> Model -> OSTypes.VolumeQuota -> OSTypes.Flavor -> Element.Element Msg
volBackedPrompt project context model volumeQuota flavor =
    let
        ( volumeCountAvail, volumeSizeGbAvail ) =
            OSQuotas.volumeQuotaAvail volumeQuota

        canLaunchVolBackedCount =
            case volumeCountAvail of
                OSTypes.Limit l ->
                    l >= 1

                OSTypes.Unlimited ->
                    True

        canLaunchVolBackedSizeGb =
            case volumeSizeGbAvail of
                OSTypes.Limit l ->
                    l >= 2

                OSTypes.Unlimited ->
                    True

        canLaunchVolBacked =
            canLaunchVolBackedCount && canLaunchVolBackedSizeGb

        defaultVolSizeGB =
            10
    in
    Element.column [ Element.spacing spacer.px12 ]
        [ Text.strong "Choose a root disk size"
        , if canLaunchVolBacked then
            let
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
                        let
                            { locale } =
                                context
                        in
                        String.concat
                            [ FormatNumber.format { locale | decimals = Exact 0 } (toFloat flavorRootDiskSize)
                            , " GB (default for selected "
                            , context.localization.virtualComputerHardwareConfig
                            , ")"
                            ]
            in
            Input.radio [ Element.spacing spacer.px4 ]
                { label = Input.labelHidden "Root disk size"
                , onChange =
                    \new ->
                        let
                            newVolSizeTextInput =
                                if new then
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

          else
            Element.text <|
                String.concat
                    [ "(N/A: "
                    , context.localization.blockDevice
                    , " "
                    , context.localization.maxResourcesPerProject
                    , " exhausted, cannot launch "
                    , Helpers.String.indefiniteArticle context.localization.blockDevice ++ " "
                    , context.localization.blockDevice
                    , "-backed "
                    , context.localization.virtualComputer
                    , ")"
                    ]
        , case model.volSizeTextInput of
            Nothing ->
                Element.none

            Just volSizeTextInput ->
                let
                    defaultVolNumericInputParams =
                        { labelText = "Root disk size (GB)"
                        , minVal = Just 2
                        , maxVal =
                            case volumeSizeGbAvail of
                                OSTypes.Limit l ->
                                    Just l

                                OSTypes.Unlimited ->
                                    Nothing
                        , defaultVal = Just defaultVolSizeGB
                        , required = True
                        }
                in
                Element.row [ Element.spacing spacer.px8 ]
                    [ numericTextInput
                        context.palette
                        (VH.inputItemAttributes context.palette)
                        volSizeTextInput
                        defaultVolNumericInputParams
                        (\newInput -> GotVolSizeTextInput <| Just newInput)
                    , case ( volumeSizeGbAvail, volSizeTextInput ) of
                        ( OSTypes.Limit volumeSizeAvail_, ValidNumericTextInput i ) ->
                            if i == volumeSizeAvail_ then
                                Element.text ("(" ++ context.localization.maxResourcesPerProject ++ " max)")

                            else
                                Element.none

                        ( _, _ ) ->
                            Element.none
                    ]
        , case compareDiskSize project model of
            Helpers.ValidationResult.Rejected { actual, acceptable } ->
                "Root disk size of %serverDiskSize% GB is not enough for %imageName%, minimum disk size required is %minDiskSize% GB"
                    |> String.replace "%serverDiskSize%" (actual |> String.fromInt)
                    |> String.replace "%minDiskSize%" (acceptable |> String.fromInt)
                    |> String.replace "%imageName%" model.imageName
                    |> invalidMessage context.palette

            Helpers.ValidationResult.Accepted ->
                Element.none

            Helpers.ValidationResult.Unknown ->
                Element.none
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
        [ Element.row [ Element.spacing spacer.px12 ]
            [ Text.strong <|
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
                            , " is chosen, each will be named, for example, \""

                            -- As example we used 3 as instance count
                            , Rest.Naming.generateServerName model.serverName 3 1
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
                ST.PositionRight

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
                Input.labelAbove VH.radioLabelAttributes
                    (Element.row [ Element.spacing spacer.px8 ]
                        [ Text.text Text.Emphasized
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
            :: (if model.includeWorkflow then
                    let
                        displayRepoInputError =
                            model.workflowInputRepository == "" && model.workflowInputIsValid == Just False

                        repoInputLabel =
                            VH.requiredLabel context.palette (Element.text "DOI or Git repository URL")

                        repoInputHelperText =
                            if displayRepoInputError then
                                invalidMessage context.palette "Required"

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
                    [ Element.column
                        [ Element.width Element.fill
                        , Element.spacing spacer.px24
                        ]
                        [ repoInput
                        , referenceInput
                        , sourcePathInput
                        ]
                    ]

                else
                    [ Element.none ]
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
        experimentalTag =
            Tag.tag context.palette "Experimental"
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing spacer.px12
        ]
        [ Input.radioRow [ Element.spacing spacer.px32 ]
            { label =
                Input.labelAbove VH.radioLabelAttributes
                    (Element.wrappedRow [ Element.spacing spacer.px8 ]
                        [ Text.text Text.Emphasized [] ("Create your own SLURM cluster with this " ++ context.localization.virtualComputer ++ " as the head node")
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
            let
                warnings =
                    [ Element.text {- @nonlocalized -} "Warning: This will only work on Jetstream Cloud, and can take 30 minutes or longer to set up a cluster."
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
            in
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
                    let
                        cloudSpecificMessage : Maybe String
                        cloudSpecificMessage =
                            GetterSetters.cloudSpecificConfigLookup context.cloudSpecificConfigs project
                                |> Maybe.andThen .desktopMessage
                    in
                    case cloudSpecificMessage of
                        Just "" ->
                            -- Empty string, cloud operator wants to hide message entirely
                            Nothing

                        Just message ->
                            Just message

                        Nothing ->
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
                            in
                            Just genericMessage
              )
                |> Maybe.map Element.text
            , let
                warningMaxGB =
                    12

                rootDiskWarnText =
                    String.join " "
                        [ "Warning: root disk may be too small for a graphical desktop environment. Please select"
                        , Helpers.String.indefiniteArticle context.localization.virtualComputerHardwareConfig
                        , context.localization.virtualComputerHardwareConfig
                        , "with"
                        , Helpers.String.indefiniteArticle <| String.fromInt warningMaxGB
                        , String.fromInt warningMaxGB
                        , "GB or larger root disk, or select a volume-backed root disk at least"
                        , String.fromInt warningMaxGB
                        , "GB in size."
                        ]
              in
              case model.volSizeTextInput of
                Nothing ->
                    case model.selectedFlavor of
                        WithSelectedFlavor flavor ->
                            if flavor.disk_root < warningMaxGB then
                                Just <| Element.text rootDiskWarnText

                            else
                                Nothing

                        _ ->
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
                    VH.radioLabelAttributes
                    (Text.strong <|
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
                        VH.radioLabelAttributes
                        (Text.strong "Deploy Guacamole for easy remote access?")
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
                    VH.radioLabelAttributes
                    (Text.strong "Install operating system updates?")
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
        guidance =
            let
                networkOptions =
                    Helpers.newServerNetworkOptions project

                withTextColor : String -> Element.Element msg
                withTextColor text =
                    Element.paragraph
                        [ Font.color (SH.toElementColor context.palette.danger.textOnNeutralBG) ]
                        [ Element.text text ]

                canChoose : Bool
                canChoose =
                    networkOptions == ManualNetworkSelection && model.networkUuid == Nothing
            in
            withTextColor
                (if canChoose then
                    "Please choose a network."

                 else
                    "Please only change this if you know what you are doing."
                )

        picker =
            let
                networkAsInputOption network =
                    ( network.uuid, VH.resourceName (Just network.name) network.uuid )
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
            (Text.strong "Network")
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
                                [ "Assign"
                                , Helpers.String.indefiniteArticle context.localization.floatingIpAddress
                                , context.localization.floatingIpAddress
                                , "to this"
                                , context.localization.virtualComputer
                                ]
                        )
                    , Input.option DoNotUseFloatingIp
                        (Element.text <|
                            String.join " "
                                [ "Do not create or assign"
                                , Helpers.String.indefiniteArticle context.localization.floatingIpAddress
                                , context.localization.floatingIpAddress
                                ]
                        )
                    ]
            in
            Input.radio [ Element.spacing spacer.px4 ]
                { label =
                    Input.labelHidden <|
                        String.join " "
                            [ "Choose"
                            , Helpers.String.indefiniteArticle context.localization.floatingIpAddress
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
                        [ Text.strong <|
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
            [ Text.strong <|
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

        promptText =
            String.join " "
                [ "Choose"
                , Helpers.String.indefiniteArticle context.localization.pkiPublicKeyForSsh
                , context.localization.pkiPublicKeyForSsh
                ]

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
                Input.radio [ Element.spacing spacer.px4 ]
                    { label = Input.labelHidden promptText
                    , onChange = \keypairName -> GotKeypairName <| keypairName
                    , options = noneOption :: List.map keypairAsOption keypairs
                    , selected = Just model.keypairName
                    }
    in
    Element.column
        [ Element.spacing spacer.px12 ]
        [ Text.strong promptText
        , VH.renderRDPP
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
                            , featherIcon [] Icons.chevronRight
                            ]
                    , onPress =
                        Just <| NoOp
                    }
            }
        ]


securityGroupPicker : View.Types.Context -> Project -> Model -> Element.Element Msg
securityGroupPicker context project model =
    let
        securityGroupAsOption : OSTypes.SecurityGroup -> Input.Option (Maybe String) Msg
        securityGroupAsOption securityGroup =
            let
                description =
                    Maybe.withDefault "" securityGroup.description

                extraInfo =
                    if String.isEmpty description then
                        ""

                    else
                        " - " ++ description

                rulesTable securityGroupUuid =
                    SecurityGroupRulesTable.view context project securityGroupUuid
            in
            Input.option (Just securityGroup.uuid)
                (Element.row []
                    [ Element.el
                        [ Element.width <| Element.maximum 600 <| Element.fill, Element.htmlAttribute <| Html.Attributes.style "min-width" "0" ]
                        (VH.ellipsizedText (securityGroup.name ++ extraInfo))
                    , Style.Widgets.ToggleTip.toggleTip
                        context
                        (SharedMsg << SharedMsg.TogglePopover)
                        (Helpers.String.hyphenate
                            [ "securityGroupRulesId"
                            , project.auth.project.uuid
                            , securityGroup.uuid
                            ]
                        )
                        (rulesTable securityGroup.uuid)
                        ST.PositionRight
                    ]
                )

        promptText =
            String.join " "
                [ "Choose"
                , Helpers.String.indefiniteArticle context.localization.securityGroup
                , context.localization.securityGroup
                ]

        renderSecurityGroups securityGroups =
            let
                presets =
                    securityGroups
                        |> List.filter (securityGroupTaggedAs securityGroupExoTags.preset)

                defaults =
                    securityGroups
                        |> List.filter (isDefaultSecurityGroup context project)
            in
            Input.radio [ Element.spacing spacer.px4 ]
                { label = Input.labelHidden promptText
                , onChange = \securityGroupUuid -> GotSecurityGroupUuid <| securityGroupUuid
                , options = List.map securityGroupAsOption (defaults ++ presets)
                , selected = Just model.securityGroupUuid
                }
    in
    Element.column
        [ Element.spacing spacer.px12 ]
        [ Text.strong promptText
        , VH.renderRDPP
            context
            project.securityGroups
            (Helpers.String.pluralize context.localization.securityGroup)
            renderSecurityGroups
        ]


userDataInput : View.Types.Context -> Model -> Element.Element Msg
userDataInput context model =
    let
        cloudConfigExplainer : Element.Element Msg
        cloudConfigExplainer =
            let
                cloudInit =
                    Link.externalLink context.palette "https://cloudinit.readthedocs.io/en/latest/index.html" "cloud-init"

                instance =
                    Element.text context.localization.virtualComputer

                cloudConfigExamplesTooltip =
                    Style.Widgets.ToggleTip.toggleTip
                        context
                        (\toggleId -> SharedMsg (SharedMsg.TogglePopover toggleId))
                        "ServerCreate-cloud-config-examples-tooltip"
                        (Element.el
                            [ Text.fontSize Text.Body
                            , Element.width (Element.px 250)
                            ]
                            (let
                                examples =
                                    Link.externalLink context.palette "https://cloudinit.readthedocs.io/en/latest/reference/examples.html" "examples"
                             in
                             Element.paragraph []
                                [ Element.text "Other configuration "
                                , examples
                                , Element.text "."
                                ]
                            )
                        )
                        ST.PositionTopLeft
            in
            Element.paragraph []
                [ Element.text "This "
                , cloudInit
                , cloudConfigExamplesTooltip
                , Element.text " config describes how to provision the "
                , instance
                , Element.text ". It's provided here to permit specific changes in rare circumstances; please modify it cautiously."
                ]

        cloudConfigWarning : Element.Element Msg
        cloudConfigWarning =
            Alert.alert []
                context.palette
                { state = Alert.Warning
                , showIcon = True
                , showContainer = True
                , content =
                    let
                        t =
                            Element.text

                        graphicalDesktop =
                            t context.localization.graphicalDesktopEnvironment

                        terminal =
                            t context.localization.commandDrivenTextInterface
                    in
                    Element.paragraph []
                        [ t "By editing this it's possible to break various Exosphere features like ", graphicalDesktop, t ", ", terminal, t ", usage graphs, setup status, etc." ]
                }

        receiveUserDataTemplate : String -> Msg
        receiveUserDataTemplate =
            GotUserDataTemplate

        copyable =
            copyableTextAccessory context.palette model.userDataTemplate
    in
    Element.column
        [ Element.spacing spacer.px12 ]
        [ Text.strong
            (Helpers.String.toTitleCase context.localization.cloudInitData)
        , cloudConfigExplainer
        , cloudConfigWarning
        , Element.row
            [ Element.spacing spacer.px8 ]
            [ Input.multiline
                (VH.inputItemAttributes context.palette
                    ++ [ Element.width Element.fill
                       , Element.height (Element.px 500)
                       , Element.spacing spacer.px4
                       , Text.fontFamily Text.Mono
                       ]
                    ++ Text.typographyAttrs Tiny
                    ++ [ copyable.id ]
                )
                { onChange = receiveUserDataTemplate
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
            , copyable.accessory
            ]
        ]


compareDiskSize : Project -> Model -> Helpers.ValidationResult.ValidationResult Int
compareDiskSize project model =
    let
        minimumImageSize : Maybe Int
        minimumImageSize =
            Maybe.map
                (\image ->
                    Basics.max
                        (image.size |> Maybe.withDefault 0 |> Helpers.Units.bytesToGiB)
                        (image.minDiskGB |> Maybe.withDefault 0)
                )
                (GetterSetters.imageLookup project model.imageUuid)

        selectedDiskSize : Maybe Int
        selectedDiskSize =
            case model.volSizeTextInput of
                Nothing ->
                    case model.selectedFlavor of
                        WithSelectedFlavor flavor ->
                            Just <| .disk_root flavor

                        _ ->
                            Nothing

                Just volSize ->
                    Style.Widgets.NumericTextInput.NumericTextInput.toMaybe volSize

        isDiskSizeEnough : Int -> Int -> Helpers.ValidationResult.ValidationResult Int
        isDiskSizeEnough disk imageMin =
            if disk >= imageMin then
                Helpers.ValidationResult.Accepted

            else
                Helpers.ValidationResult.Rejected { acceptable = imageMin, actual = disk }
    in
    Maybe.map2 isDiskSizeEnough selectedDiskSize minimumImageSize
        |> Maybe.withDefault Helpers.ValidationResult.Unknown
