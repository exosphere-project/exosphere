module View.CreateServer exposing (createServer)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Maybe
import OpenStack.Quotas as OSQuotas
import OpenStack.ServerNameValidator exposing (serverNameValidator)
import OpenStack.Types as OSTypes
import RemoteData
import Style.Helpers as SH
import Style.Widgets.NumericTextInput.NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Types.Types
    exposing
        ( CreateServerViewParams
        , Msg(..)
        , NewServerNetworkOptions(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        )
import View.Helpers as VH exposing (edges)
import View.Types
import Widget
import Widget.Style.Material


updateCreateServerRequest : Project -> CreateServerViewParams -> Msg
updateCreateServerRequest project viewParams =
    ProjectMsg project.auth.project.uuid <|
        SetProjectView <|
            CreateServer viewParams


createServer : View.Types.Context -> Project -> CreateServerViewParams -> Element.Element Msg
createServer context project viewParams =
    let
        invalidNameReasons =
            serverNameValidator (Just context.localization.virtualComputer) viewParams.serverName

        renderInvalidNameReasons =
            case invalidNameReasons of
                Just reasons ->
                    Element.column
                        [ Font.color (SH.toElementColor context.palette.error)
                        , Font.size 14
                        , Element.alignRight
                        , Element.moveDown 6
                        ]
                    <|
                        List.map Element.text reasons

                Nothing ->
                    Element.none

        maybeNetworkGuidance =
            case Helpers.newServerNetworkOptions project of
                NetworksLoading ->
                    Just "Loading networks, please wait a moment."

                AutoSelectedNetwork _ ->
                    Nothing

                ManualNetworkSelection ->
                    case viewParams.networkUuid of
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

        createOnPress =
            let
                invalidVolSizeTextInput =
                    case viewParams.volSizeTextInput of
                        Just input ->
                            case input of
                                ValidNumericTextInput _ ->
                                    False

                                InvalidNumericTextInput _ ->
                                    True

                        Nothing ->
                            False
            in
            case ( invalidNameReasons, invalidVolSizeTextInput, viewParams.networkUuid ) of
                ( Nothing, False, Just netUuid ) ->
                    Just (ProjectMsg project.auth.project.uuid (RequestCreateServer viewParams netUuid))

                ( _, _, _ ) ->
                    Nothing

        contents flavor computeQuota volumeQuota =
            [ Input.text
                (VH.inputItemAttributes context.palette.background)
                { text = viewParams.serverName
                , placeholder =
                    Just
                        (Input.placeholder
                            []
                            (Element.text <|
                                String.join " "
                                    [ "My"
                                    , context.localization.virtualComputer
                                        |> Helpers.String.toTitleCase
                                    ]
                            )
                        )
                , onChange = \n -> updateCreateServerRequest project { viewParams | serverName = n }
                , label = Input.labelLeft [] (Element.text "Name")
                }
            , renderInvalidNameReasons
            , Element.row VH.exoRowAttributes
                [ Element.text <|
                    String.concat
                        [ context.localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.toTitleCase
                        , ": "
                        ]
                , Element.text viewParams.imageName
                ]
            , flavorPicker context project viewParams computeQuota
            , volBackedPrompt context project viewParams volumeQuota flavor
            , countPicker context project viewParams computeQuota volumeQuota flavor
            , desktopEnvironmentPicker context project viewParams
            , Element.column
                VH.exoColumnAttributes
              <|
                [ Input.radioRow [ Element.spacing 10 ]
                    { label = Input.labelAbove [ Element.paddingXY 0 12, Font.bold ] (Element.text "Advanced Options")
                    , onChange = \new -> updateCreateServerRequest project { viewParams | showAdvancedOptions = new }
                    , options =
                        [ Input.option False (Element.text "Hide")
                        , Input.option True (Element.text "Show")

                        {- -}
                        ]
                    , selected = Just viewParams.showAdvancedOptions
                    }
                ]
                    ++ (if not viewParams.showAdvancedOptions then
                            [ Element.none ]

                        else
                            [ skipOperatingSystemUpdatesPicker context project viewParams
                            , guacamolePicker context project viewParams
                            , networkPicker context project viewParams
                            , keypairPicker context project viewParams
                            , userDataInput context project viewParams
                            ]
                       )
            , renderNetworkGuidance
            , Element.el [ Element.alignRight ] <|
                Widget.textButton
                    (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                    { text = "Create"
                    , onPress = createOnPress
                    }
            ]
    in
    Element.row VH.exoRowAttributes
        [ Element.column
            (VH.exoColumnAttributes
                ++ [ Element.width (Element.px 600) ]
            )
          <|
            [ Element.el
                VH.heading2
                (Element.text <|
                    String.join " "
                        [ "Create"
                        , context.localization.virtualComputer
                            |> Helpers.String.toTitleCase
                        ]
                )
            ]
                ++ (case
                        ( GetterSetters.flavorLookup project viewParams.flavorUuid
                        , project.computeQuota
                        , project.volumeQuota
                        )
                    of
                        ( Just flavor, RemoteData.Success computeQuota, RemoteData.Success volumeQuota ) ->
                            contents flavor computeQuota volumeQuota

                        ( _, _, RemoteData.Loading ) ->
                            -- TODO deduplicate this with code below
                            [ Element.row [ Element.spacing 15 ]
                                [ Widget.circularProgressIndicator
                                    (SH.materialStyle context.palette).progressIndicator
                                    Nothing
                                , Element.text "Loading..."
                                ]
                            ]

                        ( _, RemoteData.Loading, _ ) ->
                            [ Element.row [ Element.spacing 15 ]
                                [ Widget.circularProgressIndicator
                                    (SH.materialStyle context.palette).progressIndicator
                                    Nothing
                                , Element.text "Loading..."
                                ]
                            ]

                        ( _, _, _ ) ->
                            [ Element.text "oops, we shouldn't be here" ]
                   )
        ]


flavorPicker : View.Types.Context -> Project -> CreateServerViewParams -> OSTypes.ComputeQuota -> Element.Element Msg
flavorPicker context project viewParams computeQuota =
    let
        -- This is a kludge. Input.radio is intended to display a group of multiple radio buttons,
        -- but we want to embed a button in each table row, so we define several Input.radios,
        -- each containing just a single option.
        -- https://elmlang.slack.com/archives/C4F9NBLR1/p1539909855000100
        radioButton flavor =
            let
                radio_ =
                    Input.radio
                        []
                        { label = Input.labelHidden flavor.name
                        , onChange = \f -> updateCreateServerRequest project { viewParams | flavorUuid = f }
                        , options = [ Input.option flavor.uuid (Element.text " ") ]
                        , selected =
                            if flavor.uuid == viewParams.flavorUuid then
                                Just flavor.uuid

                            else
                                Nothing
                        }
            in
            -- Only allow selection if there is enough available quota
            case OSQuotas.computeQuotaFlavorAvailServers computeQuota flavor of
                Nothing ->
                    radio_

                Just availServers ->
                    if availServers < 1 then
                        Element.text "X"

                    else
                        radio_

        paddingRight =
            Element.paddingEach { edges | right = 15 }

        headerAttribs =
            [ paddingRight
            , Font.bold
            , Font.center
            ]

        columns =
            [ { header = Element.none
              , width = Element.fill
              , view = \r -> radioButton r
              }
            , { header = Element.el (headerAttribs ++ [ Font.alignLeft ]) (Element.text "Name")
              , width = Element.fill
              , view = \r -> Element.el [ paddingRight ] (Element.text r.name)
              }
            , { header = Element.el headerAttribs (Element.text "CPUs")
              , width = Element.fill
              , view = \r -> Element.el [ paddingRight, Font.alignRight ] (Element.text (String.fromInt r.vcpu))
              }
            , { header = Element.el headerAttribs (Element.text "RAM (GB)")
              , width = Element.fill
              , view = \r -> Element.el [ paddingRight, Font.alignRight ] (Element.text (r.ram_mb // 1024 |> String.fromInt))
              }
            , { header = Element.el headerAttribs (Element.text "Root Disk")
              , width = Element.fill
              , view =
                    \r ->
                        Element.el
                            [ paddingRight, Font.alignRight ]
                            (if r.disk_root == 0 then
                                Element.text "- *"

                             else
                                Element.text (String.fromInt r.disk_root ++ " GB")
                            )
              }
            , { header = Element.el headerAttribs (Element.text "Ephemeral Disk")
              , width = Element.fill
              , view =
                    \r ->
                        Element.el
                            [ paddingRight, Font.alignRight ]
                            (if r.disk_ephemeral == 0 then
                                Element.text "none"

                             else
                                Element.text (String.fromInt r.disk_ephemeral ++ " GB")
                            )
              }
            ]

        zeroRootDiskExplainText =
            case List.filter (\f -> f.disk_root == 0) project.flavors |> List.head of
                Just _ ->
                    String.concat
                        [ "* No default root disk size is defined for this "
                        , context.localization.virtualComputer
                        , " "
                        , context.localization.virtualComputerHardwareConfig
                        , ", see below"
                        ]

                Nothing ->
                    ""

        flavorEmptyHint =
            if viewParams.flavorUuid == "" then
                [ VH.hint context <|
                    String.join
                        " "
                        [ "Please pick a"
                        , context.localization.virtualComputerHardwareConfig
                        ]
                ]

            else
                []

        anyFlavorsTooLarge =
            project.flavors
                |> List.map (OSQuotas.computeQuotaFlavorAvailServers computeQuota)
                |> List.filterMap (Maybe.map (\x -> x < 1))
                |> List.isEmpty
                |> not
    in
    Element.column
        VH.exoColumnAttributes
        [ Element.el
            [ Font.bold ]
            (Element.text <| Helpers.String.toTitleCase context.localization.virtualComputerHardwareConfig)
        , Element.table
            flavorEmptyHint
            { data = GetterSetters.sortedFlavors project.flavors
            , columns = columns
            }
        , if anyFlavorsTooLarge then
            Element.text <|
                String.join " "
                    [ context.localization.virtualComputerHardwareConfig
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    , "marked 'X' are too large for your available"
                    , context.localization.maxResourcesPerProject
                    ]

          else
            Element.none
        , Element.paragraph [ Font.size 12 ] [ Element.text zeroRootDiskExplainText ]
        ]


volBackedPrompt : View.Types.Context -> Project -> CreateServerViewParams -> OSTypes.VolumeQuota -> OSTypes.Flavor -> Element.Element Msg
volBackedPrompt context project viewParams volumeQuota flavor =
    let
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
                    [ String.fromInt flavorRootDiskSize
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
                        updateCreateServerRequest project
                            { viewParams
                                | volSizeTextInput =
                                    newVolSizeTextInput
                            }
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
                    case viewParams.volSizeTextInput of
                        Just _ ->
                            Just True

                        Nothing ->
                            Just False
                }
    in
    Element.column VH.exoColumnAttributes
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
        , case viewParams.volSizeTextInput of
            Nothing ->
                Element.none

            Just volSizeTextInput ->
                Element.row VH.exoRowAttributes
                    [ numericTextInput
                        context.palette
                        (VH.inputItemAttributes context.palette.background)
                        volSizeTextInput
                        defaultVolNumericInputParams
                        (\newInput -> updateCreateServerRequest project { viewParams | volSizeTextInput = Just newInput })
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
    -> Project
    -> CreateServerViewParams
    -> OSTypes.ComputeQuota
    -> OSTypes.VolumeQuota
    -> OSTypes.Flavor
    -> Element.Element Msg
countPicker context project viewParams computeQuota volumeQuota flavor =
    let
        countAvail =
            OSQuotas.overallQuotaAvailServers
                (viewParams.volSizeTextInput
                    |> Maybe.andThen Style.Widgets.NumericTextInput.NumericTextInput.toMaybe
                )
                flavor
                computeQuota
                volumeQuota
    in
    Element.column VH.exoColumnAttributes
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
                        , String.fromInt countAvail_
                        , "of these."
                        ]

            Nothing ->
                Element.none
        , Element.row VH.exoRowAttributes
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
                { onChange = \c -> updateCreateServerRequest project { viewParams | count = round c }
                , label = Input.labelHidden "How many?"
                , min = 1
                , max = countAvail |> Maybe.withDefault 20 |> toFloat
                , step = Just 1
                , value = toFloat viewParams.count
                , thumb =
                    Input.defaultThumb
                }
            , Element.el
                [ Element.width Element.shrink ]
                (Element.text <| String.fromInt viewParams.count)
            , case countAvail of
                Just countAvail_ ->
                    if viewParams.count == countAvail_ then
                        Element.text ("(" ++ context.localization.maxResourcesPerProject ++ " max)")

                    else
                        Element.none

                Nothing ->
                    Element.none
            ]
        ]


desktopEnvironmentPicker : View.Types.Context -> Project -> CreateServerViewParams -> Element.Element Msg
desktopEnvironmentPicker context project createServerViewParams =
    let
        warnings : List (Element.Element Msg)
        warnings =
            [ let
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
              case createServerViewParams.volSizeTextInput of
                Nothing ->
                    case GetterSetters.flavorLookup project createServerViewParams.flavorUuid of
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
            , if createServerViewParams.deployDesktopEnvironment then
                Just <|
                    Element.text <|
                        String.concat
                            [ "Warning: If selected image does not already include a graphical desktop environment, "
                            , context.localization.virtualComputer
                            , " can take 30 minutes or longer to deploy."
                            ]

              else
                Nothing
            ]
                |> List.filterMap identity
    in
    Element.column VH.exoColumnAttributes
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
            , onChange = \new -> updateCreateServerRequest project { createServerViewParams | deployDesktopEnvironment = new }
            , options =
                [ Input.option False (Element.text "No")
                , Input.option True (Element.text "Yes")
                ]
            , selected = Just createServerViewParams.deployDesktopEnvironment
            }
        , if createServerViewParams.deployDesktopEnvironment then
            Element.column
                ([ Background.color (SH.toElementColor context.palette.warn), Font.color (SH.toElementColor context.palette.on.warn) ]
                    ++ VH.exoElementAttributes
                )
                (List.map (\warning -> Element.paragraph [] [ warning ]) warnings)

          else
            Element.none
        ]


guacamolePicker : View.Types.Context -> Project -> CreateServerViewParams -> Element.Element Msg
guacamolePicker context project createServerViewParams =
    case createServerViewParams.deployGuacamole of
        Nothing ->
            Element.text <|
                String.concat
                    [ "Guacamole deployment is not supported for this "
                    , context.localization.openstackWithOwnKeystone
                    , "."
                    ]

        Just deployGuacamole ->
            Element.column VH.exoColumnAttributes
                [ Input.radioRow [ Element.spacing 10 ]
                    { label = Input.labelAbove [ Element.paddingXY 0 12, Font.bold ] (Element.text "Deploy Guacamole for easy remote access?")
                    , onChange = \new -> updateCreateServerRequest project { createServerViewParams | deployGuacamole = Just new }
                    , options =
                        [ Input.option True (Element.text "Yes")
                        , Input.option False (Element.text "No")

                        {- -}
                        ]
                    , selected = Just deployGuacamole
                    }
                ]


skipOperatingSystemUpdatesPicker : View.Types.Context -> Project -> CreateServerViewParams -> Element.Element Msg
skipOperatingSystemUpdatesPicker context project createServerViewParams =
    Element.column VH.exoColumnAttributes
        [ Input.radioRow [ Element.spacing 10 ]
            { label = Input.labelAbove [ Element.paddingXY 0 12, Font.bold ] (Element.text "Install operating system updates?")
            , onChange = \new -> updateCreateServerRequest project { createServerViewParams | installOperatingSystemUpdates = new }
            , options =
                [ Input.option True (Element.text "Yes")
                , Input.option False (Element.text "No")

                {- -}
                ]
            , selected = Just createServerViewParams.installOperatingSystemUpdates
            }
        , if not createServerViewParams.installOperatingSystemUpdates then
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


networkPicker : View.Types.Context -> Project -> CreateServerViewParams -> Element.Element Msg
networkPicker context project viewParams =
    let
        networkOptions =
            Helpers.newServerNetworkOptions project

        guidance =
            let
                maybeStr =
                    if networkOptions == ManualNetworkSelection && viewParams.networkUuid == Nothing then
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
                    Input.option network.uuid (Element.text network.name)
            in
            Input.radio []
                { label = Input.labelHidden "Choose a Network"
                , onChange = \networkUuid -> updateCreateServerRequest project { viewParams | networkUuid = Just networkUuid }
                , options =
                    case project.networks.data of
                        RDPP.DoHave networks _ ->
                            List.map networkAsInputOption networks

                        RDPP.DontHave ->
                            []
                , selected = viewParams.networkUuid
                }
    in
    Element.column
        VH.exoColumnAttributes
        [ Element.el [ Font.bold ] <| Element.text "Network"
        , guidance
        , picker
        ]


keypairPicker : View.Types.Context -> Project -> CreateServerViewParams -> Element.Element Msg
keypairPicker context project viewParams =
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
                    , onChange = \keypairName -> updateCreateServerRequest project { viewParams | keypairName = Just keypairName }
                    , options = List.map keypairAsOption keypairs
                    , selected = Just (Maybe.withDefault "" viewParams.keypairName)
                    }
    in
    Element.column
        VH.exoColumnAttributes
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
          Widget.iconButton
            (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
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
                Just <|
                    ProjectMsg project.auth.project.uuid <|
                        SetProjectView <|
                            CreateKeypair "" ""
            }
        ]


userDataInput : View.Types.Context -> Project -> CreateServerViewParams -> Element.Element Msg
userDataInput context project viewParams =
    Input.multiline
        (VH.inputItemAttributes context.palette.background
            ++ [ Element.width (Element.px 600)
               , Element.height (Element.px 500)
               , Element.spacing 3
               ]
        )
        { onChange = \u -> updateCreateServerRequest project { viewParams | userDataTemplate = u }
        , text = viewParams.userDataTemplate
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
            Input.labelAbove
                [ Element.paddingXY 20 0
                , Font.bold
                ]
                (Element.text <| Helpers.String.toTitleCase context.localization.cloudInitData)
        , spellcheck = False
        }
