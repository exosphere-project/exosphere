module View.CreateServer exposing (createServer)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Maybe
import OpenStack.Quotas as OSQuotas
import OpenStack.ServerNameValidator exposing (serverNameValidator)
import OpenStack.Types as OSTypes
import RemoteData
import Style.Theme
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
import Widget
import Widget.Style.Material


updateCreateServerRequest : Project -> CreateServerViewParams -> Msg
updateCreateServerRequest project viewParams =
    ProjectMsg project.auth.project.uuid <|
        SetProjectView <|
            CreateServer viewParams


createServer : Project -> CreateServerViewParams -> Element.Element Msg
createServer project viewParams =
    let
        invalidNameReasons =
            serverNameValidator viewParams.serverName

        renderInvalidNameReasons =
            case invalidNameReasons of
                Just reasons ->
                    Element.column
                        [ Font.color (Element.rgb 1 0 0)
                        , Font.size 14
                        , Element.alignRight
                        , Element.moveDown 6
                        ]
                    <|
                        List.map Element.text reasons

                Nothing ->
                    Element.none

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
            case ( invalidNameReasons, invalidVolSizeTextInput ) of
                ( Nothing, False ) ->
                    Just (ProjectMsg project.auth.project.uuid (RequestCreateServer viewParams))

                ( _, _ ) ->
                    Nothing

        contents flavor computeQuota volumeQuota =
            [ Input.text
                [ Element.spacing 12 ]
                { text = viewParams.serverName
                , placeholder = Just (Input.placeholder [] (Element.text "My Server"))
                , onChange = \n -> updateCreateServerRequest project { viewParams | serverName = n }
                , label = Input.labelLeft [] (Element.text "Name")
                }
            , renderInvalidNameReasons
            , Element.row VH.exoRowAttributes [ Element.text "Image: ", Element.text viewParams.imageName ]
            , flavorPicker project viewParams computeQuota
            , volBackedPrompt project viewParams volumeQuota flavor
            , countPicker project viewParams computeQuota volumeQuota flavor
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
                            [ guacamolePicker project viewParams
                            , networkPicker project viewParams
                            , keypairPicker project viewParams
                            , userDataInput project viewParams
                            ]
                       )
            , Element.el [ Element.alignRight ] <|
                Widget.textButton
                    (Widget.Style.Material.containedButton Style.Theme.exoPalette)
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
            [ Element.el VH.heading2 (Element.text "Create Server") ]
                ++ (case
                        ( GetterSetters.flavorLookup project viewParams.flavorUuid
                        , project.computeQuota
                        , project.volumeQuota
                        )
                    of
                        ( Just flavor, RemoteData.Success computeQuota, RemoteData.Success volumeQuota ) ->
                            contents flavor computeQuota volumeQuota

                        ( _, _, RemoteData.Loading ) ->
                            [ Element.text "Loading..." ]

                        ( _, RemoteData.Loading, _ ) ->
                            [ Element.text "Loading..." ]

                        ( _, _, _ ) ->
                            [ Element.text "oops, we shouldn't be here" ]
                   )
        ]


flavorPicker : Project -> CreateServerViewParams -> OSTypes.ComputeQuota -> Element.Element Msg
flavorPicker project viewParams computeQuota =
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
                    "* No default root disk size is defined for this server size, see below"

                Nothing ->
                    ""

        flavorEmptyHint =
            if viewParams.flavorUuid == "" then
                [ VH.hint "Please pick a size" ]

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
        [ Element.el [ Font.bold ] (Element.text "Size")
        , Element.table
            flavorEmptyHint
            { data = GetterSetters.sortedFlavors project.flavors
            , columns = columns
            }
        , if anyFlavorsTooLarge then
            Element.text "Flavors marked 'X' are too large for your available quota"

          else
            Element.none
        , Element.paragraph [ Font.size 12 ] [ Element.text zeroRootDiskExplainText ]
        ]


volBackedPrompt : Project -> CreateServerViewParams -> OSTypes.VolumeQuota -> OSTypes.Flavor -> Element.Element Msg
volBackedPrompt project viewParams volumeQuota flavor =
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
                "Default for selected image (warning, could be too small for your work)"

            else
                String.fromInt flavorRootDiskSize ++ " GB (default for selected size)"

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
                    , Input.option True (Element.text "Custom disk size (volume-backed)")
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
            Element.text "(N/A: volume quota exhausted, cannot launch a volume-backed instance)"
        , case viewParams.volSizeTextInput of
            Nothing ->
                Element.none

            Just volSizeTextInput ->
                Element.row VH.exoRowAttributes
                    [ numericTextInput
                        volSizeTextInput
                        defaultVolNumericInputParams
                        (\newInput -> updateCreateServerRequest project { viewParams | volSizeTextInput = Just newInput })
                    , case ( volumeSizeGbAvail, volSizeTextInput ) of
                        ( Just volumeSizeAvail_, ValidNumericTextInput i ) ->
                            if i == volumeSizeAvail_ then
                                Element.text "(quota max)"

                            else
                                Element.none

                        ( _, _ ) ->
                            Element.none
                    ]
        ]


countPicker :
    Project
    -> CreateServerViewParams
    -> OSTypes.ComputeQuota
    -> OSTypes.VolumeQuota
    -> OSTypes.Flavor
    -> Element.Element Msg
countPicker project viewParams computeQuota volumeQuota flavor =
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
        [ Element.text "How many servers?"
        , case countAvail of
            Just countAvail_ ->
                Element.text ("Your quota supports up to " ++ String.fromInt countAvail_ ++ " of these.")

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
                        , Background.color (Element.rgb 0.5 0.5 0.5)
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
                        Element.text "(quota max)"

                    else
                        Element.none

                Nothing ->
                    Element.none
            ]
        ]


guacamolePicker : Project -> CreateServerViewParams -> Element.Element Msg
guacamolePicker project createServerViewParams =
    case createServerViewParams.deployGuacamole of
        Nothing ->
            Element.text "Guacamole deployment is not supported for this OpenStack cloud."

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


networkPicker : Project -> CreateServerViewParams -> Element.Element Msg
networkPicker project viewParams =
    let
        networkOptions =
            Helpers.newServerNetworkOptions project

        contents =
            case networkOptions of
                NoNetsAutoAllocate ->
                    [ Element.paragraph
                        []
                        [ Element.text "There are no networks associated with your project so Exosphere will ask OpenStack to create one for you and hope for the best." ]
                    ]

                OneNet net ->
                    [ Element.paragraph
                        []
                        [ Element.text ("There is only one network, with name \"" ++ net.name ++ "\", so Exosphere will use that one.") ]
                    ]

                MultipleNetsWithGuess _ guessNet goodGuess ->
                    let
                        guessText =
                            if goodGuess then
                                Element.paragraph
                                    []
                                    [ Element.text
                                        ("The network \"" ++ guessNet.name ++ "\" is probably a good guess so Exosphere has picked it by default.")
                                    ]

                            else
                                Element.paragraph
                                    []
                                    [ Element.text "The selected network is a guess and might not be the best choice." ]

                        networkAsInputOption network =
                            Input.option network.uuid (Element.text network.name)

                        networkEmptyHint =
                            if viewParams.networkUuid == "" then
                                [ VH.hint "Please pick a network" ]

                            else
                                []
                    in
                    [ Input.radio networkEmptyHint
                        { label = Input.labelAbove [ Element.paddingXY 0 12 ] (Element.text "Choose a Network")
                        , onChange = \networkUuid -> updateCreateServerRequest project { viewParams | networkUuid = networkUuid }
                        , options =
                            case project.networks.data of
                                RDPP.DoHave networks _ ->
                                    List.map networkAsInputOption networks

                                RDPP.DontHave ->
                                    []
                        , selected = Just viewParams.networkUuid
                        }
                    , guessText
                    ]
    in
    Element.column
        VH.exoColumnAttributes
        (Element.el [ Font.bold ] (Element.text "Network") :: contents)


keypairPicker : Project -> CreateServerViewParams -> Element.Element Msg
keypairPicker project viewParams =
    let
        keypairAsOption keypair =
            Input.option keypair.name (Element.text keypair.name)

        contents =
            if List.isEmpty project.keypairs then
                Element.text "(This OpenStack project has no keypairs to choose from, but you can still create a server!)"

            else
                Input.radio []
                    { label = Input.labelAbove [ Element.paddingXY 0 12 ] (Element.text "Choose a keypair (this is optional, skip if unsure)")
                    , onChange = \keypairName -> updateCreateServerRequest project { viewParams | keypairName = Just keypairName }
                    , options = List.map keypairAsOption project.keypairs
                    , selected = Just (Maybe.withDefault "" viewParams.keypairName)
                    }
    in
    Element.column
        VH.exoColumnAttributes
        [ Element.el [ Font.bold ] (Element.text "SSH Keypair")
        , contents
        ]


userDataInput : Project -> CreateServerViewParams -> Element.Element Msg
userDataInput project viewParams =
    Input.multiline
        [ Element.width (Element.px 600)
        , Element.height (Element.px 500)
        ]
        { onChange = \u -> updateCreateServerRequest project { viewParams | userDataTemplate = u }
        , text = viewParams.userDataTemplate
        , placeholder = Just (Input.placeholder [] (Element.text "#!/bin/bash\n\n# Your script here"))
        , label =
            Input.labelAbove
                [ Element.paddingXY 20 0
                , Font.bold
                ]
                (Element.text "User Data (Boot Script)")
        , spellcheck = False
        }
