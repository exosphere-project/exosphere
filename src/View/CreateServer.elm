module View.CreateServer exposing (createServer)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Modifier as Modifier
import Helpers.Helpers as Helpers
import Maybe
import Types.Types
    exposing
        ( CreateServerField(..)
        , CreateServerRequest
        , Msg(..)
        , NewServerNetworkOptions(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        )
import View.Helpers as VH exposing (edges)


createServer : Project -> CreateServerRequest -> Element.Element Msg
createServer project createServerRequest =
    let
        serverNameEmptyHint =
            if createServerRequest.name == "" then
                [ VH.hint "Server name can't be empty" ]

            else
                []

        requestIsValid =
            if createServerRequest.name == "" then
                False

            else if createServerRequest.flavorUuid == "" then
                False

            else
                True

        createOnPress =
            if requestIsValid == True then
                Just (ProjectMsg (Helpers.getProjectId project) (RequestCreateServer createServerRequest))

            else
                Nothing
    in
    Element.row VH.exoRowAttributes
        [ Element.column
            (VH.exoColumnAttributes
                ++ [ Element.width (Element.px 600) ]
            )
            [ Element.el VH.heading2 (Element.text "Create Server")
            , Input.text
                (Element.spacing 12 :: serverNameEmptyHint)
                { text = createServerRequest.name
                , placeholder = Just (Input.placeholder [] (Element.text "My Server"))
                , onChange = \n -> InputCreateServerField createServerRequest (CreateServerName n)
                , label = Input.labelLeft [] (Element.text "Name")
                }
            , Element.row VH.exoRowAttributes [ Element.text "Image: ", Element.text createServerRequest.imageName ]
            , Element.row VH.exoRowAttributes
                [ Element.el [ Element.width Element.shrink ] (Element.text createServerRequest.count)
                , Input.slider
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
                    { onChange = \c -> InputCreateServerField createServerRequest (CreateServerCount (String.fromFloat c))
                    , label = Input.labelLeft [] (Element.text "How many?")
                    , min = 1
                    , max = 10
                    , step = Just 1
                    , value = String.toFloat createServerRequest.count |> Maybe.withDefault 1.0
                    , thumb =
                        Input.defaultThumb
                    }
                ]
            , flavorPicker project createServerRequest
            , volBackedPrompt project createServerRequest
            , networkPicker project createServerRequest
            , keypairPicker project createServerRequest
            , userDataInput project createServerRequest
            , Element.el [ Element.alignRight ] <|
                Button.button
                    [ Modifier.Primary ]
                    createOnPress
                    "Create"
            ]
        ]


flavorPicker : Project -> CreateServerRequest -> Element.Element Msg
flavorPicker project createServerRequest =
    let
        -- This is a kludge. Input.radio is intended to display a group of multiple radio buttons,
        -- but we want to embed a button in each table row, so we define several Input.radios,
        -- each containing just a single option.
        -- https://elmlang.slack.com/archives/C4F9NBLR1/p1539909855000100
        radioButton flavor =
            Input.radio
                []
                { label = Input.labelHidden flavor.name
                , onChange = \f -> InputCreateServerField createServerRequest (CreateServerSize f)
                , options = [ Input.option flavor.uuid (Element.text " ") ]
                , selected =
                    if flavor.uuid == createServerRequest.flavorUuid then
                        Just flavor.uuid

                    else
                        Nothing
                }

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
            if createServerRequest.flavorUuid == "" then
                [ VH.hint "Please pick a size" ]

            else
                []
    in
    Element.column
        VH.exoColumnAttributes
        [ Element.el [ Font.bold ] (Element.text "Size")
        , Element.table
            flavorEmptyHint
            { data = Helpers.sortedFlavors project.flavors
            , columns = columns
            }
        , Element.paragraph [ Font.size 12 ] [ Element.text zeroRootDiskExplainText ]
        ]


volBackedPrompt : Project -> CreateServerRequest -> Element.Element Msg
volBackedPrompt project createServerRequest =
    let
        maybeFlavor =
            List.filter (\f -> f.uuid == createServerRequest.flavorUuid) project.flavors
                |> List.head

        flavorRootDiskSize =
            case maybeFlavor of
                Nothing ->
                    {- This should be an impossible state -}
                    0

                Just flavor ->
                    flavor.disk_root

        nonVolBackedOptionText =
            if flavorRootDiskSize == 0 then
                "Default for selected image (warning, could be too small for your work)"

            else
                String.fromInt flavorRootDiskSize ++ " GB (default for selected size)"

        volSizeSlider =
            --                    Element.el [ Element.width Element.shrink ] (Element.text createServerRequest.volBackedSizeGb)
            Input.slider
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
                { onChange = \c -> InputCreateServerField createServerRequest (CreateServerVolBackedSize (String.fromFloat c))
                , label = Input.labelRight [] (Element.text (createServerRequest.volBackedSizeGb ++ " GB"))
                , min = 2
                , max = 100
                , step = Just 1
                , value = String.toFloat createServerRequest.volBackedSizeGb |> Maybe.withDefault 2.0
                , thumb =
                    Input.defaultThumb
                }
    in
    Element.column VH.exoColumnAttributes
        [ Input.radio []
            { label = Input.labelAbove [ Element.paddingXY 0 12 ] (Element.text "Choose a root disk size")
            , onChange = \new -> InputCreateServerField createServerRequest (CreateServerVolBacked new)
            , options =
                [ Input.option False (Element.text nonVolBackedOptionText)
                , Input.option True (Element.text "Custom disk size (volume-backed)")

                {- -}
                ]
            , selected = Just createServerRequest.volBacked
            }
        , if not createServerRequest.volBacked then
            Element.none

          else
            volSizeSlider
        ]


networkPicker : Project -> CreateServerRequest -> Element.Element Msg
networkPicker project createServerRequest =
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
                            if createServerRequest.networkUuid == "" then
                                [ VH.hint "Please pick a network" ]

                            else
                                []
                    in
                    [ Input.radio networkEmptyHint
                        { label = Input.labelAbove [ Element.paddingXY 0 12 ] (Element.text "Choose a Network")
                        , onChange = \networkUuid -> InputCreateServerField createServerRequest (CreateServerNetworkUuid networkUuid)
                        , options = List.map networkAsInputOption project.networks
                        , selected = Just createServerRequest.networkUuid
                        }
                    , guessText
                    ]
    in
    Element.column
        VH.exoColumnAttributes
        (Element.el [ Font.bold ] (Element.text "Network") :: contents)


keypairPicker : Project -> CreateServerRequest -> Element.Element Msg
keypairPicker project createServerRequest =
    let
        keypairAsOption keypair =
            Input.option keypair.name (Element.text keypair.name)

        contents =
            if List.isEmpty project.keypairs then
                Element.text "(This OpenStack project has no keypairs to choose from, but you can still create a server!)"

            else
                Input.radio []
                    { label = Input.labelAbove [ Element.paddingXY 0 12 ] (Element.text "Choose a keypair (this is optional, skip if unsure)")
                    , onChange = \keypairName -> InputCreateServerField createServerRequest (CreateServerKeypairName keypairName)
                    , options = List.map keypairAsOption project.keypairs
                    , selected = Just (Maybe.withDefault "" createServerRequest.keypairName)
                    }
    in
    Element.column
        VH.exoColumnAttributes
        [ Element.el [ Font.bold ] (Element.text "SSH Keypair")
        , contents
        ]


userDataInput : Project -> CreateServerRequest -> Element.Element Msg
userDataInput _ createServerRequest =
    Element.column
        VH.exoColumnAttributes
        [ Input.radioRow [ Element.spacing 10 ]
            { label = Input.labelAbove [ Element.paddingXY 0 12, Font.bold ] (Element.text "Advanced Options")
            , onChange = \new -> InputCreateServerField createServerRequest (CreateServerShowAdvancedOptions new)
            , options =
                [ Input.option False (Element.text "Hide")
                , Input.option True (Element.text "Show")

                {- -}
                ]
            , selected = Just createServerRequest.showAdvancedOptions
            }
        , if not createServerRequest.showAdvancedOptions then
            Element.none

          else
            Input.multiline
                [ Element.width (Element.px 600)
                , Element.height (Element.px 500)
                ]
                { onChange = \u -> InputCreateServerField createServerRequest (CreateServerUserData u)
                , text = createServerRequest.userData
                , placeholder = Just (Input.placeholder [] (Element.text "#!/bin/bash\n\n# Your script here"))
                , label =
                    Input.labelAbove
                        [ Element.paddingXY 20 0
                        , Font.bold
                        ]
                        (Element.text "User Data (Boot Script)")
                , spellcheck = False
                }
        ]
