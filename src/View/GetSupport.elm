module View.GetSupport exposing (getSupport, viewStateToSupportableItem)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import RemoteData
import Set
import Style.Helpers as SH
import Style.Widgets.CopyableText
import Style.Widgets.Select
import Types.HelperTypes as HelperTypes
import Types.Types
    exposing
        ( Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , ProjectIdentifier
        , ProjectViewConstructor(..)
        , SupportableItemType(..)
        , ViewState(..)
        )
import UUID
import View.Helpers as VH
import View.Types
import Widget
import Widget.Style.Material


getSupport :
    Model
    -> View.Types.Context
    -> Maybe ( SupportableItemType, Maybe HelperTypes.Uuid )
    -> String
    -> Bool
    -> Element.Element Msg
getSupport model context maybeSupportableResource requestDescription isSubmitted =
    Element.column
        (VH.exoColumnAttributes
            ++ [ Element.spacing 30
               , Element.width Element.fill
               ]
        )
        [ Element.el (VH.heading2 context.palette) <| Element.text ("Get Support for " ++ model.style.appTitle)
        , case model.style.supportInfoMarkdown of
            Just markdown ->
                Element.column VH.contentContainer <|
                    VH.renderMarkdown context markdown

            Nothing ->
                Element.none
        , Element.column VH.formContainer
            -- TODO make textareas fill this width
            [ Input.radio
                VH.exoColumnAttributes
                { onChange =
                    \option ->
                        SetNonProjectView <| GetSupport (Maybe.map (\option_ -> ( option_, Nothing )) option) requestDescription False
                , selected =
                    maybeSupportableResource
                        |> Maybe.map Tuple.first
                        |> Just
                , label = Input.labelAbove [] (Element.text "What do you need help with?")
                , options =
                    List.map
                        (\itemType ->
                            let
                                itemTypeStrProto =
                                    supportableItemTypeStr context itemType

                                itemTypeStr =
                                    String.join " "
                                        [ Helpers.String.indefiniteArticle itemTypeStrProto
                                        , itemTypeStrProto
                                        ]
                                        |> Helpers.String.toTitleCase
                            in
                            Input.option (Just itemType) (Element.text itemTypeStr)
                        )
                        [ SupportableServer
                        , SupportableVolume
                        , SupportableImage
                        , SupportableProject
                        ]
                        ++ [ Input.option Nothing (Element.text "None of these things") ]
                }
            , case maybeSupportableResource of
                Nothing ->
                    Element.none

                Just ( supportableItemType, _ ) ->
                    Element.text <|
                        String.join " "
                            [ "Which"
                            , supportableItemTypeStr context supportableItemType
                            , "do you need help with?"
                            ]
            , case maybeSupportableResource of
                Nothing ->
                    Element.none

                Just ( supportableItemType, maybeSupportableItemUuid ) ->
                    let
                        onChange value =
                            let
                                newMaybeSupportableItemUuid =
                                    if value == "" then
                                        Nothing

                                    else
                                        Just value
                            in
                            SetNonProjectView <|
                                GetSupport
                                    (Just ( supportableItemType, newMaybeSupportableItemUuid ))
                                    requestDescription
                                    False

                        options =
                            case supportableItemType of
                                SupportableProject ->
                                    model.projects
                                        |> List.map
                                            (\proj ->
                                                ( proj.auth.project.uuid
                                                , VH.friendlyProjectTitle model proj
                                                )
                                            )

                                SupportableImage ->
                                    model.projects
                                        |> List.map .images
                                        |> List.concat
                                        |> List.map
                                            (\image ->
                                                ( image.uuid
                                                , image.name
                                                )
                                            )
                                        -- This removes duplicate values, heh
                                        |> Set.fromList
                                        |> Set.toList
                                        |> List.sortBy Tuple.second

                                SupportableServer ->
                                    model.projects
                                        |> List.map .servers
                                        |> List.map (RDPP.withDefault [])
                                        |> List.concat
                                        |> List.map
                                            (\server ->
                                                ( server.osProps.uuid
                                                , server.osProps.name
                                                )
                                            )
                                        |> List.sortBy Tuple.second

                                SupportableVolume ->
                                    model.projects
                                        |> List.map .volumes
                                        |> List.map (RemoteData.withDefault [])
                                        |> List.concat
                                        |> List.map
                                            (\volume ->
                                                ( volume.uuid
                                                , volume.name
                                                )
                                            )
                                        |> List.sortBy Tuple.second

                        label =
                            let
                                itemStrProto =
                                    supportableItemTypeStr context supportableItemType
                            in
                            String.join " "
                                [ "Select"
                                , Helpers.String.indefiniteArticle itemStrProto
                                , itemStrProto
                                ]
                    in
                    Style.Widgets.Select.select
                        []
                        { onChange =
                            onChange
                        , options = options
                        , selected = maybeSupportableItemUuid
                        , label = label
                        }
            , Input.multiline
                (VH.exoElementAttributes
                    ++ [ Element.height <| Element.px 200
                       , Element.width <| Element.maximum 500 Element.fill
                       ]
                )
                { onChange =
                    \newVal -> SetNonProjectView <| GetSupport maybeSupportableResource newVal False
                , text = requestDescription
                , placeholder = Nothing
                , label = Input.labelAbove [] (Element.text "Please describe exactly what you need help with.")
                , spellcheck = True
                }
            , Widget.textButton
                (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                { text = "Build Support Request"
                , onPress =
                    if String.isEmpty requestDescription then
                        Nothing

                    else
                        Just <| SetNonProjectView <| GetSupport maybeSupportableResource requestDescription True
                }
            , if isSubmitted then
                -- TODO build support request body, show it to user with a "copy to clipboard" button, ask them to paste it into an email message to the email address passed in via flags.
                Element.column
                    [ Element.spacing 10 ]
                    [ Element.paragraph
                        [ Element.spacing 10 ]
                        [ Element.text "Please copy all of the text below and paste it into an email message to: "
                        , Element.el [ Font.extraBold ] <|
                            Style.Widgets.CopyableText.copyableText context.palette [] model.style.userSupportEmail
                        , Element.text "Someone will respond and assist you."
                        ]
                    , Input.multiline
                        (VH.exoElementAttributes
                            ++ [ Element.height <| Element.px 200
                               , Element.width <| Element.maximum 500 Element.fill
                               , Element.spacing 5
                               , Font.family [ Font.monospace ]
                               , Font.size 10
                               ]
                        )
                        { onChange = \_ -> NoOp
                        , text = buildSupportRequest model context maybeSupportableResource requestDescription
                        , placeholder = Nothing
                        , label = Input.labelHidden "Support request"
                        , spellcheck = False
                        }
                    ]

              else
                Element.none
            ]
        ]


supportableItemTypeStr : View.Types.Context -> SupportableItemType -> String
supportableItemTypeStr context supportableItemType =
    case supportableItemType of
        SupportableProject ->
            context.localization.unitOfTenancy

        SupportableImage ->
            context.localization.staticRepresentationOfBlockDeviceContents

        SupportableServer ->
            context.localization.virtualComputer

        SupportableVolume ->
            context.localization.blockDevice


viewStateToSupportableItem : ViewState -> Maybe ( SupportableItemType, Maybe HelperTypes.Uuid )
viewStateToSupportableItem viewState =
    let
        supportableProjectItem :
            ProjectIdentifier
            -> ProjectViewConstructor
            -> ( SupportableItemType, Maybe HelperTypes.Uuid )
        supportableProjectItem projectUuid projectViewConstructor =
            case projectViewConstructor of
                CreateServer createServerViewParams ->
                    ( SupportableImage, Just createServerViewParams.imageUuid )

                ServerDetail serverUuid _ ->
                    ( SupportableServer, Just serverUuid )

                CreateServerImage serverUuid _ ->
                    ( SupportableServer, Just serverUuid )

                VolumeDetail volumeUuid _ ->
                    ( SupportableVolume, Just volumeUuid )

                AttachVolumeModal _ maybeVolumeUuid ->
                    maybeVolumeUuid
                        |> Maybe.map (\uuid -> ( SupportableVolume, Just uuid ))
                        |> Maybe.withDefault ( SupportableProject, Just projectUuid )

                MountVolInstructions attachment ->
                    ( SupportableServer, Just attachment.serverUuid )

                _ ->
                    ( SupportableProject, Just projectUuid )
    in
    case viewState of
        NonProjectView _ ->
            Nothing

        ProjectView projectUuid _ projectViewConstructor ->
            Just <| supportableProjectItem projectUuid projectViewConstructor


buildSupportRequest : Model -> View.Types.Context -> Maybe ( SupportableItemType, Maybe HelperTypes.Uuid ) -> String -> String
buildSupportRequest model context maybeSupportableResource requestDescription =
    String.concat
        [ "# Support Request From "
        , model.style.appTitle
        , "\n\n"
        , "## Applicable Resource\n"
        , case maybeSupportableResource of
            Nothing ->
                "Community member did not specify a resource with this support request."

            Just ( itemType, maybeUuid ) ->
                String.concat
                    [ supportableItemTypeStr context itemType
                    , case maybeUuid of
                        Just uuid ->
                            " with UUID " ++ uuid

                        Nothing ->
                            ""
                    ]
        , "\n\n"
        , "## Request Description\n"
        , requestDescription
        , "\n\n"
        , "## Exosphere Client UUID\n"
        , UUID.toString model.clientUuid
        , "\n\n"
        , String.concat
            [ "## Logged-in "
            , Helpers.String.toTitleCase context.localization.unitOfTenancy
            , "\n"
            ]
        , model.projects
            |> List.map
                (\p ->
                    String.concat
                        [ "- "
                        , p.auth.project.name
                        , " (UUID: "
                        , p.auth.project.uuid
                        , ") as user "
                        , p.auth.user.name
                        , "\n"
                        ]
                )
            |> String.concat
        , "\n"
        , "## Recent Log Messages\n"
        , let
            logMsgStr =
                model.logMessages
                    |> List.map VH.renderMessageAsString
                    |> List.map (\s -> String.append s "\n")
                    |> String.concat
          in
          if logMsgStr == "" then
            "(none)"

          else
            logMsgStr
        ]
