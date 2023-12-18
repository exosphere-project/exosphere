module Page.GetSupport exposing (Model, Msg(..), headerView, init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Set
import Style.Widgets.Button as Button
import Style.Widgets.CopyableText exposing (copyableTextAccessory)
import Style.Widgets.Select
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text exposing (TextVariant(..))
import Types.HelperTypes as HelperTypes
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import UUID
import Url.Builder
import View.Helpers as VH
import View.Types


type alias Model =
    { maybeSupportableResource : Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid )
    , requestDescription : String
    , isSubmitted : Bool
    }


type Msg
    = GotResourceType (Maybe HelperTypes.SupportableItemType)
    | GotResourceUuid (Maybe HelperTypes.Uuid)
    | GotDescription String
    | GotSubmittedForm Bool
    | NoOp


init : Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid ) -> Model
init maybeSupportableResource =
    { maybeSupportableResource = maybeSupportableResource
    , requestDescription = ""
    , isSubmitted = False
    }


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        GotResourceType maybeItemType ->
            ( { model
                | maybeSupportableResource =
                    Maybe.andThen (\itemType -> Just ( itemType, Nothing )) maybeItemType
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotResourceUuid maybeUuid ->
            let
                newModel =
                    case model.maybeSupportableResource of
                        Nothing ->
                            model

                        Just ( resourceType, _ ) ->
                            { model | maybeSupportableResource = Just ( resourceType, maybeUuid ) }
            in
            ( newModel, Cmd.none, SharedMsg.NoOp )

        GotDescription desc ->
            ( { model | requestDescription = desc }, Cmd.none, SharedMsg.NoOp )

        GotSubmittedForm submitted ->
            ( { model | isSubmitted = submitted }, Cmd.none, SharedMsg.NoOp )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


headerView : View.Types.Context -> SharedModel -> Element.Element msg
headerView context sharedModel =
    Text.heading context.palette
        VH.headerHeadingAttributes
        Element.none
        ("Get Support for " ++ sharedModel.style.appTitle)


viewSupportInfo : View.Types.Context -> SharedModel -> Element.Element Msg
viewSupportInfo context sharedModel =
    case sharedModel.style.supportInfoMarkdown of
        Just markdown ->
            Element.column [] <|
                VH.renderMarkdown context markdown

        Nothing ->
            Element.none


viewSupportForm : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
viewSupportForm context sharedModel model =
    Element.column [ Element.spacing spacer.px32, Element.width Element.fill ]
        [ Input.radio
            [ Element.spacing spacer.px12 ]
            { onChange =
                GotResourceType
            , selected =
                model.maybeSupportableResource
                    |> Maybe.map Tuple.first
                    |> Just
            , label =
                Input.labelAbove VH.radioLabelAttributes
                    (Element.text "What do you need help with?")
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
                    [ HelperTypes.SupportableServer
                    , HelperTypes.SupportableShare
                    , HelperTypes.SupportableVolume
                    , HelperTypes.SupportableImage
                    , HelperTypes.SupportableProject
                    ]
                    ++ [ Input.option Nothing (Element.text "None of these things") ]
            }
        , case model.maybeSupportableResource of
            Nothing ->
                Element.none

            Just ( supportableItemType, maybeSupportableItemUuid ) ->
                Element.column [ Element.spacing spacer.px12 ]
                    [ Element.text <|
                        String.join " "
                            [ "Which"
                            , supportableItemTypeStr context supportableItemType
                            , "do you need help with?"
                            ]
                    , let
                        options =
                            case supportableItemType of
                                HelperTypes.SupportableProject ->
                                    sharedModel.projects
                                        |> List.map
                                            (\proj ->
                                                ( proj.auth.project.uuid
                                                , VH.friendlyProjectTitle sharedModel proj
                                                )
                                            )

                                HelperTypes.SupportableImage ->
                                    sharedModel.projects
                                        |> List.map .images
                                        |> List.map (RDPP.withDefault [])
                                        |> List.concat
                                        |> List.map
                                            (\image ->
                                                ( image.uuid
                                                , VH.resourceName (Just image.name) image.uuid
                                                )
                                            )
                                        -- This removes duplicate values, heh
                                        |> Set.fromList
                                        |> Set.toList
                                        |> List.sortBy Tuple.second

                                HelperTypes.SupportableServer ->
                                    sharedModel.projects
                                        |> List.map .servers
                                        |> List.map (RDPP.withDefault [])
                                        |> List.concat
                                        |> List.map
                                            (\server ->
                                                ( server.osProps.uuid
                                                , VH.resourceName (Just server.osProps.name) server.osProps.uuid
                                                )
                                            )
                                        |> List.sortBy Tuple.second

                                HelperTypes.SupportableShare ->
                                    sharedModel.projects
                                        |> List.map .shares
                                        |> List.map (RDPP.withDefault [])
                                        |> List.concat
                                        |> List.map
                                            (\share ->
                                                ( share.uuid
                                                , VH.resourceName share.name share.uuid
                                                )
                                            )
                                        |> List.sortBy Tuple.second

                                HelperTypes.SupportableVolume ->
                                    sharedModel.projects
                                        |> List.map .volumes
                                        |> List.map (RDPP.withDefault [])
                                        |> List.concat
                                        |> List.map
                                            (\volume ->
                                                ( volume.uuid
                                                , VH.resourceName volume.name volume.uuid
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
                        context.palette
                        { onChange = GotResourceUuid
                        , options = options
                        , selected = maybeSupportableItemUuid
                        , label = label
                        }
                    ]
        , Input.multiline
            (VH.inputItemAttributes context.palette
                ++ [ Element.height <| Element.px 200
                   , Element.width Element.fill
                   ]
            )
            { onChange = GotDescription
            , text = model.requestDescription
            , placeholder = Nothing
            , label = Input.labelAbove [] (Element.text "Please describe exactly what you need help with.")
            , spellcheck = True
            }
        , Element.row
            [ Element.width Element.fill ]
            [ Element.el [ Element.alignRight ] <|
                Button.primary
                    context.palette
                    { text = "Build Support Request"
                    , onPress =
                        if String.isEmpty model.requestDescription then
                            Nothing

                        else
                            Just (GotSubmittedForm True)
                    }
            ]
        ]


viewBuiltSupportRequest : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
viewBuiltSupportRequest context sharedModel model =
    let
        boldCopyableText text =
            Element.el [ Font.extraBold ] <|
                Style.Widgets.CopyableText.copyableText context.palette [] text

        supportRequest =
            buildSupportRequest sharedModel context model.maybeSupportableResource model.requestDescription

        copyable =
            copyableTextAccessory context.palette supportRequest
    in
    Element.column
        [ Element.spacing spacer.px32, Element.width Element.fill ]
        [ Text.p
            []
            [ Element.text "To request support, click this button and send the resulting message:" ]
        , buildMailtoLink sharedModel context model
        , Text.p
            []
          <|
            List.concat
                [ [ Element.text "If the button does not work, please copy all of the text in the box below and paste it into an email message to: " ]
                , [ boldCopyableText sharedModel.style.userSupportEmailAddress
                  ]
                , case sharedModel.style.userSupportEmailSubject of
                    Just subject ->
                        [ Element.text "Please also include the following text in the subject line: "
                        , boldCopyableText subject
                        ]

                    Nothing ->
                        []
                , [ Element.text "Someone will respond and assist you." ]
                ]
        , Element.row
            [ Element.spacing spacer.px8 ]
            [ Input.multiline
                (VH.inputItemAttributes context.palette
                    ++ [ Element.height <| Element.px 200
                       , Element.width Element.fill
                       , Element.spacing spacer.px8
                       , Text.fontFamily Text.Mono
                       ]
                    ++ Text.typographyAttrs Tiny
                    ++ [ copyable.id ]
                )
                { onChange = \_ -> NoOp
                , text = supportRequest
                , placeholder = Nothing
                , label = Input.labelHidden "Support request"
                , spellcheck = False
                }
            , copyable.accessory
            ]
        , Element.row
            [ Element.width Element.fill ]
            [ Element.el [] <|
                Button.default
                    context.palette
                    { text = "Edit Support Request"
                    , onPress =
                        if String.isEmpty model.requestDescription then
                            Nothing

                        else
                            Just (GotSubmittedForm False)
                    }
            ]
        ]


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel model =
    Element.column
        (VH.formContainer ++ [ Element.spacing spacer.px32 ])
    <|
        if model.isSubmitted then
            [ viewBuiltSupportRequest context sharedModel model ]

        else
            [ viewSupportInfo context sharedModel
            , viewSupportForm context sharedModel model
            ]


supportableItemTypeStr : View.Types.Context -> HelperTypes.SupportableItemType -> String
supportableItemTypeStr context supportableItemType =
    case supportableItemType of
        HelperTypes.SupportableProject ->
            context.localization.unitOfTenancy

        HelperTypes.SupportableImage ->
            context.localization.staticRepresentationOfBlockDeviceContents

        HelperTypes.SupportableServer ->
            context.localization.virtualComputer

        HelperTypes.SupportableShare ->
            context.localization.share

        HelperTypes.SupportableVolume ->
            context.localization.blockDevice


buildMailtoLink : SharedModel -> View.Types.Context -> Model -> Element.Element Msg
buildMailtoLink sharedModel context model =
    let
        emailBody =
            buildSupportRequest sharedModel context model.maybeSupportableResource model.requestDescription

        queryParams =
            [ Url.Builder.string "body" emailBody
            , Url.Builder.string "subject" (Maybe.withDefault "" sharedModel.style.userSupportEmailSubject)
            ]

        target =
            "mailto:"
                ++ sharedModel.style.userSupportEmailAddress
                ++ Url.Builder.toQuery queryParams

        button =
            Button.primary
                context.palette
                { text = "Generate Email"
                , onPress = Just NoOp
                }
    in
    Element.link [] { url = target, label = button }


buildSupportRequest : SharedModel -> View.Types.Context -> Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid ) -> String -> String
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
