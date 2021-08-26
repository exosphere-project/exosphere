module Page.GetSupport exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Ports
import RemoteData
import Set
import Style.Helpers as SH
import Style.Widgets.CopyableText
import Style.Widgets.Select
import Types.HelperTypes as HelperTypes
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg exposing (SharedMsg(..))
import UUID
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { maybeSupportableResource : Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid )
    , requestDescription : String
    , isSubmitted : Bool
    }


type Msg
    = GotResourceType (Maybe HelperTypes.SupportableItemType)
    | GotResourceUuid (Maybe HelperTypes.Uuid)
    | GotDescription String
    | GotSubmit
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

        GotSubmit ->
            ( { model | isSubmitted = True }, Cmd.none, SharedMsg.NoOp )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel model =
    Element.column
        (VH.exoColumnAttributes
            ++ [ Element.spacing 30
               , Element.width Element.fill
               ]
        )
        [ Element.row
            (VH.heading2 context.palette ++ [ Element.spacing 12 ])
            [ FeatherIcons.helpCircle
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
            , Element.text ("Get Support for " ++ sharedModel.style.appTitle)
            ]
        , case sharedModel.style.supportInfoMarkdown of
            Just markdown ->
                Element.column VH.contentContainer <|
                    VH.renderMarkdown context markdown

            Nothing ->
                Element.none
        , Element.column VH.formContainer
            [ Input.radio
                VH.exoColumnAttributes
                { onChange =
                    GotResourceType
                , selected =
                    model.maybeSupportableResource
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
                        [ HelperTypes.SupportableServer
                        , HelperTypes.SupportableVolume
                        , HelperTypes.SupportableImage
                        , HelperTypes.SupportableProject
                        ]
                        ++ [ Input.option Nothing (Element.text "None of these things") ]
                }
            , case model.maybeSupportableResource of
                Nothing ->
                    Element.none

                Just ( supportableItemType, _ ) ->
                    Element.text <|
                        String.join " "
                            [ "Which"
                            , supportableItemTypeStr context supportableItemType
                            , "do you need help with?"
                            ]
            , case model.maybeSupportableResource of
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
                            GotResourceUuid newMaybeSupportableItemUuid

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

                                HelperTypes.SupportableServer ->
                                    sharedModel.projects
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

                                HelperTypes.SupportableVolume ->
                                    sharedModel.projects
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
                    Widget.textButton
                        (SH.materialStyle context.palette).primaryButton
                        { text = "Build Support Request"
                        , onPress =
                            if String.isEmpty model.requestDescription then
                                Nothing

                            else
                                Just GotSubmit
                        }
                ]
            , if model.isSubmitted then
                Element.column
                    [ Element.spacing 10, Element.width Element.fill ]
                    [ Element.paragraph
                        [ Element.spacing 10 ]
                        [ Element.text "Please copy all of the text below and paste it into an email message to: "
                        , Element.el [ Font.extraBold ] <|
                            Style.Widgets.CopyableText.copyableText context.palette [] sharedModel.style.userSupportEmail
                        , Element.text "Someone will respond and assist you."
                        ]
                    , Input.multiline
                        (VH.exoElementAttributes
                            ++ [ Element.height <| Element.px 200
                               , Element.width Element.fill
                               , Element.spacing 5
                               , Font.family [ Font.monospace ]
                               , Font.size 10
                               ]
                        )
                        { onChange = \_ -> NoOp
                        , text = buildSupportRequest sharedModel context model.maybeSupportableResource model.requestDescription
                        , placeholder = Nothing
                        , label = Input.labelHidden "Support request"
                        , spellcheck = False
                        }
                    ]

              else
                Element.none
            ]
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

        HelperTypes.SupportableVolume ->
            context.localization.blockDevice


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
