module Page.ServerConsoleLog exposing (LineLimit(..), Model, Msg, init, lineLimitToMaybeInt, receiveConsoleLog, update, view)

import Ansi.Log
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Html
import Html.Attributes
import OpenStack.ConsoleLog
import OpenStack.Types as OSTypes
import Route
import Style.Helpers as SH
import Style.Widgets.Button as Button
import Style.Widgets.CopyableText exposing (copyableTextAccessory)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Time
import Types.Error exposing (HttpErrorWithBody)
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { serverUuid : OSTypes.ServerUuid
    , lineLimit : LineLimit
    , consoleLog : RDPP.RemoteDataPlusPlus HttpErrorWithBody String
    , renderAnsi : Bool
    }


type LineLimit
    = LastLines Int
    | AllLines


type Msg
    = GotLineLimit LineLimit
    | GotRefresh
    | GotRenderAnsi Bool
    | SharedMsg SharedMsg.SharedMsg


init : OSTypes.ServerUuid -> Model
init serverUuid =
    { serverUuid = serverUuid
    , lineLimit = LastLines 200
    , consoleLog = RDPP.empty
    , renderAnsi = True
    }


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotLineLimit lineLimit ->
            let
                newModel =
                    { model
                        | lineLimit = lineLimit
                        , consoleLog = RDPP.setLoading model.consoleLog
                    }
            in
            ( newModel
            , requestConsoleLog project newModel
            , SharedMsg.NoOp
            )

        GotRefresh ->
            let
                newModel =
                    { model | consoleLog = RDPP.setLoading model.consoleLog }
            in
            ( newModel
            , requestConsoleLog project newModel
            , SharedMsg.NoOp
            )

        GotRenderAnsi renderAnsi ->
            ( { model | renderAnsi = renderAnsi }, Cmd.none, SharedMsg.NoOp )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


requestConsoleLog : Project -> Model -> Cmd Msg
requestConsoleLog project model =
    OpenStack.ConsoleLog.requestUserConsoleLog
        project
        model.serverUuid
        (lineLimitToMaybeInt model.lineLimit)
        |> Cmd.map SharedMsg


receiveConsoleLog : Result HttpErrorWithBody String -> Time.Posix -> Model -> Model
receiveConsoleLog result time model =
    case result of
        Ok consoleLog ->
            { model
                | consoleLog =
                    RDPP.RemoteDataPlusPlus
                        (RDPP.DoHave consoleLog time)
                        (RDPP.NotLoading Nothing)
            }

        Err error ->
            { model
                | consoleLog =
                    RDPP.RemoteDataPlusPlus
                        model.consoleLog.data
                        (RDPP.NotLoading (Just ( error, time )))
            }


lineLimitToMaybeInt : LineLimit -> Maybe Int
lineLimitToMaybeInt lineLimit =
    case lineLimit of
        LastLines lines ->
            Just lines

        AllLines ->
            Nothing


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project _ model =
    let
        maybeServer =
            GetterSetters.serverLookup project model.serverUuid

        serverName =
            maybeServer
                |> Maybe.map (.osProps >> .name)
                |> Maybe.withDefault model.serverUuid
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing spacer.px24
        ]
        [ Text.heading context.palette
            []
            Element.none
            "Console Log"
        , Element.paragraph []
            [ Element.text <|
                String.join " "
                    [ "Console output for"
                    , context.localization.virtualComputer
                    , serverName
                    ]
            ]
        , Element.link []
            { url =
                Route.toUrl context.urlPathPrefix <|
                    Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                        Route.ServerDetail model.serverUuid
            , label =
                Text.text Text.Body
                    [ Font.color (SH.toElementColor context.palette.primary) ]
                    ("Back to " ++ context.localization.virtualComputer)
            }
        , controls context model
        , VH.renderRDPP context model.consoleLog "console log" (renderConsoleLog context model)
        ]


controls : View.Types.Context -> Model -> Element.Element Msg
controls context model =
    Element.column [ Element.spacing spacer.px16, Element.width Element.fill ]
        [ Input.radioRow [ Element.spacing spacer.px16 ]
            { onChange = GotLineLimit
            , selected = Just model.lineLimit
            , label = Input.labelLeft [] (Element.text "Lines")
            , options =
                [ Input.option (LastLines 50) (Element.text "50")
                , Input.option (LastLines 200) (Element.text "200")
                , Input.option (LastLines 500) (Element.text "500")
                , Input.option (LastLines 1000) (Element.text "1000")
                , Input.option AllLines (Element.text "All")
                ]
            }
        , Input.checkbox []
            { onChange = GotRenderAnsi
            , icon = Input.defaultCheckbox
            , checked = model.renderAnsi
            , label = Input.labelRight [] (Element.text "Render ANSI colors and control sequences")
            }
        , Element.row [ Element.width Element.fill ]
            [ Element.el [ Element.alignRight ] <|
                Button.default
                    context.palette
                    { text = "Refresh"
                    , onPress = Just GotRefresh
                    }
            ]
        ]


renderConsoleLog : View.Types.Context -> Model -> String -> Element.Element Msg
renderConsoleLog context model consoleLog =
    if String.isEmpty consoleLog then
        Element.text "No console output returned."

    else if model.renderAnsi then
        ansiConsoleBlock context consoleLog

    else
        plainConsoleBlock context consoleLog


plainConsoleBlock : View.Types.Context -> String -> Element.Element Msg
plainConsoleBlock context consoleLog =
    let
        copyable =
            copyableTextAccessory context.palette consoleLog
    in
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.maximum 640 Element.fill
        , Element.scrollbars
        , Element.padding spacer.px16
        , Border.rounded spacer.px4
        , Border.width 1
        , Border.color (SH.toElementColor context.palette.neutral.border)
        , Background.color (SH.toElementColor context.palette.neutral.background.frontLayer)
        , copyable.id
        , Element.inFront <|
            Element.el
                [ Element.alignRight
                , Element.moveLeft <| toFloat spacer.px8
                , Element.moveDown <| toFloat spacer.px8
                ]
                copyable.accessory
        ]
    <|
        Element.html <|
            Html.pre
                [ Html.Attributes.style "margin" "0"
                , Html.Attributes.style "white-space" "pre-wrap"
                , Html.Attributes.style "overflow-wrap" "anywhere"
                , Html.Attributes.style "word-break" "break-word"
                , Html.Attributes.style "font-family" "monospace"
                ]
                [ Html.text consoleLog ]


ansiConsoleBlock : View.Types.Context -> String -> Element.Element Msg
ansiConsoleBlock context consoleLog =
    let
        copyable =
            copyableTextAccessory context.palette consoleLog

        ansiLog =
            Ansi.Log.update consoleLog (Ansi.Log.init Ansi.Log.Cooked)
    in
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.maximum 640 Element.fill
        , Element.scrollbars
        , Element.padding spacer.px16
        , Border.rounded spacer.px4
        , Border.width 1
        , Border.color (SH.toElementColor context.palette.neutral.border)
        , Background.color (SH.toElementColor context.palette.neutral.background.frontLayer)
        , Element.inFront <|
            Element.el
                [ Element.alignRight
                , Element.moveLeft <| toFloat spacer.px8
                , Element.moveDown <| toFloat spacer.px8
                ]
                copyable.accessory
        ]
    <|
        Element.column []
            [ Element.el
                [ copyable.id
                , Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
                , Element.htmlAttribute <| Html.Attributes.style "left" "-10000px"
                , Element.htmlAttribute <| Html.Attributes.style "width" "1px"
                , Element.htmlAttribute <| Html.Attributes.style "height" "1px"
                , Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                ]
                (Element.text consoleLog)
            , Element.html <|
                Html.div
                    [ Html.Attributes.class "console-log" ]
                    [ Ansi.Log.view ansiLog ]
            ]
