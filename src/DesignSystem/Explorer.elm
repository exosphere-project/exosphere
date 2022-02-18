module DesignSystem.Explorer exposing (main)

import Element exposing (text)
import Html
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Icon exposing (bell, console, copyToClipboard, history, ipAddress, lock, lockOpen, plusCircle, remove, roundRect, timesCircle)
import UIExplorer
    exposing
        ( UIExplorerProgram
        , category
        , createCategories
        , defaultConfig
        , exploreWithCategories
        , storiesOf
        )
import Widget exposing (textButton)



--- elm-ui helpers


toHtml : Element.Element msg -> Html.Html msg
toHtml a =
    Element.layout [] <| a



--- theme


palette : Style.Types.ExoPalette
palette =
    SH.toExoPalette
        Style.Types.defaultColors
        { theme = Style.Types.Override Style.Types.Light, systemPreference = Nothing }



--- styling


defaultIcon : (Element.Color -> number -> icon) -> icon
defaultIcon icon =
    icon (palette.on.background |> SH.toElementColor) 25



--- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )



--- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



--- MAIN


main : UIExplorerProgram {} Msg {}
main =
    exploreWithCategories
        defaultConfig
        (createCategories
            |> category "Atoms"
                [ storiesOf
                    "Text"
                    [ ( "body", \_ -> toHtml <| Element.text "Connect to...", {} )
                    ]
                , storiesOf
                    "Icon"
                    [ ( "bell", \_ -> toHtml <| defaultIcon <| bell, {} )
                    , ( "console", \_ -> toHtml <| defaultIcon <| console, {} )
                    , ( "copyToClipboard", \_ -> toHtml <| defaultIcon <| copyToClipboard, {} )
                    , ( "history", \_ -> toHtml <| defaultIcon <| history, {} )
                    , ( "ipAddress", \_ -> toHtml <| defaultIcon <| ipAddress, {} )
                    , ( "lock", \_ -> toHtml <| defaultIcon <| lock, {} )
                    , ( "lockOpen", \_ -> toHtml <| defaultIcon <| lockOpen, {} )
                    , ( "plusCircle", \_ -> toHtml <| defaultIcon <| plusCircle, {} )
                    , ( "remove", \_ -> toHtml <| defaultIcon <| remove, {} )
                    , ( "roundRect", \_ -> toHtml <| defaultIcon <| roundRect, {} )
                    , ( "timesCircle", \_ -> toHtml <| defaultIcon <| timesCircle, {} )
                    ]
                , storiesOf
                    "Button"
                    [ ( "primary"
                      , \_ ->
                            toHtml <|
                                Widget.textButton
                                    (SH.materialStyle palette).primaryButton
                                    { text = "Create", onPress = Just NoOp }
                      , {}
                      )
                    , ( "disabled"
                      , \_ ->
                            toHtml <|
                                Widget.textButton
                                    (SH.materialStyle palette).primaryButton
                                    { text = "Next", onPress = Nothing }
                      , {}
                      )
                    , ( "plain"
                      , \_ ->
                            toHtml <|
                                Widget.textButton
                                    (SH.materialStyle palette).button
                                    { text = "Next", onPress = Just NoOp }
                      , {}
                      )
                    , ( "warning"
                      , \_ ->
                            toHtml <|
                                Widget.textButton
                                    (SH.materialStyle palette).warningButton
                                    { text = "Suspend", onPress = Just NoOp }
                      , {}
                      )
                    , ( "danger"
                      , \_ ->
                            toHtml <|
                                Widget.textButton
                                    (SH.materialStyle palette).dangerButton
                                    { text = "Delete All", onPress = Just NoOp }
                      , {}
                      )
                    , ( "danger secondary"
                      , \_ ->
                            toHtml <|
                                Widget.textButton
                                    (SH.materialStyle palette).dangerButtonSecondary
                                    { text = "Delete All", onPress = Just NoOp }
                      , {}
                      )
                    ]
                ]
            |> category "Molecules"
                [ storiesOf
                    "Card"
                    [ ( "Default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                , storiesOf
                    "Input"
                    [ ( "Default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                ]
            |> category "Organisms"
                [ storiesOf
                    "Lists"
                    [ ( "Default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                ]
            |> category "Templates"
                [ storiesOf
                    "Login"
                    [ ( "Default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                , storiesOf
                    "Create"
                    [ ( "Default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                , storiesOf
                    "List"
                    [ ( "Default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                , storiesOf
                    "Detail"
                    [ ( "Default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                ]
            |> category "Pages"
                [ storiesOf
                    "Jetstream2"
                    [ ( "Default", \_ -> Html.text "//TODO: Add components to this section.", {} )
                    ]
                ]
        )
