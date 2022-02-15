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



--- MAIN


main : UIExplorerProgram {} () {}
main =
    exploreWithCategories
        defaultConfig
        (createCategories
            |> category "Atoms"
                [ storiesOf
                    "Text"
                    [ ( "Default", \_ -> toHtml <| Element.text "Connect to...", {} )
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
