module DesignSystem.Explorer exposing (main)

import Html
import UIExplorer
    exposing
        ( UIExplorerProgram
        , category
        , createCategories
        , defaultConfig
        , exploreWithCategories
        , storiesOf
        )


main : UIExplorerProgram {} () {}
main =
    exploreWithCategories
        defaultConfig
        (createCategories
            |> category "Atoms"
                [ storiesOf
                    "Text"
                    [ ( "Default", \_ -> Html.text "//TODO: Add components to this section.", {} )
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
