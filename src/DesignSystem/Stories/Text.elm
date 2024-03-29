module DesignSystem.Stories.Text exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Element.Font as Font
import FeatherIcons as Icons
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Link as Link
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text exposing (TextVariant(..))
import UIExplorer
    exposing
        ( storiesOf
        )


stories : Renderer msg -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf
        "Text"
        [ ( "text variants"
          , \m ->
                renderer (palettize m) <|
                    Element.column [ Element.spacing spacer.px16 ] <|
                        List.map (\variant -> Text.text (Tuple.first variant) [] (Tuple.second variant))
                            [ ( AppTitle, "AppTitle" )
                            , ( ExtraLarge, "ExtraLarge" )
                            , ( Large, "Large" )
                            , ( Emphasized, "Emphasized" )
                            , ( Body, "Body" )
                            , ( Small, "Small" )
                            , ( Tiny, "Tiny" )
                            ]
          , { note = Text.notes }
          )
        , ( "unstyled", \m -> renderer (palettize m) <| Element.text "This is text rendered using `Element.text` and no styling. It will inherit attributes from the document layout.", { note = Text.notes } )
        , ( "p"
          , \m ->
                renderer (palettize m) <|
                    Text.p [ Font.justify ]
                        [ Text.body veryLongCopy
                        , Text.body "[ref. "
                        , Link.externalLink (palettize m) "https://www.lipsum.com/" "www.lipsum.com"
                        , Text.body "]"
                        ]
          , { note = Text.notes }
          )
        , ( "strong", \m -> renderer (palettize m) <| Text.p [] [ Text.body "Logged in as ", Text.strong "@Jimmy:3421", Text.body "." ], { note = Text.notes } )
        , ( "mono", \m -> renderer (palettize m) <| Text.p [] [ Text.body "Your IP address is ", Text.mono "192.168.1.1", Text.body "." ], { note = Text.notes } )
        , ( "underline"
          , \m ->
                renderer (palettize m) <|
                    Text.p []
                        [ Text.body "Exosphere is a "
                        , Text.underline "user-friendly"
                        , Text.body ", extensible client for cloud computing."
                        ]
          , { note = Text.notes }
          )
        , ( "heading"
          , \m ->
                renderer (palettize m) <|
                    Text.heading (palettize m)
                        []
                        (featherIcon [] Icons.helpCircle)
                        "Get Support"
          , { note = Text.notes }
          )
        , ( "subheading"
          , \m ->
                renderer (palettize m) <|
                    Text.subheading (palettize m)
                        []
                        (featherIcon [] Icons.hardDrive)
                        "Volumes"
          , { note = Text.notes }
          )
        ]


veryLongCopy : String
veryLongCopy =
    """
    Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC,
    making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words,
    consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source.
    Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC.
    This book is a treatise on the theory of ethics, very popular during the Renaissance.
    """
