module DesignSystem.Stories.Alert exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Style.Widgets.Alert exposing (AlertState(..), alert, notes)
import Style.Widgets.Spacer exposing (spacer)
import UIExplorer exposing (storiesOf)


stories : Renderer msg -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf "Alerts"
        ([ ( "Info", Info )
         , ( "Success", Success )
         , ( "Warning", Warning )
         , ( "Danger", Danger )
         ]
            |> List.map
                (\( name, state ) ->
                    ( name
                    , \m ->
                        let
                            palette =
                                palettize m
                        in
                        renderer palette <|
                            Element.column [ Element.spacing spacer.px12 ] <|
                                List.map
                                    (\( showContainer, showIcon ) ->
                                        alert []
                                            palette
                                            { state = state
                                            , showContainer = showContainer
                                            , showIcon = showIcon
                                            , content =
                                                Element.text <|
                                                    ("This is a "
                                                        ++ name
                                                        ++ " alert"
                                                        ++ (if showContainer && showIcon then
                                                                " with a container and an icon"

                                                            else if showContainer then
                                                                " with a container"

                                                            else if showIcon then
                                                                " with an icon"

                                                            else
                                                                ""
                                                           )
                                                    )
                                            }
                                    )
                                    [ ( False, False ), ( False, True ), ( True, False ), ( True, True ) ]
                    , { note = notes }
                    )
                )
        )
