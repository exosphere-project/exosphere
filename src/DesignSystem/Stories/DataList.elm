module DesignSystem.Stories.DataList exposing
    ( Server
    , filters
    , initServers
    , stories
    )

import DesignSystem.Helpers
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import FeatherIcons
import Set
import Style.Helpers
import Style.Types
import Style.Widgets.DataList
import Style.Widgets.Icon
import Style.Widgets.Popover.Types
import UIExplorer
import Widget


type alias DataListArgs msg =
    { renderer : DesignSystem.Helpers.Renderer msg
    , toMsg : Style.Widgets.DataList.Msg -> msg
    , onDeleteServers : Set.Set String -> msg
    , onDeleteServer : String -> msg
    , onPopOver : Style.Widgets.Popover.Types.PopoverId -> msg
    }


stories :
    DataListArgs msg
    ->
        UIExplorer.UI
            (DesignSystem.Helpers.ThemeModel
                { model
                    | dataList : Style.Widgets.DataList.Model
                    , servers : List Server
                    , popover : { showPopovers : Set.Set Style.Widgets.Popover.Types.PopoverId }
                }
            )
            msg
            { note : String }
stories { renderer, toMsg, onDeleteServers, onDeleteServer, onPopOver } =
    UIExplorer.storiesOf
        "DataList"
        [ ( "default"
          , \model ->
                let
                    pallete =
                        DesignSystem.Helpers.palettize model
                in
                renderer pallete <|
                    Style.Widgets.DataList.view model.customModel.dataList
                        toMsg
                        { palette = pallete, showPopovers = model.customModel.popover.showPopovers }
                        [ Element.width (Element.maximum 900 Element.fill)
                        , Element.Font.size 16
                        ]
                        (serverView pallete onDeleteServer)
                        model.customModel.servers
                        [ \serverIds ->
                            Element.el [ Element.alignRight ]
                                (Widget.iconButton
                                    (Style.Helpers.materialStyle pallete).dangerButton
                                    { icon = Style.Widgets.Icon.remove (Element.rgb255 255 255 255) 16
                                    , text = "Delete"
                                    , onPress =
                                        Just <| onDeleteServers serverIds
                                    }
                                )
                        ]
                        (Just { filters = filters, dropdownMsgMapper = onPopOver })
                        (Just searchFilter)
          , { note = """""" }
          )
        ]


searchFilter : Style.Widgets.DataList.SearchFilter { record | name : String }
searchFilter =
    { label = "Search by name:"
    , placeholder = Just "try 'my-server'"
    , textToSearch = .name
    }


serverView : Style.Types.ExoPalette -> (String -> msg) -> Server -> Element.Element msg
serverView palette onDelete server =
    let
        statusColor =
            if server.ready then
                Element.rgb255 125 194 5

            else
                Element.rgb255 187 187 187

        interactionButton =
            Widget.iconButton
                (Style.Helpers.materialStyle palette).button
                { text = "Connect to"
                , icon =
                    Element.row
                        [ Element.spacing 5 ]
                        [ Element.text "Connect to"
                        , Element.el []
                            (FeatherIcons.chevronDown
                                |> FeatherIcons.withSize 18
                                |> FeatherIcons.toHtml []
                                |> Element.html
                            )
                        ]
                , onPress = Nothing
                }

        deleteServerButton =
            Widget.iconButton
                (Style.Helpers.materialStyle palette).dangerButton
                { icon = Style.Widgets.Icon.remove (Element.rgb255 255 255 255) 16
                , text = "Delete"
                , onPress =
                    if server.selectable then
                        Just <| onDelete server.id

                    else
                        -- to disable it
                        Nothing
                }
    in
    Element.column
        [ Element.spacing 12
        , Element.width Element.fill
        ]
        [ Element.row [ Element.spacing 10, Element.width Element.fill ]
            [ Element.el
                [ Element.Font.size 18
                , Element.Font.color (Style.Helpers.toElementColor palette.primary)
                ]
                (Element.text server.name)
            , Element.el
                [ Element.width (Element.px 12)
                , Element.height (Element.px 12)
                , Element.Border.rounded 6
                , Element.Background.color statusColor
                ]
                Element.none
            , Element.el [ Element.alignRight ]
                interactionButton
            , Element.el [ Element.alignRight ]
                deleteServerButton
            ]
        , Element.row
            [ Element.spacing 8
            , Element.width Element.fill
            , Element.Font.color <| Style.Helpers.toElementColor palette.muted.textOnNeutralBG
            ]
            [ Element.el [] (Element.text server.size)
            , Element.text "·"
            , Element.paragraph []
                [ Element.text "created "
                , Element.el [ Element.Font.color (Style.Helpers.toElementColor palette.neutral.text.default) ]
                    (Element.text server.creationTime.relativeTime)
                , Element.text " by "
                , Element.el [ Element.Font.color (Style.Helpers.toElementColor palette.neutral.text.default) ]
                    (Element.text server.creator)
                ]
            , Style.Widgets.Icon.ipAddress
                (Style.Helpers.toElementColor palette.muted.textOnNeutralBG)
                16
            , Element.el [] (Element.text server.ip)
            ]
        ]


type alias Server =
    Style.Widgets.DataList.DataRecord
        { name : String
        , creator : String
        , creationTime :
            { timestamp : Int
            , relativeTime : String
            }
        , ready : Bool
        , size : String
        , ip : String
        }


filters :
    List
        (Style.Widgets.DataList.Filter
            { record
                | creator : String
                , creationTime : { a | timestamp : Int }
            }
        )
filters =
    let
        currentUser =
            "ex3"

        creatorFilterOptionValues servers =
            List.map .creator servers
                |> Set.fromList
                |> Set.toList

        creationTimeFilterOptions =
            [ ( "1642501203000", "past day" )
            , ( "1641982803000", "past 7 days" )
            , ( "1639909203000", "past 30 days" )
            ]
    in
    [ { id = "creator"
      , label = "Creator"
      , chipPrefix = "Created by "
      , filterOptions =
            \servers ->
                creatorFilterOptionValues servers
                    |> List.map
                        (\creator ->
                            ( creator
                            , if creator == currentUser then
                                "me (" ++ creator ++ ")"

                              else
                                creator
                            )
                        )
                    |> Dict.fromList
      , filterTypeAndDefaultValue = Style.Widgets.DataList.MultiselectOption (Set.fromList [ currentUser ])
      , onFilter =
            \optionValue server ->
                server.creator == optionValue
      }
    , { id = "creationTime"
      , label = "Created within"
      , chipPrefix = "Created within "
      , filterOptions =
            \_ -> Dict.fromList creationTimeFilterOptions
      , filterTypeAndDefaultValue = Style.Widgets.DataList.UniselectOption Style.Widgets.DataList.UniselectNoChoice
      , onFilter =
            \optionValue server ->
                let
                    optionInTimestamp =
                        Maybe.withDefault 0 (String.toInt optionValue)
                in
                server.creationTime.timestamp >= optionInTimestamp
      }
    ]


initServers : List Server
initServers =
    [ { name = "kindly_mighty_katydid"
      , creator = "ex3"
      , creationTime = { timestamp = 1642544403000, relativeTime = "12 hours ago" }
      , ready = True
      , size = "m1.tiny"
      , ip = "129.114.104.147"
      , id = "rtbdf"
      , selectable = True
      }
    , { name = "cheaply_next_crab"
      , creator = "tg3456"
      , creationTime = { timestamp = 1642155016000, relativeTime = "5 days ago" }
      , ready = False
      , size = "m1.medium"
      , ip = "129.114.104.148"
      , id = "tyh43d"
      , selectable = False
      }
    , { name = "basically_well_cobra"
      , creator = "ex3"
      , creationTime = { timestamp = 1639909203000, relativeTime = "1 month ago" }
      , ready = True
      , size = "g1.v100x"
      , ip = "129.114.104.149"
      , id = "vcb543f"
      , selectable = True
      }
    , { name = "adorably_grumpy_cat"
      , creator = "tg3456"
      , creationTime = { timestamp = 1637317203000, relativeTime = "2 months ago" }
      , ready = True
      , size = "g1.v100x"
      , ip = "129.114.104.139"
      , id = "werfdse"
      , selectable = True
      }
    ]
