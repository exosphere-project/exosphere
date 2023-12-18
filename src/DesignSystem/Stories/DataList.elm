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
import FeatherIcons as Icons
import Set
import Style.Helpers
import Style.Types
import Style.Widgets.DataList
import Style.Widgets.HumanTime exposing (relativeTimeElement)
import Style.Widgets.Icon exposing (sizedFeatherIcon)
import Style.Widgets.Popover.Types
import Style.Widgets.Text
import Time
import Time.Extra exposing (Interval(..))
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
                    , predefinedNow : Time.Posix
                    , servers : List Server
                    , popover : { showPopovers : Set.Set Style.Widgets.Popover.Types.PopoverId }
                }
            )
            msg
            { note : String }
stories { renderer, toMsg, onDeleteServers, onDeleteServer, onPopOver } =
    let
        renderModel model =
            let
                now : Time.Posix
                now =
                    model.customModel.predefinedNow

                palette =
                    DesignSystem.Helpers.palettize model
            in
            renderer palette <|
                Style.Widgets.DataList.view
                    "server"
                    model.customModel.dataList
                    toMsg
                    { palette = palette, showPopovers = model.customModel.popover.showPopovers }
                    [ Element.width (Element.maximum 900 Element.fill)
                    , Style.Widgets.Text.fontSize Style.Widgets.Text.Body
                    ]
                    (serverView palette onDeleteServer now)
                    model.customModel.servers
                    [ \serverIds ->
                        Element.el [ Element.alignRight ]
                            (Widget.iconButton
                                (Style.Helpers.materialStyle palette).dangerButton
                                { icon = Style.Widgets.Icon.remove (Element.rgb255 255 255 255) 16
                                , text = "Delete"
                                , onPress =
                                    Just <| onDeleteServers serverIds
                                }
                            )
                    ]
                    (Just { filters = filters now, dropdownMsgMapper = onPopOver })
                    (Just searchFilter)
    in
    UIExplorer.storiesOf "DataList"
        [ ( "default", renderModel, { note = """""" } ) ]


searchFilter : Style.Widgets.DataList.SearchFilter { record | name : String }
searchFilter =
    { label = "Search by name:"
    , placeholder = Just "try 'my-server'"
    , textToSearch = .name
    }


serverView : Style.Types.ExoPalette -> (String -> msg) -> Time.Posix -> Server -> Element.Element msg
serverView palette onDelete now server =
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
                        , sizedFeatherIcon 18 Icons.chevronDown
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
                (Style.Widgets.Text.typographyAttrs Style.Widgets.Text.Emphasized
                    ++ [ Element.Font.color (Style.Helpers.toElementColor palette.primary) ]
                )
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
            , let
                accentColor =
                    Style.Helpers.toElementColor palette.neutral.text.default

                accented : Element.Element msg -> Element.Element msg
                accented inner =
                    Element.el [ Element.Font.color accentColor ] inner
              in
              Element.paragraph []
                [ Element.text "created "
                , accented (relativeTimeElement now server.creationTime)
                , Element.text " by "
                , accented (Element.text server.creator)
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
        , creationTime : Time.Posix
        , ready : Bool
        , size : String
        , ip : String
        }


filters :
    Time.Posix
    ->
        List
            (Style.Widgets.DataList.Filter
                { record
                    | creator : String
                    , creationTime : Time.Posix
                }
            )
filters now =
    let
        currentUser =
            "ex3"

        creatorFilterOptionValues servers =
            List.map .creator servers
                |> Set.fromList
                |> Set.toList

        daysBeforeNow : Int -> Int
        daysBeforeNow count =
            Time.Extra.add Day -count Time.utc now |> Time.posixToMillis

        creationTimeFilterOptions =
            [ ( daysBeforeNow 1 |> String.fromInt, "past day" )
            , ( daysBeforeNow 7 |> String.fromInt, "past 7 days" )
            , ( daysBeforeNow 30 |> String.fromInt, "past 30 days" )
            ]

        sameCreator : String -> { a | creator : String } -> Bool
        sameCreator name { creator } =
            name == creator

        createdAfter : String -> { a | creationTime : Time.Posix } -> Bool
        createdAfter startAsMsString { creationTime } =
            let
                createdAt =
                    Time.posixToMillis creationTime

                startAt =
                    String.toInt startAsMsString
            in
            Maybe.map (\start -> start >= createdAt) startAt
                |> Maybe.withDefault True
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
      , onFilter = sameCreator
      }
    , { id = "creationTime"
      , label = "Created within"
      , chipPrefix = "Created within "
      , filterOptions =
            \_ -> Dict.fromList creationTimeFilterOptions
      , filterTypeAndDefaultValue = Style.Widgets.DataList.UniselectOption Style.Widgets.DataList.UniselectNoChoice
      , onFilter = createdAfter
      }
    ]


initServers : Time.Posix -> List Server
initServers now =
    let
        beforeNow : Interval -> Int -> Time.Posix
        beforeNow interval count =
            Time.Extra.add interval -count Time.utc now
    in
    [ { name = "kindly_mighty_katydid"
      , creator = "ex3"
      , creationTime = beforeNow Hour 12
      , ready = True
      , size = "m1.tiny"
      , ip = "129.114.104.147"
      , id = "rtbdf"
      , selectable = True
      }
    , { name = "cheaply_next_crab"
      , creator = "tg3456"
      , creationTime = beforeNow Day 5
      , ready = False
      , size = "m1.medium"
      , ip = "129.114.104.148"
      , id = "tyh43d"
      , selectable = False
      }
    , { name = "basically_well_cobra"
      , creator = "ex3"
      , creationTime = beforeNow Day 30
      , ready = True
      , size = "g1.v100x"
      , ip = "129.114.104.149"
      , id = "vcb543f"
      , selectable = True
      }
    , { name = "adorably_grumpy_cat"
      , creator = "tg3456"
      , creationTime = beforeNow Month 2
      , ready = True
      , size = "g1.v100x"
      , ip = "129.114.104.139"
      , id = "werfdse"
      , selectable = True
      }
    ]
