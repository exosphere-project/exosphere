module Style.StyleGuide exposing (main)

import Browser
import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import FeatherIcons
import Set exposing (Set)
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Button as Button
import Style.Widgets.Card exposing (badge, clickableCardFixedSize, exoCard)
import Style.Widgets.ChipsFilter exposing (chipsFilter)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.DataList as DataList
import Style.Widgets.Icon exposing (bell, history, ipAddress, remove, roundRect, timesCircle)
import Style.Widgets.IconButton exposing (chip)
import Style.Widgets.Meter exposing (meter)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.StatusBadge exposing (StatusBadgeState(..), statusBadge)
import Widget


type Msg
    = ChipsFilterMsg Style.Widgets.ChipsFilter.ChipsFilterMsg
    | DataListMsg DataList.Msg
    | DeleteServer String
    | DeleteSelectedServers (Set String)
    | TogglePopover PopoverId
    | NoOp


type alias Server =
    DataList.DataRecord
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


serverView : Style.Types.ExoPalette -> Server -> Element.Element Msg
serverView palette server =
    let
        statusColor =
            if server.ready then
                Element.rgb255 125 194 5

            else
                Element.rgb255 187 187 187

        interactionButton =
            Widget.iconButton
                (SH.materialStyle palette).button
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
                , onPress = Just NoOp
                }

        deleteServerButton =
            Widget.iconButton
                (SH.materialStyle palette).dangerButton
                { icon = remove (Element.rgb255 255 255 255) 16
                , text = "Delete"
                , onPress =
                    if server.selectable then
                        Just <| DeleteServer server.id

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
                [ Font.size 18
                , Font.color (SH.toElementColor palette.primary)
                ]
                (Element.text server.name)
            , Element.el
                [ Element.width (Element.px 12)
                , Element.height (Element.px 12)
                , Border.rounded 6
                , Background.color statusColor
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
            , Font.color <| SH.toElementColor palette.muted.textOnNeutralBG
            ]
            [ Element.el [] (Element.text server.size)
            , Element.text "·"
            , Element.paragraph []
                [ Element.text "created "
                , Element.el [ Font.color (SH.toElementColor palette.neutral.text.default) ]
                    (Element.text server.creationTime.relativeTime)
                , Element.text " by "
                , Element.el [ Font.color (SH.toElementColor palette.neutral.text.default) ]
                    (Element.text server.creator)
                ]
            , Style.Widgets.Icon.ipAddress
                (SH.toElementColor palette.muted.textOnNeutralBG)
                16
            , Element.el [] (Element.text server.ip)
            ]
        ]


filters :
    List
        (DataList.Filter
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
      , filterTypeAndDefaultValue = DataList.MultiselectOption (Set.fromList [ currentUser ])
      , onFilter =
            \optionValue server ->
                server.creator == optionValue
      }
    , { id = "creationTime"
      , label = "Created within"
      , chipPrefix = "Created within "
      , filterOptions =
            \_ -> Dict.fromList creationTimeFilterOptions
      , filterTypeAndDefaultValue = DataList.UniselectOption DataList.UniselectNoChoice
      , onFilter =
            \optionValue server ->
                let
                    optionInTimestamp =
                        Maybe.withDefault 0 (String.toInt optionValue)
                in
                server.creationTime.timestamp >= optionInTimestamp
      }
    ]


searchFilter : DataList.SearchFilter { record | name : String }
searchFilter =
    { label = "Search by name:"
    , placeholder = Just "try 'my-server'"
    , textToSearch = .name
    }



--noinspection ElmUnresolvedReference
{- When you create a new widget, add example usages to the `widgets` list here! -}


widgets : Style.Types.ExoPalette -> Model -> List (Element.Element Msg)
widgets palette model =
    [ Element.text "Style.Widgets.MenuItem.menuItem"
    , Element.text "Style.Widgets.Icon.roundRect"
    , roundRect (palette.neutral.icon |> SH.toElementColor) 40
    , Element.text "Style.Widgets.Icon.bell"
    , bell (palette.neutral.icon |> SH.toElementColor) 40
    , Element.text "Style.Widgets.Icon.remove"
    , remove (palette.neutral.icon |> SH.toElementColor) 40
    , timesCircle (palette.neutral.icon |> SH.toElementColor) 40
    , Element.text "Style.Widgets.Icon.ipAddress"
    , ipAddress (palette.neutral.icon |> SH.toElementColor) 40
    , Element.text "Style.Widgets.Icon.history"
    , history (palette.neutral.icon |> SH.toElementColor) 40
    , Element.text "Style.Widgets.Card.exoCard"
    , exoCard palette (Element.text "Lorem ipsum dolor sit amet.")
    , Element.text "Style.Widgets.Card.exoCardFixedSize"
    , clickableCardFixedSize palette 300 300 [ Element.text "Lorem ipsum dolor sit amet." ]
    , Element.text "Style.Widgets.Card.badge"
    , badge "belongs to this project"
    , Element.text "Style.Widgets.button (danger)"
    , Button.button
        Button.Danger
        palette
        { text = "Danger button", onPress = Just NoOp }
    , Element.text "Style.Widgets.button (warning)"
    , Button.button
        Button.Warning
        palette
        { text = "Warning button", onPress = Just NoOp }
    , Element.text "Style.Widgets.CopyableText.CopyableText"
    , copyableText palette [] "foobar"
    , Element.text "Style.Widgets.IconButton.chip"
    , chip palette Nothing (Element.text "chip label")
    , Element.text "Style.Widgets.IconButton.chip (with badge)"
    , chip palette Nothing (Element.row [ Element.spacing 5 ] [ Element.text "ubuntu", badge "10" ])
    , Element.text "chipsFilter"
    , chipsFilter
        (SH.materialStyle palette)
        model.chipFilterModel
        |> Element.map ChipsFilterMsg
    , Element.text "Style.Widgets.StatusBadge.statusBadge"
    , statusBadge palette ReadyGood (Element.text "Ready")
    , Element.text "Style.Widgets.Meter"
    , meter palette "Space used" "6 of 10 GB" 6 10
    , Element.text "Style.Widgets.DataList.dataList"
    , DataList.view
        model.dataListModel
        DataListMsg
        { palette = palette, showPopovers = model.showPopovers }
        [ Element.width (Element.maximum 900 Element.fill)
        , Font.size 16
        ]
        (serverView palette)
        model.servers
        [ \serverIds ->
            Element.el [ Element.alignRight ]
                (Widget.iconButton
                    (SH.materialStyle palette).dangerButton
                    { icon = remove (Element.rgb255 255 255 255) 16
                    , text = "Delete"
                    , onPress =
                        Just <| DeleteSelectedServers serverIds
                    }
                )
        ]
        (Just { filters = filters, dropdownMsgMapper = TogglePopover })
        (Just searchFilter)
    ]


intro : List (Element.Element a)
intro =
    [ Element.el
        [ Region.heading 2, Font.size 22, Font.semiBold ]
        (Element.text "Exosphere Style Guide")
    , Element.paragraph
        []
        [ Element.text "This page demonstrates usage of Exosphere's UI widgets. "
        ]
    ]



-- Playing with elm-ui-widgets below


options : List String
options =
    [ "Apple"
    , "Kiwi"
    , "Strawberry"
    , "Pineapple"
    , "Mango"
    , "Grapes"
    , "Watermelon"
    , "Orange"
    , "Lemon"
    , "Blueberry"
    , "Grapefruit"
    , "Coconut"
    , "Cherry"
    , "Banana"
    ]


type alias ChipFilterModel =
    { selected : Set String
    , textInput : String
    , options : List String
    }


type alias Model =
    { chipFilterModel : ChipFilterModel
    , dataListModel : DataList.Model
    , servers : List Server
    , showPopovers : Set.Set PopoverId
    }


init : ( Model, Cmd Msg )
init =
    ( { chipFilterModel =
            { selected = Set.empty
            , textInput = ""
            , options = options
            }
      , dataListModel = DataList.init (DataList.getDefaultFilterOptions filters)
      , servers = initServers
      , showPopovers = Set.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChipsFilterMsg (Style.Widgets.ChipsFilter.ToggleSelection string) ->
            let
                cfm =
                    model.chipFilterModel
            in
            ( { model
                | chipFilterModel =
                    { cfm
                        | selected =
                            model.chipFilterModel.selected
                                |> (if model.chipFilterModel.selected |> Set.member string then
                                        Set.remove string

                                    else
                                        Set.insert string
                                   )
                    }
              }
            , Cmd.none
            )

        ChipsFilterMsg (Style.Widgets.ChipsFilter.SetTextInput string) ->
            let
                cfm =
                    model.chipFilterModel
            in
            ( { model
                | chipFilterModel =
                    { cfm | textInput = string }
              }
            , Cmd.none
            )

        DataListMsg dataListMsg ->
            ( { model | dataListModel = DataList.update dataListMsg model.dataListModel }, Cmd.none )

        DeleteServer serverId ->
            ( { model
                | servers =
                    List.filter
                        (\server -> not (server.id == serverId))
                        model.servers
              }
            , Cmd.none
            )

        DeleteSelectedServers serverIds ->
            ( { model
                | servers =
                    List.filter
                        (\server -> not (Set.member server.id serverIds))
                        model.servers
              }
            , Cmd.none
            )

        TogglePopover popoverId ->
            ( { model
                | showPopovers =
                    if Set.member popoverId model.showPopovers then
                        Set.remove popoverId model.showPopovers

                    else
                        Set.insert popoverId model.showPopovers
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Element.Element Msg
view model =
    let
        palette =
            SH.toExoPalette
                Style.Types.defaultColors
                { theme = Style.Types.Override Style.Types.Light, systemPreference = Nothing }
    in
    intro
        ++ widgets palette model
        |> Element.column
            [ Element.padding 10
            , Element.spacing 20
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = \model -> Element.layout [] (view model)
        , update = update
        , subscriptions = subscriptions
        }
