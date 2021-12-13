module Style.StyleGuide exposing (main)

import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Set exposing (Set)
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Card exposing (badge, clickableCardFixedSize, exoCard, exoCardWithTitleAndSubtitle, expandoCard)
import Style.Widgets.ChipsFilter exposing (chipsFilter)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.DataList as DataList
import Style.Widgets.Icon exposing (bell, history, ipAddress, remove, roundRect, timesCircle)
import Style.Widgets.IconButton exposing (chip)
import Style.Widgets.MenuItem exposing (MenuItemState(..), menuItem)
import Style.Widgets.Meter exposing (meter)
import Style.Widgets.StatusBadge exposing (StatusBadgeState(..), statusBadge)
import Widget



{- When you create a new widget, add example usages to the `widgets` list here! -}


type Msg
    = ChipsFilterMsg Style.Widgets.ChipsFilter.ChipsFilterMsg
    | ToggleExpandoCard Bool
    | DataListMsg DataList.Msg
    | NoOp


type alias Server =
    { name : String
    , creator : String
    , creationTime : String
    , ready : Bool
    , size : String
    , ip : String
    }


servers : List Server
servers =
    [ { name = "kindly_mighty_katydid"
      , creator = "ex3"
      , creationTime = "5 days ago"
      , ready = True
      , size = "m1.tiny"
      , ip = "129.114.104.147"
      }
    , { name = "cheaply_next_crab"
      , creator = "tg3456"
      , creationTime = "15 days ago"
      , ready = False
      , size = "m1.medium"
      , ip = "129.114.104.148"
      }
    , { name = "basically_well_cobra"
      , creator = "ex3"
      , creationTime = "1 month ago"
      , ready = True
      , size = "g1.v100x"
      , ip = "129.114.104.149"
      }
    ]


serverView : Server -> Element.Element msg
serverView server =
    let
        statusColor =
            if server.ready then
                Element.rgb255 125 194 5

            else
                Element.rgb255 187 187 187
    in
    Element.column
        [ Element.spacing 12
        , Element.width Element.fill
        , Font.size 16
        ]
        [ Element.row [ Element.spacing 10, Element.width Element.fill ]
            [ Element.el
                [ Font.size 18
                , Font.color (Element.rgb255 32 109 163)
                ]
                (Element.text server.name)
            , Element.el
                [ Element.width (Element.px 12)
                , Element.height (Element.px 12)
                , Border.rounded 6
                , Background.color statusColor
                ]
                Element.none
            , Element.link
                [ Element.alignRight
                , Font.color (Element.rgb255 32 109 163)
                ]
                { url = "#", label = Element.text "Action1 | Action2" }
            ]
        , Element.row
            [ Element.spacing 8
            , Element.width Element.fill
            , Font.color (Element.rgb255 96 96 96)
            ]
            [ Element.el [] (Element.text server.size)
            , Element.text "·"
            , Element.paragraph []
                [ Element.text "created "
                , Element.el [ Font.color (Element.rgb255 0 0 0) ] (Element.text server.creationTime)
                , Element.text " by "
                , Element.el [ Font.color (Element.rgb255 0 0 0) ] (Element.text server.creator)
                ]
            , Style.Widgets.Icon.ipAddress (Element.rgb255 96 96 96) 16
            , Element.el [] (Element.text server.ip)
            ]
        ]



--noinspection ElmUnresolvedReference


widgets : Style.Types.ExoPalette -> Model -> List (Element.Element Msg)
widgets palette model =
    [ Element.text "Style.Widgets.MenuItem.menuItem"
    , menuItem palette Active Nothing "Active menu item" "https://try.exosphere.app"
    , menuItem palette Inactive Nothing "Inactive menu item" "https://try.exosphere.app"
    , Element.text "Style.Widgets.Icon.roundRect"
    , roundRect (palette.on.background |> SH.toElementColor) 40
    , Element.text "Style.Widgets.Icon.bell"
    , bell (palette.on.background |> SH.toElementColor) 40
    , Element.text "Style.Widgets.Icon.remove"
    , remove (palette.on.background |> SH.toElementColor) 40
    , timesCircle (palette.on.background |> SH.toElementColor) 40
    , Element.text "Style.Widgets.Icon.ipAddress"
    , ipAddress (palette.on.background |> SH.toElementColor) 40
    , Element.text "Style.Widgets.Icon.history"
    , history (palette.on.background |> SH.toElementColor) 40
    , Element.text "Style.Widgets.Card.exoCard"
    , exoCard palette (Element.text "Lorem ipsum dolor sit amet.")
    , Element.text "Style.Widgets.Card.exoCardFixedSize"
    , clickableCardFixedSize palette 300 300 [ Element.text "Lorem ipsum dolor sit amet." ]
    , Element.text "Style.Widgets.Card.exoCardWithTitleAndSubtitle"
    , exoCardWithTitleAndSubtitle palette (Element.text "Title") (Element.text "Subtitle") (Element.text "Lorem ipsum dolor sit amet.")
    , Element.text "Style.Widgets.Card.expandoCard"
    , expandoCard palette
        model.expandoCardExpanded
        (\new -> ToggleExpandoCard new)
        (Element.text "Title")
        (Element.text "Subtitle")
        (Element.text "contents")
    , Element.text "Style.Widgets.Card.badge"
    , badge "belongs to this project"
    , Element.text "Widgets.textButton (dangerButton)"
    , Widget.textButton
        (SH.materialStyle palette).dangerButton
        { text = "Danger button", onPress = Just NoOp }
    , Element.text "Widgets.textButton (warningButton)"
    , Widget.textButton
        (SH.materialStyle palette).warningButton
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
        [ Element.width (Element.maximum 900 Element.fill) ]
        serverView
        servers
        DataList.init
        |> Element.map (\msg -> msgMapper (DataListMsg msg))
    ]


intro : List (Element.Element a)
intro =
    [ Element.el
        [ Region.heading 2, Font.size 22, Font.bold ]
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
    , expandoCardExpanded : Bool
    , dataListModel : DataList.DataListModel
    }


init : ( Model, Cmd Msg )
init =
    ( { chipFilterModel =
            { selected = Set.empty
            , textInput = ""
            , options = options
            }
      , expandoCardExpanded = False
      , dataListModel = DataList.init
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

        ToggleExpandoCard new ->
            ( { model
                | expandoCardExpanded = new
              }
            , Cmd.none
            )

        DataListMsg dataListMsg ->
            ( { model | dataListModel = DataList.update dataListMsg model.dataListModel }, Cmd.none )

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
