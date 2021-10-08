module Style.StyleGuide exposing (main)

import Browser
import Element
import Element.Font as Font
import Element.Region as Region
import Set exposing (Set)
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Card exposing (badge, exoCard, exoCardFixedSize, exoCardWithTitleAndSubtitle, expandoCard)
import Style.Widgets.ChipsFilter exposing (chipsFilter)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon exposing (bell, ipAddress, remove, roundRect, timesCircle)
import Style.Widgets.IconButton exposing (chip)
import Style.Widgets.MenuItem exposing (MenuItemState(..), menuItem)
import Style.Widgets.StatusBadge exposing (StatusBadgeState(..), statusBadge)
import Widget



{- When you create a new widget, add example usages to the `widgets` list here! -}


type Msg
    = ChipsFilterMsg Style.Widgets.ChipsFilter.ChipsFilterMsg
    | ToggleExpandoCard Bool
    | NoOp



--noinspection ElmUnresolvedReference


widgets : (Msg -> msg) -> Style.Types.ExoPalette -> Model -> List (Element.Element msg)
widgets msgMapper palette model =
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
    , Element.text "Style.Widgets.Card.exoCard"
    , exoCard palette (Element.text "Lorem ipsum dolor sit amet.")
    , Element.text "Style.Widgets.Card.exoCardFixedSize"
    , exoCardFixedSize palette 300 300 [ Element.text "Lorem ipsum dolor sit amet." ]
    , Element.text "Style.Widgets.Card.exoCardWithTitleAndSubtitle"
    , exoCardWithTitleAndSubtitle palette (Element.text "Title") (Element.text "Subtitle") (Element.text "Lorem ipsum dolor sit amet.")
    , Element.text "Style.Widgets.Card.expandoCard"
    , expandoCard palette
        model.expandoCardExpanded
        (\new -> msgMapper (ToggleExpandoCard new))
        (Element.text "Title")
        (Element.text "Subtitle")
        (Element.text "contents")
    , Element.text "Style.Widgets.Card.badge"
    , badge "belongs to this project"
    , Element.text "Widgets.textButton (dangerButton)"
    , Widget.textButton
        (SH.materialStyle palette).dangerButton
        { text = "Danger button", onPress = Just (msgMapper NoOp) }
    , Element.text "Widgets.textButton (warningButton)"
    , Widget.textButton
        (SH.materialStyle palette).warningButton
        { text = "Warning button", onPress = Just (msgMapper NoOp) }
    , Element.text "Style.Widgets.CopyableText.CopyableText"
    , copyableText palette [] "foobar"
    , Element.text "Style.Widgets.IconButton.chip"
    , chip palette Nothing (Element.text "chip label")
    , Element.text "Style.Widgets.IconButton.chip (with badge)"
    , chip palette Nothing (Element.row [ Element.spacing 5 ] [ Element.text "ubuntu", badge "10" ])
    , Element.text "chipsFilter"
    , chipsFilter
        (ChipsFilterMsg >> msgMapper)
        (SH.materialStyle palette)
        model.chipFilterModel
    , Element.text "Style.Widgets.StatusBadge.statusBadge"
    , statusBadge palette ReadyGood (Element.text "Ready")
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
    }


init : ( Model, Cmd Msg )
init =
    ( { chipFilterModel =
            { selected = Set.empty
            , textInput = ""
            , options = options
            }
      , expandoCardExpanded = False
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

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : (Msg -> msg) -> Model -> Element.Element msg
view msgMapper model =
    let
        palette =
            SH.toExoPalette
                Style.Types.defaultPrimaryColor
                Style.Types.defaultSecondaryColor
                { theme = Style.Types.Override Style.Types.Light, systemPreference = Nothing }
    in
    intro
        ++ widgets msgMapper palette model
        |> Element.column
            [ Element.padding 10
            , Element.spacing 20
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view identity >> Element.layout []
        , update = update
        , subscriptions = subscriptions
        }
