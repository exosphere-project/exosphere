module Style.StyleGuide exposing (main)

import Browser
import Color
import Element
import Element.Font as Font
import Element.Region as Region
import Framework.Modifier exposing (Modifier(..))
import OpenStack.Types as OSTypes
import Set exposing (Set)
import Style.Widgets.Card exposing (badge, exoCard)
import Style.Widgets.ChipsFilter exposing (chipsFilter)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon exposing (bell, question, remove, rightArrow, roundRect, timesCircle)
import Style.Widgets.IconButton exposing (chip, iconButton)
import Style.Widgets.MenuItem exposing (MenuItemState(..), menuItem)
import Widget
import Widget.Style exposing (ButtonStyle, ColumnStyle, RowStyle, SortTableStyle, TextInputStyle)
import Widget.Style.Material as Material



{- When you create a new widget, add example usages to the `widgets` list here! -}


type ChangedSortingMsgLocal
    = ChangedSorting String


type ImagePressedMsgLocal
    = ImagePressed OSTypes.Image


type Msg
    = ChipsFilterMsg Style.Widgets.ChipsFilter.ChipsFilterMsg
    | ChangedSortingMsg ChangedSortingMsgLocal
    | ImagePressedMsg ImagePressedMsgLocal


widgets : (Msg -> msg) -> Style style msg -> Model -> List (Element.Element msg)
widgets msgMapper style model =
    [ Element.text "Style.Widgets.MenuItem.menuItem"
    , menuItem Active "Active menu item" Nothing
    , menuItem Inactive "Inactive menu item" Nothing
    , Element.text "Style.Widgets.Icon.roundRect"
    , roundRect Color.black 40
    , Element.text "Style.Widgets.Icon.bell"
    , bell Color.black 40
    , Element.text "Style.Widgets.Icon.question"
    , question Color.black 40
    , Element.text "Style.Widgets.Icon.remove"
    , remove Color.black 40
    , Element.text "Style.Widgets.Icon.timesCircle (black)"
    , timesCircle Color.black 40
    , Element.text "Style.Widgets.Icon.timesCircle (white)"
    , timesCircle Color.white 40
    , Element.text "Style.Widgets.Card.exoCard"
    , exoCard "Title" "Subtitle" (Element.text "Lorem ipsum dolor sit amet.")
    , Element.text "Style.Widgets.Card.badge"
    , badge "belongs to this project"
    , Element.text "Style.Widgets.IconButton.iconButton"
    , iconButton [ Small, Danger ] Nothing (remove Color.white 16)
    , Element.text "Style.Widgets.CopyableText.CopyableText"
    , copyableText "foobar"
    , Element.text "Style.Widgets.IconButton.chip"
    , chip Nothing (Element.text "chip label")
    , Element.text "Style.Widgets.IconButton.chip (with badge)"
    , chip Nothing (Element.row [ Element.spacing 5 ] [ Element.text "ubuntu", badge "10" ])
    , Element.text "chipsFilter"
    , chipsFilter (ChipsFilterMsg >> msgMapper) style model.chipFilterModel
    , Element.text "viewList"
    , viewImageList (ImagePressedMsg >> msgMapper) style model.imageListModel
    , Element.text "viewSortTable"
    , viewSortTable (ChangedSortingMsg >> msgMapper) style model.sortTableModel
    ]


viewImageList : (ImagePressedMsgLocal -> msg) -> Style style msg -> List OSTypes.Image -> Element.Element msg
viewImageList msgMapper style images =
    List.map (renderImage msgMapper) images
        |> Widget.column style.cardColumn


renderImage : (ImagePressedMsgLocal -> msg) -> OSTypes.Image -> Element.Element msg
renderImage msgMapper image =
    let
        tagChip : String -> Element.Element msg
        tagChip tag =
            Widget.button materialStyle.chipButton
                { text = tag
                , icon = Element.none
                , onPress =
                    Nothing
                }
    in
    Element.column [ Element.width Element.fill ]
        [ [ Element.el [ Element.width Element.fill ] (Element.text image.name)
          , Element.el [ Element.alignRight ]
                (Widget.button materialStyle.primaryButton
                    { text = "Launch"
                    , icon = rightArrow Color.white 16
                    , onPress =
                        if image.status == OSTypes.ImageActive then
                            ImagePressed image
                                |> msgMapper
                                |> Just

                        else
                            Nothing
                    }
                )
          ]
            |> Element.row
                [ Element.width Element.fill
                ]
        , Element.row [ Element.width Element.fill ]
            [ Element.el [ Element.width Element.shrink ] (Element.text ("Size: " ++ (image.size |> Maybe.map String.fromInt |> Maybe.withDefault "N/A")))
            ]
        , Element.row [ Element.width Element.fill ]
            [ [ Element.el [] (Element.text "Tags:") ]
                ++ List.map tagChip image.tags
                |> Element.wrappedRow
                    [ Element.width Element.fill
                    , Element.spacingXY 10 0
                    ]
            ]
        ]


viewSortTable : (ChangedSortingMsgLocal -> msg) -> Style style msg -> SortTableModel -> Element.Element msg
viewSortTable msgMapper style model =
    Widget.sortTable style.sortTable
        { content =
            [ { id = 1, name = "Antonio", rating = 2.456, hash = Nothing }
            , { id = 2, name = "Ana", rating = 1.34, hash = Just "45jf" }
            , { id = 3, name = "Alfred", rating = 4.22, hash = Just "6fs1" }
            , { id = 4, name = "Thomas", rating = 3, hash = Just "k52f" }
            ]
        , columns =
            [ Widget.intColumn
                { title = "Id"
                , value = .id
                , toString = \int -> "#" ++ String.fromInt int
                , width = Element.fill
                }
            , Widget.stringColumn
                { title = "Name"
                , value = .name
                , toString = identity
                , width = Element.fill
                }
            , Widget.floatColumn
                { title = "Rating"
                , value = .rating
                , toString = String.fromFloat
                , width = Element.fill
                }
            , Widget.unsortableColumn
                { title = "Hash"
                , toString = .hash >> Maybe.withDefault "None"
                , width = Element.fill
                }
            ]
        , asc = model.asc
        , sortBy = model.title
        , onChange = ChangedSorting >> msgMapper
        }


intro : List (Element.Element a)
intro =
    [ Element.el
        [ Region.heading 2, Font.size 22, Font.bold ]
        (Element.text "Exosphere Style Guide")
    , Element.paragraph
        []
        [ Element.text "This page demonstrates usage of Exosphere's UI widgets. "
        , Element.text "See also the style guide for elm-style-framework (TODO link to demo style guide)"
        ]
    ]



-- Playing with elm-ui-widgets below


type alias Style style msg =
    { style
        | textInput : TextInputStyle msg
        , column : ColumnStyle msg
        , sortTable : SortTableStyle msg
        , cardColumn : ColumnStyle msg
        , primaryButton : ButtonStyle msg
        , button : ButtonStyle msg
        , chipButton : ButtonStyle msg
        , row : RowStyle msg
    }


materialStyle : Style {} msg
materialStyle =
    { textInput = Material.textInput Material.defaultPalette
    , column = Material.column
    , sortTable =
        { containerTable = []
        , headerButton = Material.textButton Material.defaultPalette
        , ascIcon =
            Material.expansionPanel Material.defaultPalette
                |> .collapseIcon
        , descIcon =
            Material.expansionPanel Material.defaultPalette
                |> .expandIcon
        , defaultIcon = Element.none
        }
    , cardColumn = Material.cardColumn Material.defaultPalette
    , primaryButton = Material.containedButton Material.defaultPalette
    , button = Material.outlinedButton Material.defaultPalette
    , chipButton = Material.chip Material.defaultPalette

    --, row = Material.row
    , row =
        { containerRow =
            [ Element.paddingXY 0 8
            , Element.spacing 8
            , Element.width Element.fill
            , Element.explain Debug.todo
            ]
        , element =
            [ Element.width Element.fill
            , Element.explain Debug.todo
            ]
        , ifFirst = []
        , ifLast = []
        , otherwise = []
        }
    }


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


type alias SortTableModel =
    { title : String
    , asc : Bool
    }


type alias Model =
    { chipFilterModel : ChipFilterModel
    , sortTableModel : SortTableModel
    , imageListModel : List OSTypes.Image
    }


init : ( Model, Cmd Msg )
init =
    ( { chipFilterModel =
            { selected = Set.empty
            , textInput = ""
            , options = options
            }
      , sortTableModel = { title = "Name", asc = True }
      , imageListModel =
            [ { name = "Ubuntu 0.9"
              , status = OSTypes.ImageActive
              , uuid = "C9E9DA8D-FA63-489B-B4CF-9D045D36FB79"
              , size = Just 1000
              , checksum = Just "BCAC4727-3DD5-48FE-8A4E-492052899382"
              , diskFormat = Just "qemu"
              , containerFormat = Just "vbox"
              , tags = [ "distro-base", "ubuntu" ]
              , projectUuid = "38F0560E-846A-4F71-967B-0C85A63B2006"
              }
            , { name = "Windows 3.1"
              , status = OSTypes.ImageKilled
              , uuid = "79DAD7DA-65E4-4C56-88E9-6CACE40BBF61"
              , size = Just 666
              , checksum = Just "68C93527-26FC-47AF-A6D2-C763EBC38C4A"
              , diskFormat = Just "qemu"
              , containerFormat = Just "vbox"
              , tags = [ "distro-base", "windows" ]
              , projectUuid = "38F0560E-846A-4F71-967B-0C85A63B2006"
              }
            , { name = "RedHat 20.1"
              , status = OSTypes.ImageActive
              , uuid = "B27D9BC6-5D8B-4C19-801D-98D81D015053"
              , size = Just 333
              , checksum = Just "07AA5229-F5A6-49AA-BF49-9C50510A19BF"
              , diskFormat = Just "qemu"
              , containerFormat = Just "vbox"
              , tags = [ "distro-base", "redhat" ]
              , projectUuid = "38F0560E-846A-4F71-967B-0C85A63B2006"
              }
            ]
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

        ChangedSortingMsg (ChangedSorting string) ->
            ( { model
                | sortTableModel =
                    { title = string
                    , asc =
                        if model.sortTableModel.title == string then
                            not model.sortTableModel.asc

                        else
                            True
                    }
              }
            , Cmd.none
            )

        ImagePressedMsg imagePressedMsgLocal ->
            let
                _ =
                    Debug.log "imagePressedMsgLocal" imagePressedMsgLocal
            in
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : (Msg -> msg) -> Style style msg -> Model -> Element.Element msg
view msgMapper style model =
    intro
        ++ widgets msgMapper style model
        |> Element.column
            [ Element.padding 10
            , Element.spacing 20
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view identity materialStyle >> Element.layout []
        , update = update
        , subscriptions = subscriptions
        }
