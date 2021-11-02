module Page.ProjectOverview exposing (Model, Msg, init, update, view)

import Element
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Helpers.String
import Route
import Style.Helpers as SH
import Style.Widgets.Card
import Style.Widgets.Icon as Icon
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    {}


type Msg
    = NoOp


init : Model
init =
    Model


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context p _ =
    let
        renderTile : Element.Element Msg -> String -> Route.ProjectRouteConstructor -> Element.Element Msg -> Element.Element Msg
        renderTile icon str projRouteConstructor contents =
            Element.link []
                { url = Route.toUrl context.urlPathPrefix (Route.ProjectRoute p.auth.project.uuid projRouteConstructor)
                , label =
                    tile context
                        [ Element.column [ Element.padding 10 ]
                            [ Element.row
                                (VH.heading3 context.palette
                                    ++ [ Element.spacing 12
                                       , Border.width 0
                                       , Element.mouseOver
                                            [ Font.color
                                                (context.palette.primary
                                                    |> SH.toElementColor
                                                )
                                            ]
                                       , Element.pointer
                                       ]
                                )
                                [ icon
                                , Element.text str
                                ]
                            , contents
                            ]
                        ]
                }

        renderDescription : String -> Element.Element Msg
        renderDescription description =
            Element.el VH.contentContainer <|
                Element.paragraph [ Font.italic ] [ Element.text description ]
    in
    Element.column
        [ Element.spacing 15, Element.width Element.fill ]
        [ VH.renderMaybe p.description renderDescription
        , Element.wrappedRow [ Element.spacing 25 ]
            [ renderTile
                (FeatherIcons.server
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.virtualComputer
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.ServerList
                (Element.text "some contents")
            , renderTile
                (FeatherIcons.hardDrive
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.blockDevice
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.VolumeList
                (Element.text "some contents")
            , renderTile
                (Icon.ipAddress (SH.toElementColor context.palette.on.background) 24)
                (context.localization.floatingIpAddress
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.FloatingIpList
                (Element.text "some contents")
            , renderTile
                (FeatherIcons.key
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.pkiPublicKeyForSsh
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.KeypairList
                (Element.text "some contents")
            ]
        ]


tile : View.Types.Context -> List (Element.Element Msg) -> Element.Element Msg
tile context contents =
    Style.Widgets.Card.exoCardFixedSize context.palette 400 400 contents
