module Page.ShareDetail exposing (Model, Msg(..), init, update, view)

import DateFormat.Relative
import Element
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (Unit(..), humanNumber)
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Helpers.Time
import OpenStack.Types as OSTypes exposing (Share)
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Card
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Time
import Types.HelperTypes exposing (FloatingIpOption(..))
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { shareUuid : OSTypes.ShareUuid
    }


type Msg
    = SharedMsg SharedMsg.SharedMsg
    | NoOp


init : OSTypes.ShareUuid -> Model
init shareUuid =
    { shareUuid = shareUuid
    }


update : Msg -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg model =
    case msg of
        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


popoverMsgMapper : PopoverId -> Msg
popoverMsgMapper popoverId =
    SharedMsg <| SharedMsg.TogglePopover popoverId


view : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> Element.Element Msg
view context project currentTimeAndZone model =
    VH.renderRDPP context
        project.shares
        context.localization.share
        (\_ ->
            {- Attempt to look up a given share uuid; if a share is found, call render. -}
            case GetterSetters.shareLookup project model.shareUuid of
                Just share ->
                    render context project currentTimeAndZone share

                Nothing ->
                    Element.text <|
                        String.join " "
                            [ "No"
                            , context.localization.share
                            , "found"
                            ]
        )


createdAgoByWhomEtc :
    View.Types.Context
    ->
        { ago : ( String, Element.Element msg )
        , creator : String
        , size : String
        , shareProtocol : String
        , shareTypeName : String
        , visibility : String
        }
    -> Element.Element msg
createdAgoByWhomEtc context { ago, creator, size, shareProtocol, shareTypeName, visibility } =
    let
        ( agoWord, agoContents ) =
            ago

        subduedText =
            Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)
    in
    Element.wrappedRow
        [ Element.width Element.fill, Element.spaceEvenly ]
    <|
        [ Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| agoWord ++ " ")
            , agoContents
            , Element.el [ subduedText ] (Element.text <| " by ")
            , Element.text creator
            ]
        , Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| "size ")
            , Element.text size
            ]
        , Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| "visibility ")
            , Element.text visibility
            ]
        , Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| "protocol ")
            , Element.text shareProtocol
            ]
        , Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| "type ")
            , Element.text shareTypeName
            ]
        ]


shareNameView : Share -> Element.Element Msg
shareNameView share =
    let
        name_ =
            VH.resourceName share.name share.uuid

        nameViewPlain =
            Element.row
                [ Element.spacing spacer.px8 ]
                [ Text.text Text.ExtraLarge [] name_ ]
    in
    nameViewPlain


shareStatus : View.Types.Context -> Share -> Element.Element Msg
shareStatus context share =
    let
        statusBadge =
            VH.shareStatusBadge context.palette share.status
    in
    Element.row [ Element.spacing spacer.px16 ]
        [ statusBadge
        ]


render : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Share -> Element.Element Msg
render context project ( currentTime, _ ) share =
    let
        whenCreated =
            let
                timeDistanceStr =
                    DateFormat.Relative.relativeTime currentTime share.createdAt

                createdTimeText =
                    let
                        createdTimeFormatted =
                            Helpers.Time.humanReadableDateAndTime share.createdAt
                    in
                    Element.text ("Created on: " ++ createdTimeFormatted)

                toggleTipContents =
                    Element.column [] [ createdTimeText ]
            in
            Element.row
                [ Element.spacing spacer.px4 ]
                [ Element.text timeDistanceStr
                , Style.Widgets.ToggleTip.toggleTip
                    context
                    popoverMsgMapper
                    (Helpers.String.hyphenate
                        [ "createdTimeTip"
                        , project.auth.project.uuid
                        , share.uuid
                        ]
                    )
                    toggleTipContents
                    ST.PositionBottomLeft
                ]

        creator =
            if share.userUuid == project.auth.user.uuid then
                "me"

            else
                "another user"

        sizeString =
            let
                locale =
                    context.locale

                ( sizeDisplay, sizeLabel ) =
                    -- The share size, in GiBs.
                    humanNumber { locale | decimals = Exact 0 } GibiBytes share.size
            in
            sizeDisplay ++ " " ++ sizeLabel

        description =
            case share.description of
                Just str ->
                    Element.row [ Element.padding spacer.px8 ]
                        [ Element.paragraph [ Element.width Element.fill ] <|
                            [ Element.text <| str ]
                        ]

                Nothing ->
                    Element.none

        tile : List (Element.Element Msg) -> List (Element.Element Msg) -> Element.Element Msg
        tile headerContents contents =
            Style.Widgets.Card.exoCard context.palette
                (Element.column
                    [ Element.width Element.fill
                    , Element.padding spacer.px16
                    , Element.spacing spacer.px16
                    ]
                    (List.concat
                        [ [ Element.row
                                (Text.subheadingStyleAttrs context.palette
                                    ++ Text.typographyAttrs Text.Large
                                    ++ [ Border.width 0 ]
                                )
                                headerContents
                          ]
                        , [ description ]
                        , contents
                        ]
                    )
                )
    in
    Element.column [ Element.spacing spacer.px24, Element.width Element.fill ]
        [ Element.row (Text.headingStyleAttrs context.palette)
            [ FeatherIcons.share2 |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Text.text Text.ExtraLarge
                []
                (context.localization.share
                    |> Helpers.String.toTitleCase
                )
            , shareNameView share
            , Element.row [ Element.alignRight, Text.fontSize Text.Body, Font.regular, Element.spacing spacer.px16 ]
                [ shareStatus context share ]
            ]
        , tile
            [ FeatherIcons.database |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Element.text "Info"
            , Element.el
                [ Text.fontSize Text.Tiny
                , Font.color (SH.toElementColor context.palette.neutral.text.subdued)
                , Element.alignBottom
                ]
                (copyableText context.palette
                    [ Element.width (Element.shrink |> Element.minimum 240) ]
                    share.uuid
                )
            ]
            [ createdAgoByWhomEtc
                context
                { ago = ( "created", whenCreated )
                , creator = creator
                , size = sizeString
                , shareProtocol = OSTypes.shareProtocolToString share.shareProtocol
                , shareTypeName = share.shareTypeName
                , visibility = OSTypes.shareVisibilityToString share.visibility
                }
            ]
        ]
