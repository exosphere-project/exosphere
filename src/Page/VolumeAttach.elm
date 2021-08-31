module Page.VolumeAttach exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Types as OSTypes
import RemoteData
import Style.Helpers as SH
import Style.Widgets.Select
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..))
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { maybeServerUuid : Maybe OSTypes.ServerUuid
    , maybeVolumeUuid : Maybe OSTypes.VolumeUuid
    }


type Msg
    = GotServerUuid (Maybe OSTypes.ServerUuid)
    | GotVolumeUuid (Maybe OSTypes.VolumeUuid)
    | GotSubmit OSTypes.ServerUuid OSTypes.VolumeUuid


init : Maybe OSTypes.ServerUuid -> Maybe OSTypes.VolumeUuid -> Model
init maybeServerUuid maybeVolumeUuid =
    Model maybeServerUuid maybeVolumeUuid


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotServerUuid maybeServerUuid ->
            ( { model | maybeServerUuid = maybeServerUuid }, Cmd.none, SharedMsg.NoOp )

        GotVolumeUuid maybeVolumeUuid ->
            ( { model | maybeVolumeUuid = maybeVolumeUuid }, Cmd.none, SharedMsg.NoOp )

        GotSubmit serverUuid volumeUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg project.auth.project.uuid <|
                ServerMsg serverUuid <|
                    RequestAttachVolume volumeUuid
            )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        serverChoices =
            -- Future TODO instead of hiding servers that are ineligible to have a newly attached volume, show them grayed out with mouseover text like "volume cannot be attached to this server because X"
            RDPP.withDefault [] project.servers
                |> List.filter
                    (\s ->
                        not <|
                            List.member
                                s.osProps.details.openstackStatus
                                [ OSTypes.ServerShelved
                                , OSTypes.ServerShelvedOffloaded
                                , OSTypes.ServerError
                                , OSTypes.ServerSoftDeleted
                                , OSTypes.ServerBuilding
                                ]
                    )
                |> List.map
                    (\s ->
                        ( s.osProps.uuid, VH.possiblyUntitledResource s.osProps.name context.localization.virtualComputer )
                    )

        volumeChoices =
            RemoteData.withDefault [] project.volumes
                |> List.filter (\v -> v.status == OSTypes.Available)
                |> List.map
                    (\v ->
                        ( v.uuid
                        , String.concat
                            [ VH.possiblyUntitledResource v.name context.localization.blockDevice
                            , " - "
                            , String.fromInt v.size ++ " GB"
                            ]
                        )
                    )
    in
    Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el (VH.heading2 context.palette) <|
            Element.text <|
                String.join " "
                    [ "Attach a"
                    , context.localization.blockDevice
                        |> Helpers.String.toTitleCase
                    ]
        , Element.column VH.formContainer
            [ Style.Widgets.Select.select []
                { label =
                    String.join " "
                        [ "Select"
                        , Helpers.String.indefiniteArticle context.localization.virtualComputer
                        , context.localization.virtualComputer
                        ]
                , onChange = GotServerUuid
                , options = serverChoices
                , selected = model.maybeServerUuid
                }
            , Style.Widgets.Select.select []
                -- TODO if no volumes in list, suggest user create a volume and provide link to that view
                { label = "Select a " ++ context.localization.blockDevice
                , onChange = GotVolumeUuid
                , options = volumeChoices
                , selected = model.maybeVolumeUuid
                }
            , let
                params =
                    case ( model.maybeServerUuid, model.maybeVolumeUuid ) of
                        ( Just serverUuid, Just volumeUuid ) ->
                            let
                                volAttachedToServer =
                                    GetterSetters.serverLookup project serverUuid
                                        |> Maybe.map (GetterSetters.volumeIsAttachedToServer volumeUuid)
                                        |> Maybe.withDefault False
                            in
                            if volAttachedToServer then
                                { onPress = Nothing
                                , warnText =
                                    Just <|
                                        String.join " "
                                            [ "This"
                                            , context.localization.blockDevice
                                            , "is already attached to this"
                                            , context.localization.virtualComputer
                                            ]
                                }

                            else
                                { onPress = Just <| GotSubmit serverUuid volumeUuid
                                , warnText = Nothing
                                }

                        _ ->
                            {- User hasn't selected a server and volume yet so we keep the button disabled but don't yell at him/her -}
                            { onPress = Nothing
                            , warnText = Nothing
                            }

                button =
                    Element.el [ Element.alignRight ] <|
                        Widget.textButton
                            (SH.materialStyle context.palette).primaryButton
                            { text = "Attach"
                            , onPress = params.onPress
                            }
              in
              Element.row [ Element.width Element.fill ]
                [ case params.warnText of
                    Just warnText ->
                        Element.el [ Font.color <| SH.toElementColor context.palette.error ] <| Element.text warnText

                    Nothing ->
                        Element.none
                , button
                ]
            ]
        ]
