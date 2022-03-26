module Page.InstanceSourcePicker exposing (Model, Msg, init, update, view)

import Element
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Page.ImageList
import Page.InstanceTypeList
import Style.Helpers as SH
import Style.Widgets.Text as Text
import Types.HelperTypes
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { tab : Maybe Int
    , imageListModel : Page.ImageList.Model
    }


type Msg
    = SetTab Int
    | ImageListMsg Page.ImageList.Msg
    | InstanceTypeListMsg Page.InstanceTypeList.Msg
    | NoOp


init : Model
init =
    Model (Just 0) (Page.ImageList.init False False)


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        SetTab tab ->
            ( { model | tab = Just tab }, Cmd.none, SharedMsg.NoOp )

        ImageListMsg imageListMsg ->
            let
                ( imageListModel, imageListCmd, sharedMsg ) =
                    Page.ImageList.update imageListMsg project model.imageListModel
            in
            ( { model | imageListModel = imageListModel }
            , Cmd.map ImageListMsg imageListCmd
            , sharedMsg
            )

        InstanceTypeListMsg _ ->
            -- This page doesn't currently have a model or update function. Dummy NoOp Msg is only used so that
            -- buttons don't look disabled.
            ( model, Cmd.none, SharedMsg.NoOp )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        maybeInstanceTypes =
            GetterSetters.cloudSpecificConfigLookup context.cloudSpecificConfigs project
                |> Maybe.map .instanceTypes

        viewImageList =
            Page.ImageList.view context project model.imageListModel
                |> Element.map ImageListMsg

        imageListOnlyView : Element.Element Msg
        imageListOnlyView =
            Element.column
                (VH.exoColumnAttributes
                    ++ [ Element.width Element.fill ]
                )
                [ Text.heading context.palette
                    []
                    Element.none
                    (String.join " "
                        [ "Choose"
                        , Helpers.String.indefiniteArticle context.localization.staticRepresentationOfBlockDeviceContents
                        , context.localization.staticRepresentationOfBlockDeviceContents
                        ]
                    )
                , viewImageList
                ]

        tabbedView : List Types.HelperTypes.InstanceType -> Element.Element Msg
        tabbedView opSysChoices =
            Element.column
                (VH.exoColumnAttributes
                    ++ [ Element.width Element.fill ]
                )
                [ Text.heading context.palette
                    []
                    Element.none
                    (String.join " "
                        [ "Choose"
                        , context.localization.virtualComputer
                            |> Helpers.String.indefiniteArticle
                        , context.localization.virtualComputer
                            |> Helpers.String.toTitleCase
                        , "Source"
                        ]
                    )
                , Widget.tab (SH.materialStyle context.palette).tab
                    { tabs =
                        Widget.Select
                            model.tab
                            [ { text = "By Type", icon = Element.none }
                            , { text = "By Image", icon = Element.none }
                            ]
                            (\i -> Just <| SetTab i)
                    , content =
                        \maybeTabInt ->
                            case maybeTabInt of
                                Just 0 ->
                                    Page.InstanceTypeList.view context project opSysChoices
                                        |> Element.map InstanceTypeListMsg

                                Just 1 ->
                                    viewImageList

                                _ ->
                                    Element.none
                    }
                ]

        loadedView _ =
            -- At least one instance type + version must be defined to show the instance types tab.
            -- Otherwise we just show a list of images.
            case maybeInstanceTypes of
                Just choices ->
                    if List.isEmpty choices then
                        imageListOnlyView

                    else if List.concatMap .versions choices |> List.isEmpty then
                        imageListOnlyView

                    else
                        tabbedView choices

                Nothing ->
                    imageListOnlyView
    in
    VH.renderRDPP context project.images (Helpers.String.pluralize context.localization.staticRepresentationOfBlockDeviceContents) loadedView
