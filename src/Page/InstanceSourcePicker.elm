module Page.InstanceSourcePicker exposing (Model, Msg, init, update, view)

import Element
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Page.ImageList
import Page.OperatingSystemList
import Style.Helpers as SH
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
    | OperatingSystemListMsg Page.OperatingSystemList.Msg
    | NoOp


init : Model
init =
    Model (Just 0) Page.ImageList.init


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

        OperatingSystemListMsg _ ->
            -- This page doesn't currently have a model or update function. Dummy NoOp Msg is only used so that
            -- buttons don't look disabled.
            ( model, Cmd.none, SharedMsg.NoOp )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    if List.isEmpty project.images then
        Element.row [ Element.spacing 15 ]
            [ Widget.circularProgressIndicator (SH.materialStyle context.palette).progressIndicator Nothing
            , Element.text <|
                String.join " "
                    [ context.localization.staticRepresentationOfBlockDeviceContents
                        |> Helpers.String.toTitleCase
                        |> Helpers.String.pluralize
                    , "loading..."
                    ]
            ]

    else
        let
            maybeOperatingSystemChoices =
                GetterSetters.cloudSpecificConfigLookup context.cloudSpecificConfigs project
                    |> Maybe.map .operatingSystemChoices

            viewImageList =
                Page.ImageList.view context project model.imageListModel
                    |> Element.map ImageListMsg

            imageListOnlyView : Element.Element Msg
            imageListOnlyView =
                Element.column
                    (VH.exoColumnAttributes
                        ++ [ Element.width Element.fill ]
                    )
                    [ Element.el (VH.heading2 context.palette)
                        (Element.text <|
                            String.join " "
                                [ "Choose"
                                , Helpers.String.indefiniteArticle context.localization.staticRepresentationOfBlockDeviceContents
                                , context.localization.staticRepresentationOfBlockDeviceContents
                                ]
                        )
                    , viewImageList
                    ]

            tabbedView : List Types.HelperTypes.OperatingSystemChoice -> Element.Element Msg
            tabbedView opSysChoices =
                Element.column
                    (VH.exoColumnAttributes
                        ++ [ Element.width Element.fill ]
                    )
                    [ Element.el (VH.heading2 context.palette) <|
                        Element.text <|
                            Helpers.String.toTitleCase <|
                                String.join " "
                                    [ "Choose"
                                    , context.localization.virtualComputer
                                        |> Helpers.String.indefiniteArticle
                                    , context.localization.virtualComputer
                                    , "Source"
                                    ]
                    , Widget.tab (SH.materialStyle context.palette).tab
                        { tabs =
                            Widget.Select
                                model.tab
                                [ { text = "By Operating System", icon = Element.none }
                                , { text = "By Image", icon = Element.none }
                                ]
                                (\i -> Just <| SetTab i)
                        , content =
                            \maybeTabInt ->
                                case maybeTabInt of
                                    Just 0 ->
                                        Page.OperatingSystemList.view context project opSysChoices
                                            |> Element.map OperatingSystemListMsg

                                    Just 1 ->
                                        viewImageList

                                    _ ->
                                        Element.none
                        }
                    ]
        in
        -- At least one operating system choice + version must be defined to show the operating system choices tab.
        -- Otherwise we just show a list of images.
        case maybeOperatingSystemChoices of
            Just choices ->
                if List.isEmpty choices then
                    imageListOnlyView

                else if List.map .versions choices |> List.concat |> List.isEmpty then
                    imageListOnlyView

                else
                    tabbedView choices

            Nothing ->
                imageListOnlyView
