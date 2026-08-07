module Page.Settings exposing (Model, Msg(..), headerView, init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.String
import Helpers.Time
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Style.Widgets.Uuid exposing (copyableUuid)
import Types.ExtensionApproval exposing (ExtensionApproval)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    {}


type Msg
    = SelectTheme ST.ThemeChoice
    | GotEnableExperimentalFeatures Bool
    | GotEnableAppVersionUpdateNotifications Bool
    | SharedMsg SharedMsg.SharedMsg


init : Model
init =
    {}


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        SelectTheme mode ->
            ( model, Cmd.none, SharedMsg.SelectTheme mode )

        GotEnableExperimentalFeatures choice ->
            ( model, Cmd.none, SharedMsg.SetExperimentalFeaturesEnabled choice )

        GotEnableAppVersionUpdateNotifications choice ->
            ( model, Cmd.none, SharedMsg.SetAppVersionUpdateNotificationsEnabled choice )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


headerView : View.Types.Context -> Element.Element msg
headerView context =
    Text.heading context.palette
        VH.headerHeadingAttributes
        Element.none
        "Settings"


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel _ =
    let
        experimentalFeatureToggleTip =
            Element.el [ Element.paddingXY spacer.px8 0 ] <|
                Style.Widgets.ToggleTip.toggleTip
                    context
                    (\experimentalFeaturesTipId -> SharedMsg <| SharedMsg.TogglePopover experimentalFeaturesTipId)
                    "settingsExperimentalFeaturesToggleTip"
                    (Element.paragraph
                        [ Element.width (Element.fill |> Element.minimum 300)
                        , Element.spacing spacer.px8
                        , Font.regular
                        ]
                        [ Element.text "New features in development. An example is adding a custom workflow when you launch a server." ]
                    )
                    ST.PositionRight

        appVersionUpdateNotificationsToggleTip =
            Element.el [ Element.paddingXY spacer.px8 0 ] <|
                Style.Widgets.ToggleTip.toggleTip
                    context
                    (\appVersionUpdateNotificationsTipId -> SharedMsg <| SharedMsg.TogglePopover appVersionUpdateNotificationsTipId)
                    "settingsAppVersionUpdateNotificationsToggleTip"
                    (Element.paragraph
                        [ Element.width (Element.fill |> Element.minimum 300)
                        , Element.spacing spacer.px8
                        , Font.regular
                        ]
                        [ Element.text "Shows a dismissable banner when a new version of Exosphere is available. (Refresh your browser for this setting take effect.)" ]
                    )
                    ST.PositionRight
    in
    Element.column (VH.formContainer ++ [ Element.spacing spacer.px32 ])
        [ Input.radio
            [ Element.spacing spacer.px12 ]
            { onChange = SelectTheme
            , options =
                [ Input.option (ST.Override ST.Light) (Element.text "Light")
                , Input.option (ST.Override ST.Dark) (Element.text "Dark")
                , Input.option ST.System (Element.text "System")
                ]
            , selected =
                Just sharedModel.style.styleMode.theme
            , label =
                Input.labelAbove VH.radioLabelAttributes
                    (Text.strong "Color theme")
            }
        , Input.radio
            [ Element.spacing spacer.px12 ]
            { onChange =
                \newChoice ->
                    GotEnableExperimentalFeatures newChoice
            , options =
                [ Input.option False (Element.text "Disabled")
                , Input.option True (Element.text "Enabled")
                ]
            , selected =
                Just sharedModel.viewContext.experimentalFeaturesEnabled
            , label =
                Input.labelAbove
                    VH.radioLabelAttributes
                    (Text.text Text.Emphasized
                        [ Element.onRight experimentalFeatureToggleTip
                        ]
                        "Experimental features"
                    )
            }
        , Input.radio
            [ Element.spacing spacer.px12 ]
            { onChange =
                \newChoice ->
                    GotEnableAppVersionUpdateNotifications newChoice
            , options =
                [ Input.option False (Element.text "Disabled")
                , Input.option True (Element.text "Enabled")
                ]
            , selected =
                Just sharedModel.viewContext.appVersionUpdateNotificationsEnabled
            , label =
                Input.labelAbove
                    VH.radioLabelAttributes
                    (Text.text Text.Emphasized
                        [ Element.onRight appVersionUpdateNotificationsToggleTip
                        ]
                        "Show App Update Notifications"
                    )
            }
        , extensionsSection context
        ]


{-| The "Extensions" section lists the per-instance extension approvals the user has granted
(the `exoext` dynamic-UI mechanism), each with a Forget action. It is hidden entirely when no
approval exists, so users who never enabled an extension never see it. Layout follows the flat
Settings column; no new table widget.
-}
extensionsSection : View.Types.Context -> Element.Element Msg
extensionsSection context =
    case context.extensionApprovals of
        [] ->
            Element.none

        approvals ->
            let
                extensionsToggleTip =
                    Element.el [ Element.paddingXY spacer.px8 0 ] <|
                        Style.Widgets.ToggleTip.toggleTip
                            context
                            (\extensionsTipId -> SharedMsg <| SharedMsg.TogglePopover extensionsTipId)
                            "settingsExtensionsToggleTip"
                            (Element.paragraph
                                [ Element.width (Element.fill |> Element.minimum 300)
                                , Element.spacing spacer.px8
                                , Font.regular
                                ]
                                [ Element.text ("Extensions are an experimental feature. Each interface is published by " ++ Helpers.String.indefiniteArticle context.localization.virtualComputer ++ " " ++ context.localization.virtualComputer ++ " in your " ++ context.localization.unitOfTenancy ++ ", not by Exosphere. Forgetting one returns that " ++ context.localization.virtualComputer ++ "'s card to its off state.") ]
                            )
                            ST.PositionRight
            in
            Element.column
                [ Element.spacing spacer.px16, Element.width Element.fill ]
                (Text.text Text.Emphasized
                    [ Element.onRight extensionsToggleTip ]
                    "Extensions"
                    :: List.map (extensionApprovalRow context) approvals
                )


extensionApprovalRow : View.Types.Context -> ExtensionApproval -> Element.Element Msg
extensionApprovalRow context approval =
    let
        subdued =
            Font.color (SH.toElementColor context.palette.neutral.text.subdued)

        approvedAtDisplay =
            Helpers.Time.iso8601StringToPosix approval.approvedAt
                |> Result.map Helpers.Time.humanReadableDateAndTime
                |> Result.withDefault approval.approvedAt

        displayName =
            if approval.nameAtApproval == "" then
                "(unnamed " ++ context.localization.virtualComputer ++ ")"

            else
                approval.nameAtApproval

        detailLine label value =
            Element.wrappedRow [ Element.spacing spacer.px8, subdued, Text.fontSize Text.Small ]
                [ Element.text label
                , value
                ]
    in
    Element.column
        [ Element.spacing spacer.px8
        , Element.width Element.fill
        ]
        [ Element.row [ Element.spacing spacer.px12, Element.width Element.fill ]
            [ Text.strong displayName
            , Element.el [ Element.alignRight ]
                (Button.default context.palette
                    { text = "Forget"
                    , onPress = Just (SharedMsg (SharedMsg.ForgetExtensionApproval approval.instanceUuid))
                    }
                )
            ]
        , detailLine (Helpers.String.toTitleCase context.localization.virtualComputer) (copyableUuid context.palette approval.instanceUuid)
        , detailLine (Helpers.String.toTitleCase context.localization.openstackWithOwnKeystone) (Element.text approval.cloudUrl)
        , detailLine (Helpers.String.toTitleCase context.localization.unitOfTenancy) (copyableUuid context.palette approval.projectUuid)
        , detailLine "Approved" (Element.text approvedAtDisplay)
        ]
