module Page.Jetstream2Allocation exposing (view)

import DateFormat.Relative
import Element
import Element.Font as Font
import Element.Region as Region
import FormatNumber.Locales
import Helpers.Formatting
import Helpers.String
import Helpers.Time
import Style.Types as ST
import Style.Widgets.Meter
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Time
import Types.Jetstream2Accounting as Accounting
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH exposing (edges)
import View.Types


view : View.Types.Context -> Project -> Time.Posix -> Element.Element SharedMsg.SharedMsg
view context project currentTime =
    let
        meter : Accounting.Allocation -> Element.Element SharedMsg.SharedMsg
        meter allocation =
            let
                serviceUnitsUsed =
                    allocation.serviceUnitsUsed |> Maybe.map round |> Maybe.withDefault 0

                serviceUnitsAllocated =
                    allocation.serviceUnitsAllocated |> round

                serviceUnitsRemaining =
                    max 0 (serviceUnitsAllocated - serviceUnitsUsed)

                title =
                    Accounting.resourceToStr context.localization.virtualComputer allocation.resource

                subtitle =
                    -- Hard-coding USA locale to work around some kind of bug in elm-format-number where 1000000 is rendered as 10,00,000.
                    -- Don't worry, approximately all Jetstream2 users are USA-based, and nobody else will see this.
                    String.join " "
                        [ serviceUnitsRemaining
                            |> Helpers.Formatting.humanCount FormatNumber.Locales.usLocale
                        , "SUs remaining"
                        ]
            in
            Style.Widgets.Meter.meter
                context.palette
                title
                subtitle
                serviceUnitsUsed
                serviceUnitsAllocated

        toggleTip : Accounting.Allocation -> Element.Element SharedMsg.SharedMsg
        toggleTip allocation =
            let
                contents : Element.Element SharedMsg.SharedMsg
                contents =
                    [ String.concat
                        [ "Start: "
                        , DateFormat.Relative.relativeTime currentTime allocation.startDate
                        , " ("
                        , Helpers.Time.humanReadableDate allocation.startDate
                        , ")"
                        ]
                    , String.concat
                        [ "End: "
                        , DateFormat.Relative.relativeTime currentTime allocation.endDate
                        , " ("
                        , Helpers.Time.humanReadableDate allocation.endDate
                        , ")"
                        ]
                    , String.concat
                        [ "Original Allocation: "
                        , allocation.serviceUnitsAllocated
                            |> round
                            |> Helpers.Formatting.humanCount FormatNumber.Locales.usLocale
                        , " SUs"
                        ]
                    ]
                        |> List.map Element.text
                        |> Element.column []

                toggleTipId =
                    Helpers.String.hyphenate
                        [ "JS2AllocationTip"
                        , project.auth.project.uuid
                        , Accounting.resourceToStr context.localization.virtualComputer allocation.resource
                        ]
            in
            Style.Widgets.ToggleTip.toggleTip
                context
                (\toggleTipId_ -> SharedMsg.TogglePopover toggleTipId_)
                toggleTipId
                contents
                ST.PositionRight

        renderAllocation : Accounting.Allocation -> Element.Element SharedMsg.SharedMsg
        renderAllocation allocation =
            Element.row [ Element.spacing spacer.px8 ]
                [ meter allocation
                , Element.el
                    [ Element.alignBottom
                    , Element.paddingEach { edges | bottom = 2 }
                    ]
                    (toggleTip allocation)
                ]

        renderRDPPSuccess : List Accounting.Allocation -> Element.Element SharedMsg.SharedMsg
        renderRDPPSuccess allocations =
            let
                shownAndSortedAllocations =
                    Accounting.shownAndSortedAllocations currentTime allocations

                heading =
                    Text.text Text.Large
                        [ Font.regular, Region.heading 2 ]
                        "Allocation Usage"

                headingToggleTip =
                    let
                        text =
                            "Allocation usage is updated every 24 hours. Usage displayed here may be up to a day old."

                        contents =
                            Element.text text
                    in
                    Style.Widgets.ToggleTip.toggleTip
                        context
                        (\toggleTipId -> SharedMsg.TogglePopover toggleTipId)
                        "allocation-usage-heading-toggle-tip"
                        contents
                        ST.PositionRight
            in
            Element.column
                [ Element.spacing spacer.px24 ]
                [ Element.row [ Element.spacing spacer.px12 ]
                    [ heading, headingToggleTip ]
                , Element.wrappedRow
                    [ Element.paddingEach { edges | bottom = spacer.px12 }
                    , Element.spacing spacer.px24
                    ]
                    (List.map renderAllocation shownAndSortedAllocations)
                ]
    in
    case project.endpoints.jetstream2Accounting of
        Just _ ->
            -- Is a Jetstream2 project
            VH.renderRDPP context project.jetstream2Allocations "allocation" renderRDPPSuccess

        Nothing ->
            -- Is not a Jetstream2 project
            Element.none
