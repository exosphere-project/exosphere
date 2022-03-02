module Helpers.ResourceList exposing
    ( creationTimeFilterOptions
    , listItemColumnAttribs
    , onCreationTimeFilter
    )

import Dict
import Element
import Element.Font as Font
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.DataList exposing (FilterOptionText, FilterOptionValue)
import Time


creationTimeFilterOptions : Dict.Dict FilterOptionValue FilterOptionText
creationTimeFilterOptions =
    Dict.fromList
        -- (milliseconds, time period text)
        -- left padded with 0s to preserve order when creating Dict
        [ ( "0086400000", "past day" )
        , ( "0604800000", "past week" )
        , ( "2592000000", "past 30 days" )
        ]


onCreationTimeFilter : FilterOptionValue -> Time.Posix -> Time.Posix -> Bool
onCreationTimeFilter optionValue resourceCreationTime currentTime =
    let
        timeElapsedSinceCreation =
            Time.posixToMillis currentTime
                - Time.posixToMillis resourceCreationTime
    in
    case String.toInt optionValue of
        Just optionInTimePeriod ->
            timeElapsedSinceCreation <= optionInTimePeriod

        Nothing ->
            True


listItemColumnAttribs : ExoPalette -> List (Element.Attribute msg)
listItemColumnAttribs palette =
    [ Element.spacing 12
    , Element.width Element.fill
    , Font.color (SH.toElementColorWithOpacity palette.on.background 0.62)
    ]
