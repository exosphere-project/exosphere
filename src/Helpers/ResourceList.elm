module Helpers.ResourceList exposing (creationTimeFilterOptions, onCreationTimeFilter)

import Dict
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
