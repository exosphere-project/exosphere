module Helpers.ResourceList exposing
    ( creationTimeFilterOptions
    , creatorFilterOptions
    , listItemColumnAttribs
    , onCreationTimeFilter
    )

import Dict
import Element
import Element.Font as Font
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.DataList exposing (FilterOptionText, FilterOptionValue)
import Style.Widgets.Spacer exposing (spacer)
import Time
import Types.Project exposing (Project)


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
    case String.toInt optionValue of
        Just optionInTimePeriod ->
            let
                timeElapsedSinceCreation =
                    Time.posixToMillis currentTime
                        - Time.posixToMillis resourceCreationTime
            in
            timeElapsedSinceCreation <= optionInTimePeriod

        Nothing ->
            True


listItemColumnAttribs : ExoPalette -> List (Element.Attribute msg)
listItemColumnAttribs palette =
    [ Element.spacing spacer.px12
    , Element.width Element.fill
    , Font.color (SH.toElementColor palette.neutral.text.subdued)
    ]


creatorFilterOptions : Project -> List String -> Dict.Dict String String
creatorFilterOptions project =
    List.map
        (\creator ->
            ( creator
            , if creator == project.auth.user.name then
                "me (" ++ creator ++ ")"

              else
                creator
            )
        )
        >> Dict.fromList
