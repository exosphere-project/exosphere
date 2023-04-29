module Style.Widgets.HumanTime exposing (relativeTimeElement)

import DateFormat.Relative
import Element exposing (Element)
import Helpers.Time exposing (humanReadableDateAndTime)
import Html exposing (text, time)
import Html.Attributes exposing (datetime, title)
import Time


{-| Displays a readable relative time with a title/hover showing the full time.
-}
relativeTimeElement : Time.Posix -> Time.Posix -> Element msg
relativeTimeElement currentTime timestamp =
    let
        fullTime =
            humanReadableDateAndTime timestamp

        relativeTime =
            DateFormat.Relative.relativeTime currentTime timestamp
    in
    Element.html (time [ datetime fullTime, title fullTime ] [ text relativeTime ])
