module Style.Widgets.Icon exposing
    ( bell
    , chevronUp
    , copyToClipboard
    , downArrow
    , lock
    , lockOpen
    , plusCircle
    , question
    , remove
    , rightArrow
    , roundRect
    , timesCircle
    , unlock
    , upArrow
    , windowClose
    )

import Color
import Element
import Html
import Svg
import Svg.Attributes as SA


windowClose : Color.Color -> Int -> Element.Element msg
windowClose cl size =
    Element.el [] <|
        Element.html <|
            windowClose_ cl size


windowClose_ : Color.Color -> Int -> Html.Html msg
windowClose_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 1792 1792"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)
            , SA.d "M1175 1321l146-146q10-10 10-23t-10-23l-233-233 233-233q10-10 10-23t-10-23l-146-146q-10-10-23-10t-23 10l-233 233-233-233q-10-10-23-10t-23 10l-146 146q-10 10-10 23t10 23l233 233-233 233q-10 10-10 23t10 23l146 146q10 10 23 10t23-10l233-233 233 233q10 10 23 10t23-10zm617-1033v1216q0 66-47 113t-113 47h-1472q-66 0-113-47t-47-113v-1216q0-66 47-113t113-47h1472q66 0 113 47t47 113z"
            ]
            []
        ]


bell : Color.Color -> Int -> Element.Element msg
bell cl size =
    Element.el [] <|
        Element.html <|
            bell_ cl size


bell_ : Color.Color -> Int -> Html.Html msg
bell_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 500 500"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)
            , SA.d "M224 512c35.32 0 63.97-28.65 63.97-64H160.03c0 35.35 28.65 64 63.97 64zm215.39-149.71c-19.32-20.76-55.47-51.99-55.47-154.29 0-77.7-54.48-139.9-127.94-155.16V32c0-17.67-14.32-32-31.98-32s-31.98 14.33-31.98 32v20.84C118.56 68.1 64.08 130.3 64.08 208c0 102.3-36.15 133.53-55.47 154.29-6 6.45-8.66 14.16-8.61 21.71.11 16.4 12.98 32 32.1 32h383.8c19.12 0 32-15.6 32.1-32 .05-7.55-2.61-15.27-8.61-21.71z"
            ]
            []
        ]


question : Color.Color -> Int -> Element.Element msg
question cl size =
    Element.el [] <|
        Element.html <|
            question_ cl size


question_ : Color.Color -> Int -> Html.Html msg
question_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 1792 1792"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)

            -- From https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/black/svg/question-circle.svg
            , SA.d "M1024 1376v-192q0-14-9-23t-23-9h-192q-14 0-23 9t-9 23v192q0 14 9 23t23 9h192q14 0 23-9t9-23zm256-672q0-88-55.5-163t-138.5-116-170-41q-243 0-371 213-15 24 8 42l132 100q7 6 19 6 16 0 25-12 53-68 86-92 34-24 86-24 48 0 85.5 26t37.5 59q0 38-20 61t-68 45q-63 28-115.5 86.5t-52.5 125.5v36q0 14 9 23t23 9h192q14 0 23-9t9-23q0-19 21.5-49.5t54.5-49.5q32-18 49-28.5t46-35 44.5-48 28-60.5 12.5-81zm384 192q0 209-103 385.5t-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103 385.5 103 279.5 279.5 103 385.5z"
            ]
            []
        ]


downArrow : Color.Color -> Int -> Element.Element msg
downArrow cl size =
    Element.el [] <|
        Element.html <|
            downArrow_ cl size


downArrow_ : Color.Color -> Int -> Html.Html msg
downArrow_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 1792 1792"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)

            -- From https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/black/svg/arrow-down.svg
            , SA.d "M1675 832q0 53-37 90l-651 652q-39 37-91 37-53 0-90-37l-651-652q-38-36-38-90 0-53 38-91l74-75q39-37 91-37 53 0 90 37l294 294v-704q0-52 38-90t90-38h128q52 0 90 38t38 90v704l294-294q37-37 90-37 52 0 91 37l75 75q37 39 37 91z"
            ]
            []
        ]


upArrow : Color.Color -> Int -> Element.Element msg
upArrow cl size =
    Element.el [] <|
        Element.html <|
            upArrow_ cl size


upArrow_ : Color.Color -> Int -> Html.Html msg
upArrow_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 1792 1792"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)

            -- From https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/black/svg/arrow-up.svg
            , SA.d "M1675 971q0 51-37 90l-75 75q-38 38-91 38-54 0-90-38l-294-293v704q0 52-37.5 84.5t-90.5 32.5h-128q-53 0-90.5-32.5t-37.5-84.5v-704l-294 293q-36 38-90 38t-90-38l-75-75q-38-38-38-90 0-53 38-91l651-651q35-37 90-37 54 0 91 37l651 651q37 39 37 91z"
            ]
            []
        ]


rightArrow : Color.Color -> Int -> Element.Element msg
rightArrow cl size =
    Element.el [] <|
        Element.html <|
            rightArrow_ cl size


rightArrow_ : Color.Color -> Int -> Html.Html msg
rightArrow_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 1792 1792"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)

            -- From https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/black/svg/question-circle.svg
            , SA.d "M1363 877l-742 742q-19 19-45 19t-45-19l-166-166q-19-19-19-45t19-45l531-531-531-531q-19-19-19-45t19-45l166-166q19-19 45-19t45 19l742 742q19 19 19 45t-19 45z"
            ]
            []
        ]


remove : Color.Color -> Int -> Element.Element msg
remove cl size =
    Element.el [] <|
        Element.html <|
            remove_ cl size


remove_ : Color.Color -> Int -> Html.Html msg
remove_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 12 14"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)
            , SA.d "M2 14h8l1-9h-10l1 9zM8 1.996v-1.996h-4v1.996l-4 0.004v3l0.998-1h10.002l1 1v-3.004h-4zM6.986 1.996h-1.986v-0.998h1.986v0.998z"
            ]
            []
        ]


roundRect : Color.Color -> Int -> Element.Element msg
roundRect cl size =
    Element.el [] <|
        Element.html <|
            roundRect_ cl size


roundRect_ : Color.Color -> Int -> Html.Html msg
roundRect_ cl size =
    Svg.svg
        [ SA.width <| String.fromInt size, SA.height <| String.fromInt size, SA.viewBox "0 0 60 60" ]
        [ Svg.rect [ SA.x "0", SA.y "0", SA.width "60", SA.height "60", SA.rx "15", SA.ry "15", SA.fill (Color.colorToHex cl) ] [] ]


copyToClipboard : Color.Color -> Int -> Element.Element msg
copyToClipboard cl size =
    Element.el [] <|
        Element.html <|
            copyToClipboard_ cl size


copyToClipboard_ : Color.Color -> Int -> Html.Html msg
copyToClipboard_ cl size =
    {- https://octicons.github.com/icon/clippy/ -}
    Svg.svg
        [ SA.viewBox "0 0 14 16"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)
            , SA.d "M2 13h4v1H2v-1zm5-6H2v1h5V7zm2 3V8l-3 3 3 3v-2h5v-2H9zM4.5 9H2v1h2.5V9zM2 12h2.5v-1H2v1zm9 1h1v2c-.02.28-.11.52-.3.7-.19.18-.42.28-.7.3H1c-.55 0-1-.45-1-1V4c0-.55.45-1 1-1h3c0-1.11.89-2 2-2 1.11 0 2 .89 2 2h3c.55 0 1 .45 1 1v5h-1V6H1v9h10v-2zM2 5h8c0-.55-.45-1-1-1H8c-.55 0-1-.45-1-1s-.45-1-1-1-1 .45-1 1-.45 1-1 1H3c-.55 0-1 .45-1 1z"
            ]
            []
        ]


timesCircle : Color.Color -> Int -> Element.Element msg
timesCircle cl size =
    Element.el [] <|
        Element.html <|
            timesCircle_ cl size


timesCircle_ : Color.Color -> Int -> Html.Html msg
timesCircle_ cl size =
    Svg.svg
        [ SA.width <| String.fromInt size
        , SA.height <| String.fromInt size
        , SA.viewBox "0 0 1792 1792"
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)
            , SA.d "M1277 1122q0-26-19-45l-181-181 181-181q19-19 19-45 0-27-19-46l-90-90q-19-19-46-19-26 0-45 19l-181 181-181-181q-19-19-45-19-27 0-46 19l-90 90q-19 19-19 46 0 26 19 45l181 181-181 181q-19 19-19 45 0 27 19 46l90 90q19 19 46 19 26 0 45-19l181-181 181 181q19 19 45 19 27 0 46-19l90-90q19-19 19-46zm387-226q0 209-103 385.5t-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103 385.5 103 279.5 279.5 103 385.5z"
            ]
            []
        ]


plusCircle : Color.Color -> Int -> Element.Element msg
plusCircle cl size =
    Element.el [] <|
        Element.html <|
            plusCircle_ cl size


plusCircle_ : Color.Color -> Int -> Html.Html msg
plusCircle_ cl size =
    Svg.svg
        [ SA.width <| String.fromInt size
        , SA.height <| String.fromInt size
        , SA.viewBox "0 0 1792 1792"
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)
            , SA.d "M1344 960v-128q0-26-19-45t-45-19h-256v-256q0-26-19-45t-45-19h-128q-26 0-45 19t-19 45v256h-256q-26 0-45 19t-19 45v128q0 26 19 45t45 19h256v256q0 26 19 45t45 19h128q26 0 45-19t19-45v-256h256q26 0 45-19t19-45zm320-64q0 209-103 385.5t-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103 385.5 103 279.5 279.5 103 385.5z"
            ]
            []
        ]


lock : Color.Color -> Int -> Element.Element msg
lock cl size =
    Element.el [] <|
        Element.html <|
            lock_ cl size


lock_ : Color.Color -> Int -> Html.Html msg
lock_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 1792 1792"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)

            -- From view-source:https://raw.githubusercontent.com/encharm/Font-Awesome-SVG-PNG/master/black/svg/lock.svg
            , SA.d "M640 768h512v-192q0-106-75-181t-181-75-181 75-75 181v192zm832 96v576q0 40-28 68t-68 28h-960q-40 0-68-28t-28-68v-576q0-40 28-68t68-28h32v-192q0-184 132-316t316-132 316 132 132 316v192h32q40 0 68 28t28 68z"
            ]
            []
        ]


unlock : Color.Color -> Int -> Element.Element msg
unlock cl size =
    Element.el [] <|
        Element.html <|
            unlock_ cl size


unlock_ : Color.Color -> Int -> Html.Html msg
unlock_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 1792 1792"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)

            -- From view-source:https://raw.githubusercontent.com/encharm/Font-Awesome-SVG-PNG/master/black/svg/unlock-alt.svg
            , SA.d "M1376 768q40 0 68 28t28 68v576q0 40-28 68t-68 28h-960q-40 0-68-28t-28-68v-576q0-40 28-68t68-28h32v-320q0-185 131.5-316.5t316.5-131.5 316.5 131.5 131.5 316.5q0 26-19 45t-45 19h-64q-26 0-45-19t-19-45q0-106-75-181t-181-75-181 75-75 181v320h736z"
            ]
            []
        ]


lockOpen : Color.Color -> Int -> Element.Element msg
lockOpen cl size =
    Element.el [] <|
        Element.html <|
            lockOpen_ cl size


lockOpen_ : Color.Color -> Int -> Html.Html msg
lockOpen_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 1792 1792"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)

            -- From view-source:https://raw.githubusercontent.com/encharm/Font-Awesome-SVG-PNG/master/black/svg/unlock.svg
            , SA.d "M1728 576v256q0 26-19 45t-45 19h-64q-26 0-45-19t-19-45v-256q0-106-75-181t-181-75-181 75-75 181v192h96q40 0 68 28t28 68v576q0 40-28 68t-68 28h-960q-40 0-68-28t-28-68v-576q0-40 28-68t68-28h672v-192q0-185 131.5-316.5t316.5-131.5 316.5 131.5 131.5 316.5z"
            ]
            []
        ]


chevronUp : Color.Color -> Int -> Element.Element msg
chevronUp cl size =
    Element.el [] <|
        Element.html <|
            chevronUp_ cl size


chevronUp_ : Color.Color -> Int -> Html.Html msg
chevronUp_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 256 256"
        , SA.height <| String.fromInt size
        , SA.fill "none"
        , SA.stroke "currentColor"
        , SA.stroke (Color.colorToHex cl)
        , SA.strokeWidth "4"
        , SA.viewBox "0 0 24 24"
        , SA.width <| String.fromInt size
        ]
        [ Svg.polyline
            [ SA.points "22 20 12 9 2 20"
            ]
            []
        ]
