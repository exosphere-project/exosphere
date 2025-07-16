module Style.Widgets.Icon exposing
    ( Icon(..)
    , bell
    , console
    , copyToClipboard
    , featherIcon
    , history
    , iconEl
    , ipAddress
    , lock
    , lockOpen
    , plusCircle
    , remove
    , roundRect
    , sizedFeatherIcon
    , timesCircle
    )

import Color
import Element
import FeatherIcons
import Html
import Style.Helpers as SH
import Svg
import Svg.Attributes as SA


type Icon
    = Bell
    | HelpCircle
    | Info
    | Settings


{-| Creates an Elm-UI Element for a given FeatherIcons icon.

This function accepts a list of Elm-UI attributes which can be passed in
to apply the attributes directly, obviating the need for neutral wrapper
Element.el elements. For example, one can pass in `alignTop` to vertically
align the icon in the surrounding context.

    featherIcon [ Element.alignTop ] FeatherIcons.server

If all you need is a basic icon pass an empty array of attributes.

    featherIcon [] FeatherIcons.chevronDown

If you need the basic icon but want to specify a size, see [sizedFeatherIcon](Style.Widgets.Icon#sizedFeatherIcon).

-}
featherIcon : List (Element.Attribute msg) -> FeatherIcons.Icon -> Element.Element msg
featherIcon attributes icon =
    icon
        |> FeatherIcons.toHtml []
        |> Element.html
        |> Element.el attributes


{-| Creates an Elm-UI Element for a given FeatherIcons icon with a specific size.

This is a simpler version of [featherIcon](Style.Widgets.Icon#featherIcon) which
creates a plain Element given a `FeatherIcons` icon and a basic size.

    sizedFeatherIcon 18 FeatherIcons.hardDrive

If you need anything more complicated than this, then `featherIcon` is more
appropriate, as it allows passing arbitrary Elm-UI attributes.

Since `FeatherIcon`'s functions to add image size also return the `FeatherIcon.Icon`
type, it's possible to create a size simply by piping an icon into `FeatherIcon.withSize`
method, but this helper function exists because of how common it is within Exosphere
to create an icon with a specific known size.

-}
sizedFeatherIcon : Float -> FeatherIcons.Icon -> Element.Element msg
sizedFeatherIcon size icon =
    icon
        |> FeatherIcons.withSize size
        |> featherIcon []


iconEl : List (Element.Attribute msg) -> Icon -> Int -> Color.Color -> Element.Element msg
iconEl attributes icon size color =
    let
        svg =
            case icon of
                Bell ->
                    bell_

                HelpCircle ->
                    fromFeather FeatherIcons.helpCircle

                Info ->
                    fromFeather FeatherIcons.info

                Settings ->
                    fromFeather FeatherIcons.settings
    in
    toEl attributes (svg (SH.toElementColor color) size)


bell : Element.Color -> Int -> Element.Element msg
bell cl size =
    Element.el [] <|
        Element.html <|
            bell_ cl size


bell_ : Element.Color -> Int -> Html.Html msg
bell_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 500 500"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (elementColorToSvgColor cl)
            , SA.d "M224 512c35.32 0 63.97-28.65 63.97-64H160.03c0 35.35 28.65 64 63.97 64zm215.39-149.71c-19.32-20.76-55.47-51.99-55.47-154.29 0-77.7-54.48-139.9-127.94-155.16V32c0-17.67-14.32-32-31.98-32s-31.98 14.33-31.98 32v20.84C118.56 68.1 64.08 130.3 64.08 208c0 102.3-36.15 133.53-55.47 154.29-6 6.45-8.66 14.16-8.61 21.71.11 16.4 12.98 32 32.1 32h383.8c19.12 0 32-15.6 32.1-32 .05-7.55-2.61-15.27-8.61-21.71z"
            ]
            []
        ]


remove : Element.Color -> Int -> Element.Element msg
remove cl size =
    Element.el [] <|
        Element.html <|
            remove_ cl size


remove_ : Element.Color -> Int -> Html.Html msg
remove_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 12 14"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (elementColorToSvgColor cl)
            , SA.d "M2 14h8l1-9h-10l1 9zM8 1.996v-1.996h-4v1.996l-4 0.004v3l0.998-1h10.002l1 1v-3.004h-4zM6.986 1.996h-1.986v-0.998h1.986v0.998z"
            ]
            []
        ]


roundRect : Element.Color -> Int -> Element.Element msg
roundRect cl size =
    Element.el [] <|
        Element.html <|
            roundRect_ cl size


roundRect_ : Element.Color -> Int -> Html.Html msg
roundRect_ cl size =
    Svg.svg
        [ SA.width <| String.fromInt size, SA.height <| String.fromInt size, SA.viewBox "0 0 60 60" ]
        [ Svg.rect [ SA.x "0", SA.y "0", SA.width "60", SA.height "60", SA.rx "15", SA.ry "15", SA.fill (elementColorToSvgColor cl) ] [] ]


copyToClipboard : Element.Color -> Int -> Element.Element msg
copyToClipboard cl size =
    Element.el [] <|
        Element.html <|
            copyToClipboard_ cl size


copyToClipboard_ : Element.Color -> Int -> Html.Html msg
copyToClipboard_ cl size =
    {- https://octicons.github.com/icon/clippy/ -}
    Svg.svg
        [ SA.viewBox "0 0 14 16"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (elementColorToSvgColor cl)
            , SA.d "M2 13h4v1H2v-1zm5-6H2v1h5V7zm2 3V8l-3 3 3 3v-2h5v-2H9zM4.5 9H2v1h2.5V9zM2 12h2.5v-1H2v1zm9 1h1v2c-.02.28-.11.52-.3.7-.19.18-.42.28-.7.3H1c-.55 0-1-.45-1-1V4c0-.55.45-1 1-1h3c0-1.11.89-2 2-2 1.11 0 2 .89 2 2h3c.55 0 1 .45 1 1v5h-1V6H1v9h10v-2zM2 5h8c0-.55-.45-1-1-1H8c-.55 0-1-.45-1-1s-.45-1-1-1-1 .45-1 1-.45 1-1 1H3c-.55 0-1 .45-1 1z"
            ]
            []
        ]


timesCircle : Element.Color -> Int -> Element.Element msg
timesCircle cl size =
    Element.el [] <|
        Element.html <|
            timesCircle_ cl size


timesCircle_ : Element.Color -> Int -> Html.Html msg
timesCircle_ cl size =
    Svg.svg
        [ SA.width <| String.fromInt size
        , SA.height <| String.fromInt size
        , SA.viewBox "0 0 1792 1792"
        ]
        [ Svg.path
            [ SA.fill (elementColorToSvgColor cl)
            , SA.d "M1277 1122q0-26-19-45l-181-181 181-181q19-19 19-45 0-27-19-46l-90-90q-19-19-46-19-26 0-45 19l-181 181-181-181q-19-19-45-19-27 0-46 19l-90 90q-19 19-19 46 0 26 19 45l181 181-181 181q-19 19-19 45 0 27 19 46l90 90q19 19 46 19 26 0 45-19l181-181 181 181q19 19 45 19 27 0 46-19l90-90q19-19 19-46zm387-226q0 209-103 385.5t-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103 385.5 103 279.5 279.5 103 385.5z"
            ]
            []
        ]


plusCircle : Element.Color -> Int -> Element.Element msg
plusCircle cl size =
    Element.el [] <|
        Element.html <|
            plusCircle_ cl size


plusCircle_ : Element.Color -> Int -> Html.Html msg
plusCircle_ cl size =
    Svg.svg
        [ SA.width <| String.fromInt size
        , SA.height <| String.fromInt size
        , SA.viewBox "0 0 1792 1792"
        ]
        [ Svg.path
            [ SA.fill (elementColorToSvgColor cl)
            , SA.d "M1344 960v-128q0-26-19-45t-45-19h-256v-256q0-26-19-45t-45-19h-128q-26 0-45 19t-19 45v256h-256q-26 0-45 19t-19 45v128q0 26 19 45t45 19h256v256q0 26 19 45t45 19h128q26 0 45-19t19-45v-256h256q26 0 45-19t19-45zm320-64q0 209-103 385.5t-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103 385.5 103 279.5 279.5 103 385.5z"
            ]
            []
        ]


lock : Element.Color -> Int -> Element.Element msg
lock cl size =
    Element.el [] <|
        Element.html <|
            lock_ cl size


lock_ : Element.Color -> Int -> Html.Html msg
lock_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 1792 1792"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (elementColorToSvgColor cl)

            -- From view-source:https://raw.githubusercontent.com/encharm/Font-Awesome-SVG-PNG/master/black/svg/lock.svg
            , SA.d "M640 768h512v-192q0-106-75-181t-181-75-181 75-75 181v192zm832 96v576q0 40-28 68t-68 28h-960q-40 0-68-28t-28-68v-576q0-40 28-68t68-28h32v-192q0-184 132-316t316-132 316 132 132 316v192h32q40 0 68 28t28 68z"
            ]
            []
        ]


lockOpen : Element.Color -> Int -> Element.Element msg
lockOpen cl size =
    Element.el [] <|
        Element.html <|
            lockOpen_ cl size


lockOpen_ : Element.Color -> Int -> Html.Html msg
lockOpen_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 1792 1792"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (elementColorToSvgColor cl)

            -- From view-source:https://raw.githubusercontent.com/encharm/Font-Awesome-SVG-PNG/master/black/svg/unlock.svg
            , SA.d "M1728 576v256q0 26-19 45t-45 19h-64q-26 0-45-19t-19-45v-256q0-106-75-181t-181-75-181 75-75 181v192h96q40 0 68 28t28 68v576q0 40-28 68t-68 28h-960q-40 0-68-28t-28-68v-576q0-40 28-68t68-28h672v-192q0-185 131.5-316.5t316.5-131.5 316.5 131.5 131.5 316.5z"
            ]
            []
        ]


console : Element.Color -> Int -> Element.Element msg
console cl size =
    Element.el [] <|
        Element.html <|
            console_ cl size


console_ : Element.Color -> Int -> Html.Html msg
console_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 26 26"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (elementColorToSvgColor cl)

            -- From https://visualpharm.com/assets/211/Vga-595b40b65ba036ed117d4d45.svg
            , SA.d "M 3.09375 6 C 1.2296474 6 -0.30515611 7.6378133 0 9.46875 L 1.40625 17.40625 L 1.40625 17.46875 L 1.4375 17.46875 C 1.6285981 18.972505 2.9636894 20 4.40625 20 L 21.59375 20 C 23.060417 20 24.310778 18.914861 24.59375 17.5 A 1.0001 1.0001 0 0 0 24.59375 17.46875 L 26 9.46875 C 26.29627 7.6911319 24.864103 6 23 6 L 3.09375 6 z M 3.09375 8 L 23 8 C 23.735897 8 24.10373 8.5026181 24 9.125 L 22.625 17.09375 C 22.6229 17.10439 22.6273 17.11454 22.625 17.125 C 22.50113 17.6897 22.117386 18 21.59375 18 L 4.40625 18 C 3.8729167 18 3.4517364 17.642364 3.40625 17.1875 A 1.0001 1.0001 0 0 0 3.375 17.125 L 2 9.125 C 1.9051561 8.5559367 2.3578526 8 3.09375 8 z M 6 9 C 5.4477153 9 5 9.4477153 5 10 C 5 10.552285 5.4477153 11 6 11 C 6.5522847 11 7 10.552285 7 10 C 7 9.4477153 6.5522847 9 6 9 z M 10 9 C 9.4477153 9 9 9.4477153 9 10 C 9 10.552285 9.4477153 11 10 11 C 10.552285 11 11 10.552285 11 10 C 11 9.4477153 10.552285 9 10 9 z M 14 9 C 13.447715 9 13 9.4477153 13 10 C 13 10.552285 13.447715 11 14 11 C 14.552285 11 15 10.552285 15 10 C 15 9.4477153 14.552285 9 14 9 z M 18 9 C 17.447715 9 17 9.4477153 17 10 C 17 10.552285 17.447715 11 18 11 C 18.552285 11 19 10.552285 19 10 C 19 9.4477153 18.552285 9 18 9 z M 8 12 C 7.4477153 12 7 12.447715 7 13 C 7 13.552285 7.4477153 14 8 14 C 8.5522847 14 9 13.552285 9 13 C 9 12.447715 8.5522847 12 8 12 z M 12 12 C 11.447715 12 11 12.447715 11 13 C 11 13.552285 11.447715 14 12 14 C 12.552285 14 13 13.552285 13 13 C 13 12.447715 12.552285 12 12 12 z M 16 12 C 15.447715 12 15 12.447715 15 13 C 15 13.552285 15.447715 14 16 14 C 16.552285 14 17 13.552285 17 13 C 17 12.447715 16.552285 12 16 12 z M 20 12 C 19.447715 12 19 12.447715 19 13 C 19 13.552285 19.447715 14 20 14 C 20.552285 14 21 13.552285 21 13 C 21 12.447715 20.552285 12 20 12 z M 6 15 C 5.4477153 15 5 15.447715 5 16 C 5 16.552285 5.4477153 17 6 17 C 6.5522847 17 7 16.552285 7 16 C 7 15.447715 6.5522847 15 6 15 z M 10 15 C 9.4477153 15 9 15.447715 9 16 C 9 16.552285 9.4477153 17 10 17 C 10.552285 17 11 16.552285 11 16 C 11 15.447715 10.552285 15 10 15 z M 14 15 C 13.447715 15 13 15.447715 13 16 C 13 16.552285 13.447715 17 14 17 C 14.552285 17 15 16.552285 15 16 C 15 15.447715 14.552285 15 14 15 z M 18 15 C 17.447715 15 17 15.447715 17 16 C 17 16.552285 17.447715 17 18 17 C 18.552285 17 19 16.552285 19 16 C 19 15.447715 18.552285 15 18 15 z"
            ]
            []
        ]


ipAddress : Element.Color -> Int -> Element.Element msg
ipAddress cl size =
    Element.el [] <|
        Element.html <|
            ipAddress_ cl size


ipAddress_ : Element.Color -> Int -> Html.Html msg
ipAddress_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 20.234 20.234"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (elementColorToSvgColor cl)

            -- From https://www.svgrepo.com/svg/130884/ip-address
            , SA.d "M6.776,4.72h1.549v6.827H6.776V4.72z M11.751,4.669c-0.942,0-1.61,0.061-2.087,0.143v6.735h1.53 V9.106c0.143,0.02,0.324,0.031,0.527,0.031c0.911,0,1.691-0.224,2.218-0.721c0.405-0.386,0.628-0.952,0.628-1.621 c0-0.668-0.295-1.234-0.729-1.579C13.382,4.851,12.702,4.669,11.751,4.669z M11.709,7.95c-0.222,0-0.385-0.01-0.516-0.041V5.895 c0.111-0.03,0.324-0.061,0.639-0.061c0.769,0,1.205,0.375,1.205,1.002C13.037,7.535,12.53,7.95,11.709,7.95z M10.117,0 C5.523,0,1.8,3.723,1.8,8.316s8.317,11.918,8.317,11.918s8.317-7.324,8.317-11.917S14.711,0,10.117,0z M10.138,13.373 c-3.05,0-5.522-2.473-5.522-5.524c0-3.05,2.473-5.522,5.522-5.522c3.051,0,5.522,2.473,5.522,5.522 C15.66,10.899,13.188,13.373,10.138,13.373z"
            ]
            []
        ]


history : Element.Color -> Int -> Element.Element msg
history cl size =
    Element.el [] <|
        Element.html <|
            history_ cl size


history_ : Element.Color -> Int -> Html.Html msg
history_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 512 512"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (elementColorToSvgColor cl)

            -- From
            , SA.d "M504 255.531c.253 136.64-111.18 248.372-247.82 248.468-59.015.042-113.223-20.53-155.822-54.911-11.077-8.94-11.905-25.541-1.839-35.607l11.267-11.267c8.609-8.609 22.353-9.551 31.891-1.984C173.062 425.135 212.781 440 256 440c101.705 0 184-82.311 184-184 0-101.705-82.311-184-184-184-48.814 0-93.149 18.969-126.068 49.932l50.754 50.754c10.08 10.08 2.941 27.314-11.313 27.314H24c-8.837 0-16-7.163-16-16V38.627c0-14.254 17.234-21.393 27.314-11.314l49.372 49.372C129.209 34.136 189.552 8 256 8c136.81 0 247.747 110.78 248 247.531zm-180.912 78.784l9.823-12.63c8.138-10.463 6.253-25.542-4.21-33.679L288 256.349V152c0-13.255-10.745-24-24-24h-16c-13.255 0-24 10.745-24 24v135.651l65.409 50.874c10.463 8.137 25.541 6.253 33.679-4.21z"
            ]
            []
        ]


elementColorToSvgColor : Element.Color -> String
elementColorToSvgColor elColor =
    let
        rgb =
            Element.toRgb elColor

        strVals =
            ([ rgb.red, rgb.green, rgb.blue ]
                |> List.map (\float -> float * 255)
            )
                ++ [ rgb.alpha ]
                |> List.map String.fromFloat
                |> String.join ","
    in
    "rgba(" ++ strVals ++ ")"


fromFeather : FeatherIcons.Icon -> Element.Color -> Int -> Html.Html msg
fromFeather icon color size =
    icon
        |> FeatherIcons.withSize (1.2 * toFloat size)
        |> FeatherIcons.toHtml [ SA.stroke (elementColorToSvgColor color) ]


toEl : List (Element.Attribute msg) -> Html.Html msg -> Element.Element msg
toEl attributes html =
    Element.el attributes (Element.html html)
