-- This is minimally required for Exosphere to compile with elm-style-framework dependency


module MyStyle exposing (configuration)

import Dict


configuration : Dict.Dict String String
configuration =
    Dict.fromList
        [ ( "font_url", "https://exosphere.gitlab.io/exosphere/fonts/open-sans-regular-400.css" )
        , ( "font_typeface", "Open Sans" )
        , ( "font_fallback", "sans-serif" )
        , ( "primary", "#0088ce" )
        , ( "danger", "#b80000" )
        , ( "black", "#030303" )
        , ( "black_bis", "#292e34" )
        , ( "black_ter", "#393f44" )
        , ( "grey_darker", "#393f44" )
        , ( "grey_dark", "#393f44" )
        , ( "grey", "#8b8d8f" )
        , ( "grey_light", "#bbbbbb" )
        , ( "grey_lighter", "#d1d1d1" )
        , ( "white_ter", "#ededed" )
        , ( "white_bis", "#f5f5f5" )
        , ( "white", "#ffffff" )
        ]
