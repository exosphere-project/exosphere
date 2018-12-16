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
        ]
