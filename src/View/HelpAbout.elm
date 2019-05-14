module View.HelpAbout exposing (helpAbout)

import Element
import Types.Types exposing (..)
import View.Helpers as VH


helpAbout : Element.Element Msg
helpAbout =
    Element.column (List.append VH.exoColumnAttributes [ Element.spacing 30 ])
        [ Element.el VH.heading2 <| Element.text "Getting Help"
        , Element.paragraph []
            [ Element.text "To ask for help, report a bug, or request a new feature, "
            , VH.browserLink
                "https://gitlab.com/exosphere/exosphere/issues"
                "create an issue"
            , Element.text " on Exosphere's GitLab project. Someone will respond within a day or so. For real-time assistance, see if anyone is on "
            , VH.browserLink
                "https://c-mart.sandcats.io/shared/ak1ymBWynN1MZe0ot1yEBOh6RF6fZ9G2ZOo2xhnmVC5"
                "Exosphere Chat"
            , Element.text ". Sign in there with your email or GitHub account."
            ]
        , Element.el VH.heading2 <| Element.text "About Exosphere"
        , Element.paragraph []
            [ Element.text "Exosphere is a user-friendly, extensible client for cloud computing. Check out our "
            , VH.browserLink "https://gitlab.com/exosphere/exosphere/blob/master/README.md" "README on GitLab"
            , Element.text "."
            ]
        ]
