module View.FloatingIps exposing (floatingIps)

import Element exposing (Element)
import Types.Types exposing (FloatingIpListViewParams, Msg(..), Project)
import View.Types


floatingIps : View.Types.Context -> Bool -> Project -> FloatingIpListViewParams -> (FloatingIpListViewParams -> Msg) -> Element.Element Msg
floatingIps context showHeading project viewParams toMsg =
    Element.text "TODO"
