module View.FloatingIps exposing (floatingIps)

import Element exposing (Element)
import Helpers.String
import Types.Types exposing (FloatingIpListViewParams, Msg(..), Project)
import View.Helpers as VH
import View.Types


floatingIps : View.Types.Context -> Bool -> Project -> FloatingIpListViewParams -> (FloatingIpListViewParams -> Msg) -> Element.Element Msg
floatingIps context showHeading project viewParams toMsg =
    let
        renderFloatingIps =
            Element.text "TODO"
    in
    Element.column
        [ Element.spacing 20, Element.width Element.fill ]
        [ if showHeading then
            Element.el VH.heading2 <|
                Element.text
                    (context.localization.pkiPublicKeyForSsh
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    )

          else
            Element.none
        , Element.text <| Debug.toString project.floatingIps
        ]
