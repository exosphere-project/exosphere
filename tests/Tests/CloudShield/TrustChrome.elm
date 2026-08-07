module Tests.CloudShield.TrustChrome exposing (suite)

{-| The two sentences Exosphere says in its OWN voice around a published card: the §5.3 opt-in
prompt (shown before anything the publisher wrote is rendered at all) and the §5.2 provenance
marker (shown above everything it renders afterwards).

They are pinned here because they are the card's trust surface and because they are the one place
on it where Exosphere's own vocabulary applies. Every other page calls a server by the noun the
deployer configured; these read the same field, so a deployer who calls them something else sees
that word in the prompt that asks for their trust.

-}

import CloudShield.Card as Card
import Element
import Style.Helpers as SH
import Style.Types as ST
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.CloudShield.Fixtures exposing (cardViewConfig)
import Time
import Types.Defaults
import Types.HelperTypes as HelperTypes


palette : ST.ExoPalette
palette =
    SH.toExoPalette ST.defaultColors { theme = ST.System, systemPreference = Nothing }


{-| A deployer who calls a server something other than the default "instance". Nothing else about
the card changes, so any difference below is the localization reaching the copy.
-}
customLocalization : HelperTypes.Localization
customLocalization =
    let
        base =
            Types.Defaults.localization
    in
    { base | virtualComputer = "cloud server" }


render : HelperTypes.Localization -> Bool -> Query.Single Card.Msg
render localization approved =
    Card.view palette localization Time.utc { cardViewConfig | approved = approved } [] Card.init
        |> Element.layout []
        |> Query.fromHtml


suite : Test
suite =
    describe "the card's trust chrome speaks the deployer's vocabulary"
        [ test "the opt-in prompt names the publisher with the configured noun" <|
            \_ ->
                render Types.Defaults.localization False
                    |> Query.has
                        [ Selector.text "The instance “cloudshield-vm” offers an extension UI. Extensions are off until you enable them. Enabling is remembered for this instance; you can forget it any time." ]
        , test "a deployer's own noun reaches the opt-in prompt, both times it appears" <|
            \_ ->
                render customLocalization False
                    |> Query.has
                        [ Selector.text "The cloud server “cloudshield-vm” offers an extension UI. Extensions are off until you enable them. Enabling is remembered for this cloud server; you can forget it any time." ]
        , test "the provenance marker names the publisher with the configured noun" <|
            \_ ->
                render Types.Defaults.localization True
                    |> Query.has
                        [ Selector.text "Published by the \"cloudshield-vm\" instance, not verified by Exosphere." ]
        , test "a deployer's own noun reaches the provenance marker" <|
            \_ ->
                render customLocalization True
                    |> Query.has
                        [ Selector.text "Published by the \"cloudshield-vm\" cloud server, not verified by Exosphere." ]
        , test "the opt-in gate is the whole gate: an unapproved card draws no provenance marker" <|
            \_ ->
                -- Guards the fixtures above rather than the copy: the two sentences are alternatives,
                -- so a test asserting one must not be silently reading the other's render.
                render Types.Defaults.localization False
                    |> Query.hasNot [ Selector.text "not verified by Exosphere." ]
        ]
