module Tests.CloudShield.ManifestError exposing (suite)

{-| What a researcher actually sees when the fail-closed decoder refuses a manifest.

The card used to print the raw decoder diagnostic: a JSON dump wrapped in decoder prose, filling
the card, telling a researcher nothing they could act on. These tests pin the replacement — a
plain-language sentence per failure kind, and the diagnostic behind a collapsed toggle — through
`Card.view`, so they cover the wiring too (notably that the publishing instance's name reaches the
version-skew copy).

-}

import CloudShield.Card as Card
import Element
import Style.Helpers as SH
import Style.Types as ST
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time


palette : ST.ExoPalette
palette =
    SH.toExoPalette ST.defaultColors { theme = ST.System, systemPreference = Nothing }


{-| A card model that is idle apart from whether the technical-details toggle is expanded.
-}
modelWith : Bool -> Card.Model
modelWith detailExpanded =
    let
        base =
            Card.init
    in
    { base | showManifestErrorDetail = detailExpanded }


{-| An approved card whose manifest body is `manifestJson`, so `view` takes the decode path.
Everything else is the quiet default: no history, no session, nothing in flight.
-}
configFor : String -> Card.ViewConfig
configFor manifestJson =
    { approved = True
    , sourceName = "cloudshield-vm"
    , manifest = Card.ManifestReady manifestJson
    , transportLabel = Nothing
    , scanTimer = Nothing
    , transportWarning = Nothing
    , statusOverride = Nothing
    , results = Nothing
    , history = { rows = [], loading = False, loaded = True }
    , activeResultId = Nothing
    , pendingResultId = Nothing
    , erroredResultId = Nothing
    , expiredResultId = Nothing
    , requestBusy = False
    , allowedIframeOrigins = []
    , embedUrl = ""
    , embedState = Card.EmbedIdle
    , demoIframeUrl = Nothing
    }


render : String -> Bool -> Query.Single Card.Msg
render manifestJson detailExpanded =
    Card.view palette Time.utc (configFor manifestJson) [] (modelWith detailExpanded)
        |> Element.layout []
        |> Query.fromHtml


{-| A manifest naming a component type this catalog does not have: version skew.
-}
offCatalogManifest : String
offCatalogManifest =
    """{ "root": "x", "elements": { "x": { "type": "ScriptInjector", "props": {}, "children": [] } } }"""


{-| A manifest no newer renderer would accept either: `Text` with no `value`.
-}
malformedManifest : String
malformedManifest =
    """{ "root": "t", "elements": { "t": { "type": "Text", "props": {}, "children": [] } } }"""


{-| A fragment of the raw decoder diagnostic, from the fail-closed arm's own wording. Its presence
on screen means the decoder dump leaked; a researcher must never meet it uninvited.
-}
rawDiagnosticFragment : String
rawDiagnosticFragment =
    "off-catalog"


suite : Test
suite =
    describe "the card's refused-manifest presentation"
        [ test "version skew says so, and names the VM that published the manifest" <|
            \_ ->
                render offCatalogManifest False
                    |> Query.has
                        [ Selector.text "This extension needs a newer Exosphere"
                        , Selector.text "The \"cloudshield-vm\" VM published interface features this version of Exosphere doesn't support yet. Updating Exosphere may fix this."
                        ]
        , test "a malformed manifest says the refusal is deliberate and points at the publisher" <|
            \_ ->
                render malformedManifest False
                    |> Query.has
                        [ Selector.text "This extension published an interface Exosphere can't render"
                        , Selector.text "Refusing to display it is a safety feature. The extension may have a bug; consider notifying its publisher."
                        ]
        , test "the two kinds do not show each other's message" <|
            \_ ->
                render malformedManifest False
                    |> Query.hasNot [ Selector.text "This extension needs a newer Exosphere" ]
        , test "the raw decoder diagnostic is NOT on screen while the details stay collapsed" <|
            \_ ->
                -- The whole point. Collapsed is the default, so this is what a researcher meets.
                render offCatalogManifest False
                    |> Query.hasNot [ Selector.text rawDiagnosticFragment ]
        , test "the details toggle is offered, collapsed" <|
            \_ ->
                render offCatalogManifest False
                    |> Query.has [ Selector.text "▸ Technical details" ]
        , test "expanding the details reveals the raw decoder diagnostic" <|
            \_ ->
                -- Confined to the details region rather than removed: whoever is debugging the
                -- publisher still gets the exact decoder text, one click in.
                render offCatalogManifest True
                    |> Query.has
                        [ Selector.text rawDiagnosticFragment
                        , Selector.text "▾ Technical details"
                        ]
        , test "a manifest that DOES decode shows no error presentation at all" <|
            \_ ->
                -- Guards the fixtures above: the error path is reached because the manifest is
                -- rejected, not because `view` always draws this.
                let
                    validManifest =
                        """{ "root": "t", "elements": { "t": { "type": "Text", "props": { "value": "hello" }, "children": [] } } }"""
                in
                render validManifest False
                    |> Query.hasNot [ Selector.text "Technical details" ]
        ]
