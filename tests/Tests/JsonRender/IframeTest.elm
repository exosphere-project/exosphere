module Tests.JsonRender.IframeTest exposing (suite)

{-| Focused coverage for the origin-pinned `Iframe` element's empty-vs-disallowed `src` handling.

The rule: an **empty / unresolved** `src` renders **nothing** (the host owns the empty / loading /
error affordance around the frame), while a **non-empty but disallowed** src renders the benign
security placeholder. This is the fix for the results region wrongly showing "Embedded content is
unavailable" during loading / idle.

-}

import Dict
import Expect
import Json.Encode as Encode
import JsonRender
import JsonRender.Render as Render
import JsonRender.Spec exposing (Spec)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


allowedOrigins : List String
allowedOrigins =
    [ "https://1-2-3-4.sslip.io" ]


iframeManifest : String
iframeManifest =
    """
    { "root": "frame"
    , "elements":
        { "frame":
            { "type": "Iframe"
            , "props": { "src": { "$state": "/embedUrl" }, "title": "CloudShield scan results" }
            }
        }
    }
    """


specOf : String -> Spec
specOf raw =
    case JsonRender.decodeString raw of
        Ok spec ->
            spec

        Err _ ->
            { root = "missing", elements = Dict.empty, state = Encode.null }


render : String -> Query.Single Render.Msg
render embedUrl =
    Render.view allowedOrigins (specOf iframeManifest) (Encode.object [ ( "embedUrl", Encode.string embedUrl ) ]) Render.init
        |> Query.fromHtml


suite : Test
suite =
    describe "JsonRender.Render Iframe empty-vs-disallowed src"
        [ test "an allowlisted https src renders the <iframe>" <|
            \_ ->
                render "https://1-2-3-4.sslip.io/app"
                    |> Query.findAll [ Selector.tag "iframe" ]
                    |> Query.count (Expect.equal 1)
        , test "an empty (unresolved) src renders no iframe" <|
            \_ ->
                render ""
                    |> Query.findAll [ Selector.tag "iframe" ]
                    |> Query.count (Expect.equal 0)
        , test "an empty (unresolved) src renders NOTHING — no security placeholder (host owns the empty affordance)" <|
            \_ ->
                render ""
                    |> Query.hasNot [ Selector.text "Embedded content is unavailable." ]
        , test "a non-empty but disallowed src DOES render the security placeholder" <|
            \_ ->
                render "https://evil.example.com/app"
                    |> Query.has [ Selector.text "Embedded content is unavailable." ]
        , test "a disallowed src renders no iframe (only the placeholder)" <|
            \_ ->
                render "https://evil.example.com/app"
                    |> Query.findAll [ Selector.tag "iframe" ]
                    |> Query.count (Expect.equal 0)
        ]
