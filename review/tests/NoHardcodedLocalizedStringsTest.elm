module NoHardcodedLocalizedStringsTest exposing (all)

import NoHardcodedLocalizedStrings
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)


rule : Rule
rule =
    NoHardcodedLocalizedStrings.rule NoHardcodedLocalizedStrings.exosphereLocalizedStrings


all : Test
all =
    describe "NoHardcodedLocalizedStrings"
        [ test "should not report an error when a string is tagged" <|
            \() ->
                """module A exposing (..)
-- @nonlocalized
a = "cloud"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when a string is tagged inline" <|
            \() ->
                """module A exposing (..)
a = "test" ++ {- @nonlocalized -} "cloud"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when using a localized string" <|
            \() ->
                """module A exposing (..)
s = "cloud"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Replace `cloud` with  context.localization.openstackWithOwnKeystone"
                            , details =
                                [ "cloud is a localized string, and should not be hardcoded"
                                , "If this is intentional, tag the string with a {- @nonlocalized -} comment on the preceding line"
                                ]
                            , under = "\"cloud\""
                            }
                        ]
        , test "should report an error when using title cased localized strings" <|
            \() ->
                """module A exposing (..)
s = "Cloud"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Replace `Cloud` with (Helpers.String.toTitleCase context.localization.openstackWithOwnKeystone)"
                            , details =
                                [ "Cloud is a localized string, and should not be hardcoded"
                                , "If this is intentional, tag the string with a {- @nonlocalized -} comment on the preceding line"
                                ]
                            , under = "\"Cloud\""
                            }
                        ]
        , test "should report an error when using upper cased localized strings" <|
            \() ->
                """module A exposing (..)
s = "CLOUD"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Replace `CLOUD` with (String.toUpper context.localization.openstackWithOwnKeystone)"
                            , details =
                                [ "CLOUD is a localized string, and should not be hardcoded"
                                , "If this is intentional, tag the string with a {- @nonlocalized -} comment on the preceding line"
                                ]
                            , under = "\"CLOUD\""
                            }
                        ]
        , test "should report an error when using pluralized localized strings" <|
            \() ->
                """module A exposing (..)
s = "Clouds"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Replace `Clouds` with (context.localization.openstackWithOwnKeystone |> Helpers.String.pluralized |> Helpers.String.toTitleCase)"
                            , details =
                                [ "Clouds is a localized string, and should not be hardcoded"
                                , "If this is intentional, tag the string with a {- @nonlocalized -} comment on the preceding line"
                                ]
                            , under = "\"Clouds\""
                            }
                        ]
        , test "should report an error when a string is unecessarily tagged" <|
            \() ->
                """module A exposing (..)
s = {- @nonlocalized -} "This isn't a problem"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unused @nonlocalized tag"
                            , details =
                                [ "@nonlocalized tags must only be used to mark strings that have intentional usages of localizable strings"
                                ]
                            , under = "{- @nonlocalized -}"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
s =  "This isn't a problem"
"""
                        ]
        ]
