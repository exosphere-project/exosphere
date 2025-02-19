module Tests.Helpers.Multipart exposing (multipartTests)

import Expect
import Helpers.Multipart as Multipart
import Test exposing (describe, test)


multipartTests : Test.Test
multipartTests =
    describe "Multipart transfer encoding"
        [ test "empty multiparts are an empty string" <|
            \_ ->
                Expect.equal
                    (Multipart.mixed (Multipart.boundary "====exosphere")
                        |> Multipart.string
                    )
                    ""
        , test "When the multipart only contains a single part, we don't need the container" <|
            \_ ->
                Expect.equal
                    (Multipart.mixed (Multipart.boundary "====exosphere")
                        |> Multipart.addStringPart "text/plain" [] "testing"
                        |> Multipart.string
                    )
                    "Content-Transfer-Encoding: 7bit\u{000D}\nContent-Type: text/plain\u{000D}\n\u{000D}\ntesting"
        , test "Simple multiparts should get encoded properly" <|
            \_ ->
                Expect.equal
                    (Multipart.mixed (Multipart.boundary "====exosphere")
                        |> Multipart.addStringPart "text/plain" [] "part 1"
                        |> Multipart.addStringPart "text/plain" [] "part 2\nhas\nmultiple\nlines\n"
                        |> Multipart.string
                    )
                    (String.concat
                        [ "MIME-Version: 1.0\u{000D}\n"
                        , "Content-Transfer-Encoding: 7bit\u{000D}\n"
                        , "Content-Type: multipart/mixed; boundary=\"=============================exosphere==\"\u{000D}\n"
                        , "Number-Attachments: 2\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=============================exosphere==\u{000D}\n"
                        , "Content-Transfer-Encoding: 7bit\u{000D}\n"
                        , "Content-Type: text/plain\u{000D}\n"
                        , "\u{000D}\n"
                        , "part 1"
                        , "\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=============================exosphere==\u{000D}\n"
                        , "Content-Transfer-Encoding: 7bit\u{000D}\n"
                        , "Content-Type: text/plain\u{000D}\n"
                        , "\u{000D}\n"
                        , "part 2\n"
                        , "has\n"
                        , "multiple\n"
                        , "lines\n"
                        , "\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=============================exosphere==--"
                        ]
                    )
        , test "Multiparts with 8 bit ascii get marked as such" <|
            \_ ->
                Expect.equal
                    (Multipart.mixed (Multipart.boundary "====exosphere")
                        |> Multipart.addStringPart "text/plain" [] "Ø"
                        |> Multipart.string
                    )
                    "Content-Transfer-Encoding: 8bit\u{000D}\nContent-Type: text/plain\u{000D}\n\u{000D}\nØ"
        , test "Multiparts with unicode content get base64 encoded" <|
            \_ ->
                Expect.equal
                    (Multipart.mixed (Multipart.boundary "====exosphere")
                        |> Multipart.addStringPart "text/plain" [] "🤣"
                        |> Multipart.string
                    )
                    "Content-Transfer-Encoding: base64\u{000D}\nContent-Type: text/plain\u{000D}\n\u{000D}\n8J+kow=="
        , test "Multiparts containing only an empty multipart are still empty" <|
            \_ ->
                Expect.equal
                    (Multipart.mixed (Multipart.boundary "====1")
                        |> Multipart.addMultipart (Multipart.mixed (Multipart.boundary "====2"))
                        |> Multipart.string
                    )
                    ""
        , test "Nested Multiparts containing a single final part don't need a container" <|
            \_ ->
                Expect.equal
                    (Multipart.mixed (Multipart.boundary "====1")
                        |> Multipart.addMultipart
                            (Multipart.mixed (Multipart.boundary "====2")
                                |> Multipart.addStringPart "text/plain" [] "test"
                            )
                        |> Multipart.string
                    )
                    "Content-Transfer-Encoding: 7bit\u{000D}\nContent-Type: text/plain\u{000D}\n\u{000D}\ntest"
        , test "Nested single-entry multiparts don't need their container" <|
            \_ ->
                Expect.equal
                    (Multipart.mixed (Multipart.boundary "====1")
                        |> Multipart.addStringPart "text/plain" [] "outer"
                        |> Multipart.addMultipart
                            (Multipart.mixed (Multipart.boundary "====2")
                                |> Multipart.addStringPart "text/plain" [] "test"
                            )
                        |> Multipart.string
                    )
                    (String.concat
                        [ "MIME-Version: 1.0\u{000D}\n"
                        , "Content-Transfer-Encoding: 7bit\u{000D}\n"
                        , "Content-Type: multipart/mixed; boundary=\"=====================================1==\"\u{000D}\n"
                        , "Number-Attachments: 2\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=====================================1==\u{000D}\n"
                        , "Content-Transfer-Encoding: 7bit\u{000D}\n"
                        , "Content-Type: text/plain\u{000D}\n"
                        , "\u{000D}\n"
                        , "outer\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=====================================1==\u{000D}\n"
                        , "Content-Transfer-Encoding: 7bit\u{000D}\n"
                        , "Content-Type: text/plain\u{000D}\n"
                        , "\u{000D}\n"
                        , "test\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=====================================1==--"
                        ]
                    )
        , test "Nested multiparts work properly" <|
            \_ ->
                Expect.equal
                    (Multipart.mixed (Multipart.boundary "====1")
                        |> Multipart.addStringPart "text/plain" [] "outer"
                        |> Multipart.addMultipart
                            (Multipart.alternative (Multipart.boundary "====2")
                                |> Multipart.addStringPart "text/html" [] "<h1>test</h1>"
                                |> Multipart.addStringPart "text/plain" [] "# test\n"
                            )
                        |> Multipart.string
                    )
                    (String.concat
                        [ "MIME-Version: 1.0\u{000D}\n"
                        , "Content-Transfer-Encoding: 7bit\u{000D}\n"
                        , "Content-Type: multipart/mixed; boundary=\"=====================================1==\"\u{000D}\n"
                        , "Number-Attachments: 2\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=====================================1==\u{000D}\n"
                        , "Content-Transfer-Encoding: 7bit\u{000D}\n"
                        , "Content-Type: text/plain\u{000D}\n"
                        , "\u{000D}\n"
                        , "outer\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=====================================1==\u{000D}\n"
                        , "MIME-Version: 1.0\u{000D}\n"
                        , "Content-Transfer-Encoding: 7bit\u{000D}\n"
                        , "Content-Type: multipart/alternative; boundary=\"=====================================2==\"\u{000D}\n"
                        , "Number-Attachments: 2\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=====================================2==\u{000D}\n"
                        , "Content-Transfer-Encoding: 7bit\u{000D}\n"
                        , "Content-Type: text/html\u{000D}\n"
                        , "\u{000D}\n"
                        , "<h1>test</h1>\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=====================================2==\u{000D}\n"
                        , "Content-Transfer-Encoding: 7bit\u{000D}\n"
                        , "Content-Type: text/plain\u{000D}\n"
                        , "\u{000D}\n"
                        , "# test\n"
                        , "\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=====================================2==--\u{000D}\n"
                        , "\u{000D}\n"
                        , "--=====================================1==--"
                        ]
                    )
        ]
