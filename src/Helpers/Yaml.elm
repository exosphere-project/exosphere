module Helpers.Yaml exposing (safeEncodeString)

import String.Extra
import Yaml.Encode as YE


{-| Helper function to safely encode strings in Yaml documents

    import Yaml.Encode as YE

    YE.toString 2 (safeEncodeString "0700")
    --> "\"0700\""

    YE.toString 2 (safeEncodeString "multi\nline")
    --> "\"multi\\nline\""

    YE.toString 2 (safeEncodeString "has \"quotes\"")
    --> "\"has \\\"quotes\\\"\""

-}
safeEncodeString : String -> YE.Encoder
safeEncodeString =
    encodeNewlines
        >> encodeQuotes
        >> String.Extra.quote
        >> YE.string


encodeNewlines : String -> String
encodeNewlines =
    String.replace "\n" "\\n"


encodeQuotes : String -> String
encodeQuotes s =
    if String.contains "\"" s then
        String.replace "\"" "\\\"" s

    else
        s
