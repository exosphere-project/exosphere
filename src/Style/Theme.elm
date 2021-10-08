module Style.Theme exposing (decodeThemePreference, fromString)

import Json.Decode as JD
import Json.Encode as JE
import Style.Types as ST


decodeThemePreference : JE.Value -> Maybe ST.Theme
decodeThemePreference value =
    value
        |> JD.decodeValue (JD.nullable JD.string)
        |> Result.withDefault Nothing
        |> Maybe.map fromString
        |> Maybe.withDefault Nothing


fromString : String -> Maybe ST.Theme
fromString theme =
    case theme of
        "dark" ->
            Just ST.Dark

        "light" ->
            Just ST.Light

        _ ->
            Nothing
