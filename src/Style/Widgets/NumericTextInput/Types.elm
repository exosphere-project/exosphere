module Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..), NumericTextInputParams)


type NumericTextInput
    = ValidNumericTextInput Int
    | InvalidNumericTextInput String
    | BlankNumericTextInput


type alias NumericTextInputParams =
    { labelText : String
    , minVal : Maybe Int
    , maxVal : Maybe Int
    , defaultVal : Maybe Int
    , required : Bool
    }
