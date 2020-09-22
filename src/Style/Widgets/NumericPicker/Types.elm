module Style.Widgets.NumericPicker.Types exposing (NumericPickerParams, NumericTextInput(..))


type NumericTextInput
    = ValidNumericTextInput Int
    | InvalidNumericTextInput String


type alias NumericPickerParams =
    { labelText : String
    , minVal : Maybe Int
    , maxVal : Maybe Int
    , defaultVal : Maybe Int
    }
