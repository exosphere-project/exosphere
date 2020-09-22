module Style.Widgets.NumericPicker.Types exposing (NumericPickerParams, NumericTextInput(..))


type NumericTextInput
    = ValidNumericTextInput Int
    | InvalidNumericTextInput String


type alias NumericPickerParams =
    { labelText : String
    , minVal : Int
    , maxVal : Int
    , defaultVal : Maybe Int
    }
