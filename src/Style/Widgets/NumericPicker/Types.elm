module Style.Widgets.NumericPicker.Types exposing (NumericTextInput)


type NumericTextInput
    = ValidNumericTextInput Int
    | InvalidNumericTextInput String
