module View.Keypairs exposing (keypairs)

import Element
import Types.Types
    exposing
        ( DeleteKeypairConfirmation
        , Msg
        , Project
        )
import View.Types


keypairs : View.Types.Context -> Project -> List DeleteKeypairConfirmation -> Element.Element Msg
keypairs context project deleteConfirmations =
    Element.text "keypairs go here"
