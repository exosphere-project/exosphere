module Utils.Base64 exposing (encode)

import Base64
import Word.Bytes as Bytes


encode : String -> String
encode =
    Bytes.fromUTF8
        >> Base64.encode
