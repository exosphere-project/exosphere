port module Ports exposing (instantiateClipboardJs, openInBrowser, openNewWindow, setStorage)

import Json.Encode as Encode


port openInBrowser : String -> Cmd msg


port openNewWindow : String -> Cmd msg


port setStorage : Encode.Value -> Cmd msg


port instantiateClipboardJs : () -> Cmd msg
