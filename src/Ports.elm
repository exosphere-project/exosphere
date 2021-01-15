port module Ports exposing
    ( instantiateClipboardJs
    , openInBrowser
    , openNewWindow
    , pushUrlAndTitleToMatomo
    , setFavicon
    , setStorage
    )

import Json.Encode as Encode


port openInBrowser : String -> Cmd msg


port openNewWindow : String -> Cmd msg


port setStorage : Encode.Value -> Cmd msg


port instantiateClipboardJs : () -> Cmd msg


port setFavicon : String -> Cmd msg


port pushUrlAndTitleToMatomo : String -> Cmd msg
