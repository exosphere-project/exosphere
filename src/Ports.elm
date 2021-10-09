port module Ports exposing
    ( changeThemePreference
    , instantiateClipboardJs
    , logout
    , openNewWindow
    , pushUrlAndTitleToMatomo
    , setFavicon
    , setStorage
    )

import Json.Encode as Encode


port changeThemePreference : (Encode.Value -> msg) -> Sub msg


port openNewWindow : String -> Cmd msg


port setStorage : Encode.Value -> Cmd msg


port instantiateClipboardJs : () -> Cmd msg


port logout : () -> Cmd msg


port setFavicon : String -> Cmd msg


port pushUrlAndTitleToMatomo : { newUrl : String, pageTitle : String } -> Cmd msg
